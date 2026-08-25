# Utilidades: acceso a OpenAlex, parsing y estado (dedup).
suppressWarnings(suppressMessages({
  library(jsonlite)
}))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

OA_MAILTO <- Sys.getenv("OPENALEX_MAILTO", "meritxell.rami@upf.edu")

enc <- function(x) utils::URLencode(x, reserved = TRUE)

# Lee YAML forzando UTF-8 (evita fallos cuando el locale del sistema es C).
read_yaml_utf8 <- function(path) {
  yaml::yaml.load(paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
}

# Lanza una consulta a OpenAlex /works con un filtro ya construido (paginada por cursor).
oa_request <- function(filter_str, max_records = 400) {
  records <- list()
  cursor <- "*"
  repeat {
    url <- paste0(
      "https://api.openalex.org/works?",
      "filter=", filter_str,
      "&per-page=200",
      "&cursor=", enc(cursor),
      "&mailto=", OA_MAILTO
    )
    resp <- tryCatch(
      jsonlite::fromJSON(url, simplifyVector = FALSE),
      error = function(e) { message("  ! OpenAlex error: ", conditionMessage(e)); NULL }
    )
    if (is.null(resp) || is.null(resp$results) || length(resp$results) == 0) break
    records <- c(records, resp$results)
    cursor <- resp$meta$next_cursor %||% NULL
    if (is.null(cursor) || length(records) >= max_records) break
    Sys.sleep(0.2)
  }
  records
}

oa_works_by_issn <- function(issn, from_date) {
  oa_request(sprintf("primary_location.source.issn:%s,from_publication_date:%s", issn, from_date))
}

oa_works_by_topic <- function(phrase, from_date) {
  # Las comillas fuerzan búsqueda de FRASE EXACTA en OpenAlex. Sin ellas la API
  # busca las palabras por separado y devuelve muchísimo ruido: por ejemplo
  # "relationship satisfaction" pasa de 15 resultados a 418, colando papers de
  # marketing sobre "user satisfaction".
  oa_request(sprintf("type:article,from_publication_date:%s,title_and_abstract.search:%s",
                     from_date, enc(sprintf('"%s"', phrase))))
}

reconstruct_abstract <- function(idx) {
  if (is.null(idx) || length(idx) == 0) return("")
  words <- names(idx)
  rep_words <- rep(words, lengths(idx))
  positions <- unlist(idx, use.names = FALSE)
  paste(rep_words[order(positions)], collapse = " ")
}

extract_work <- function(w) {
  authors <- vapply(w$authorships %||% list(), function(a) a$author$display_name %||% NA_character_,
                    character(1))
  src   <- w$primary_location$source
  issns <- if (!is.null(src$issn)) paste(unlist(src$issn), collapse = ";") else NA_character_
  doi   <- w$doi %||% NA_character_
  list(
    id       = w$id %||% NA_character_,
    url      = if (!is.na(doi)) doi else (w$id %||% NA_character_),
    title    = w$title %||% w$display_name %||% "(sin título)",
    date     = w$publication_date %||% NA_character_,
    authors  = paste(stats::na.omit(authors), collapse = ", "),
    journal  = src$display_name %||% NA_character_,
    issn     = issns,
    type     = w$type %||% NA_character_,
    abstract = reconstruct_abstract(w$abstract_inverted_index)
  )
}

normalize_issn <- function(x) {
  x <- toupper(gsub("[^0-9Xx]", "", x))
  unique(x[nchar(x) == 8])
}

# Carga los ISSN de las primeras `top_n` revistas por ranking SJR
# desde el export de Scimago (CSV con ';'). Si top_n es NULL, usa todas las Q1.
load_whitelist_issns <- function(path, top_n = 80) {
  if (!file.exists(path)) return(NULL)
  df <- tryCatch(read.csv2(path, stringsAsFactors = FALSE, check.names = FALSE),
                 error = function(e) NULL)
  if (is.null(df)) return(NULL)
  icol <- grep("^Issn$",        names(df), ignore.case = TRUE)[1]
  rcol <- grep("^Rank$",        names(df), ignore.case = TRUE)[1]
  qcol <- grep("Best Quartile", names(df), ignore.case = TRUE)[1]
  if (is.na(icol)) return(NULL)
  if (!is.null(top_n) && !is.na(rcol)) {
    rank <- suppressWarnings(as.integer(df[[rcol]]))
    sel  <- df[[icol]][!is.na(rank) & rank <= top_n]
  } else if (!is.na(qcol)) {
    sel  <- df[[icol]][df[[qcol]] == "Q1"]
  } else {
    sel  <- df[[icol]]
  }
  normalize_issn(unlist(strsplit(paste(sel, collapse = ","), "[,;]")))
}

work_in_whitelist <- function(w, wlset) {
  if (is.null(wlset)) return(NA)
  if (is.na(w$issn)) return(FALSE)
  any(normalize_issn(unlist(strsplit(w$issn, ";"))) %in% wlset)
}

# --- Convocatorias (calls / special issues / symposiums anunciados) -----------
CALL_KEYWORDS <- c("call for paper", "call-for-paper", "special issue", "special-issue",
                   "special section", "symposium", "themed", "cfp")

http_get <- function(u) {
  tryCatch({
    if (requireNamespace("curl", quietly = TRUE)) {
      h <- curl::new_handle()
      curl::handle_setheaders(h, "User-Agent" = "Mozilla/5.0 (alerta-revistas)")
      rawToChar(curl::curl_fetch_memory(u, handle = h)$content)
    } else {
      paste(readLines(u, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    }
  }, error = function(e) { message("  ! fetch error ", u, ": ", conditionMessage(e)); "" })
}

abs_url <- function(href, base) {
  if (grepl("^https?://", href)) return(href)
  m <- regmatches(base, regexec("^(https?://[^/]+)", base))[[1]]
  host <- if (length(m) >= 2) m[2] else base
  if (startsWith(href, "/")) paste0(host, href) else paste0(host, "/", href)
}

# Extrae de una página HTML los enlaces cuyo texto o URL sugiere una convocatoria.
extract_call_items <- function(html, base_url) {
  if (is.null(html) || html == "") return(character(0))
  anchors <- regmatches(html, gregexpr("<a\\b[^>]*href=[\"'][^\"']+[\"'][^>]*>.*?</a>",
                                       html, ignore.case = TRUE, perl = TRUE))[[1]]
  items <- character(0)
  for (a in anchors) {
    href <- sub(".*href=[\"']([^\"']+)[\"'].*", "\\1", a, perl = TRUE)
    text <- gsub("<[^>]+>", "", a)
    text <- trimws(gsub("\\s+", " ", text))
    hay  <- tolower(paste(text, href))
    if (nchar(text) >= 8 && any(vapply(CALL_KEYWORDS, function(k) grepl(k, hay, fixed = TRUE), logical(1)))) {
      items <- c(items, paste0(text, " || ", abs_url(href, base_url)))
    }
  }
  sort(unique(items))
}

# Compara con el snapshot anterior; devuelve solo las convocatorias nuevas.
# Si no había snapshot previo, siembra (write) y no reporta nada (evita spam inicial).
diff_calls <- function(current, snap_path, write = TRUE) {
  prev <- if (file.exists(snap_path)) readLines(snap_path, warn = FALSE, encoding = "UTF-8") else NULL
  nuevos <- if (is.null(prev)) character(0) else setdiff(current, prev)
  if (write && length(current) > 0) {
    dir.create(dirname(snap_path), showWarnings = FALSE, recursive = TRUE)
    writeLines(current, snap_path, useBytes = TRUE)
  }
  if (is.null(prev)) attr(nuevos, "seeded") <- TRUE
  nuevos
}

# --- Estado (dedup) -----------------------------------------------------------
load_seen <- function(path) {
  if (!file.exists(path)) return(character(0))
  df <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || is.null(df$id)) character(0) else as.character(df$id)
}

append_seen <- function(path, ids, section) {
  if (length(ids) == 0) return(invisible())
  new <- data.frame(id = ids, first_seen = as.character(Sys.Date()),
                    section = section, stringsAsFactors = FALSE)
  hdr <- !file.exists(path)
  utils::write.table(new, path, sep = ",", row.names = FALSE, col.names = hdr,
                     append = !hdr, qmethod = "double")
}
