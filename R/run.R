# Alerta semanal de revistas de Ciencia Política.
# Genera y envía un correo con: (1) novedades por revista, (2) special issues /
# symposiums / calls, (3) papers nuevos sobre los temas de interés en revistas Q1.
#
# Variables de entorno:
#   SMTP_USERNAME, SMTP_PASSWORD  -> Gmail + contraseña de aplicación (envío)
#   MAIL_TO                       -> destinatario (por defecto = SMTP_USERNAME)
#   DAYS_BACK                     -> ventana de días hacia atrás (def. 8)
#   DRY_RUN=1                     -> no envía: imprime el correo por consola y no toca el estado

suppressWarnings(try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE))
suppressWarnings(suppressMessages({
  library(yaml); library(glue)
}))

root <- tryCatch(dirname(dirname(normalizePath(sys.frame(1)$ofile))), error = function(e) getwd())
if (!file.exists(file.path(root, "R", "utils.R"))) root <- getwd()
source(file.path(root, "R", "utils.R"))

DAYS_BACK   <- as.integer(Sys.getenv("DAYS_BACK", "8"))
TOP_SJR     <- as.integer(Sys.getenv("TOP_SJR", "80"))
from_date   <- as.character(Sys.Date() - DAYS_BACK)
DRY_RUN     <- Sys.getenv("DRY_RUN") == "1"
IGNORE_SEEN <- Sys.getenv("IGNORE_SEEN") == "1"  # reenvía todo lo de la ventana sin tocar la memoria

revistas <- read_yaml_utf8(file.path(root, "config", "revistas.yaml"))$journals
temas    <- read_yaml_utf8(file.path(root, "config", "temas.yaml"))$groups
wlset    <- load_whitelist_issns(file.path(root, "config", "sjr_political_science.csv"), top_n = TOP_SJR)
seen_path <- file.path(root, "state", "seen.csv")
seen      <- if (IGNORE_SEEN) character(0) else load_seen(seen_path)

fmt_item <- function(w) {
  glue("- **[{w$title}]({w$url})** — {ifelse(nchar(w$authors)>0, w$authors, 'autores n/d')} ",
       "— *{w$journal %||% 'revista n/d'}* ({w$date %||% 's/f'})")
}

new_ids <- list()  # acumula ids nuevos por sección para guardarlos al final

# Títulos que no son artículos de investigación (se excluyen de la sección 1).
SKIP_TITLE <- c("front matter", "back matter", "book review", "correction to",
                "corrigendum", "erratum", "issue information", "editorial board")

# ---- Sección 1: novedades por revista (agrupadas por revista) ----------------
# Cada revista con novedades = un bloque: encabezado en negrita + viñetas debajo.
message("[1/2] Novedades por revista…")
sec1_blocks <- character(0)
for (j in revistas) {
  works <- lapply(oa_works_by_issn(j$issn, from_date), extract_work)
  items <- character(0)
  for (w in works) {
    if (is.na(w$id) || w$id %in% seen) next
    tl <- tolower(w$title)
    if (any(vapply(SKIP_TITLE, function(k) grepl(k, tl, fixed = TRUE), logical(1)))) next
    aut   <- if (nchar(w$authors) > 0) w$authors else "autores n/d"
    items <- c(items, paste0("- <span style=\"font-size:90%\">[", w$title, "](", w$url,
                             ") — ", aut, "</span>"))
    new_ids$articulos <- c(new_ids$articulos, w$id)
  }
  if (length(items))
    sec1_blocks <- c(sec1_blocks, paste0("**", j$name, "**\n\n", paste(items, collapse = "\n")))
  Sys.sleep(0.15)
}
sec1_md <- if (length(sec1_blocks)) paste(sec1_blocks, collapse = "\n\n") else "_Sin novedades esta semana._"

# ---- Sección 2: temas en las top-80 revistas SJR ----------------------------
message(glue("[2/2] Temas en las top-{TOP_SJR} revistas SJR…"))
sec3_blocks <- character(0)
seen_topic <- character(0)
for (g in temas) {
  hits <- list()
  for (phrase in g$search_any) {
    for (w in lapply(oa_works_by_topic(phrase, from_date), extract_work)) {
      if (is.na(w$id) || w$id %in% c(seen, seen_topic)) next
      if (!isTRUE(work_in_whitelist(w, wlset))) next
      if (length(g$must_also) > 0) {
        hay <- tolower(paste(w$title, w$abstract))
        if (!any(sapply(g$must_also, function(k) grepl(tolower(k), hay, fixed = TRUE)))) next
      }
      hits[[w$id]] <- w
      seen_topic <- c(seen_topic, w$id)
    }
    Sys.sleep(0.15)
  }
  if (length(hits) > 0) {
    lines <- vapply(hits, fmt_item, character(1))
    sec3_blocks <- c(sec3_blocks, paste0("**", g$label, "**\n\n", paste(lines, collapse = "\n")))
    new_ids$temas <- c(new_ids$temas, names(hits))
  }
}
sec3_md <- if (length(sec3_blocks)) paste(sec3_blocks, collapse = "\n\n") else "_Sin novedades esta semana._"

# ---- Montaje del correo ------------------------------------------------------
wl_note <- if (is.null(wlset))
  "> ⚠️ Falta `config/sjr_political_science.csv` (export de Scimago). Sin él no puedo aplicar el filtro top-80; ver README.\n\n" else ""

md <- paste0(
  "## 📚 Alerta de revistas — semana del ", Sys.Date(), "\n\n",
  "Ventana: últimos ", DAYS_BACK, " días. Fuente: OpenAlex. Filtro temático: top-", TOP_SJR, " SJR.\n\n",
  "### 1) Novedades en tus revistas\n\n",
  sec1_md, "\n\n",
  "### 2) Papers nuevos sobre tus temas (top-", TOP_SJR, " SJR)\n\n",
  wl_note,
  sec3_md, "\n"
)

total_new <- length(unlist(new_ids))
message(glue("Resultados: {length(unlist(new_ids$articulos))} arts. revista | ",
             "{length(unlist(new_ids$temas))} temas | {total_new} novedades en total."))

if (DRY_RUN) {
  cat("\n========== DRY RUN (no se envía, no se guarda estado) ==========\n\n")
  cat(md, "\n")
  quit(save = "no")
}

# Si no hay novedades, no se envía nada. Esto permite programar varios horarios
# de respaldo el mismo día: el primero que encuentre algo lo envía y los siguientes,
# al no quedar nada nuevo, no mandan un correo vacío.
if (total_new == 0 && !IGNORE_SEEN) {
  message("Sin novedades: no se envía correo (los horarios de respaldo evitan duplicados).")
  quit(save = "no")
}

# ---- Envío por Gmail ---------------------------------------------------------
suppressWarnings(suppressMessages(library(blastula)))
user <- Sys.getenv("SMTP_USERNAME")
to   <- Sys.getenv("MAIL_TO", user)
if (user == "") stop("Falta SMTP_USERNAME.")

email <- compose_email(body = md(md))
smtp_send(
  email,
  from = user, to = to,
  subject = glue("📚 Alerta revistas — {Sys.Date()} ({total_new} novedades)"),
  credentials = creds_envvar(user = user, pass_envvar = "SMTP_PASSWORD", provider = "gmail")
)
message("Correo enviado a ", to)

# ---- Guardar estado ----------------------------------------------------------
if (IGNORE_SEEN) {
  message("IGNORE_SEEN activo: no se modifica la memoria.")
} else {
  append_seen(seen_path, unique(new_ids$articulos), "articulos")
  append_seen(seen_path, unique(new_ids$temas),     "temas")
  message("Estado actualizado.")
}
