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

DAYS_BACK <- as.integer(Sys.getenv("DAYS_BACK", "8"))
TOP_SJR   <- as.integer(Sys.getenv("TOP_SJR", "80"))
from_date <- as.character(Sys.Date() - DAYS_BACK)
DRY_RUN   <- Sys.getenv("DRY_RUN") == "1"

revistas <- read_yaml_utf8(file.path(root, "config", "revistas.yaml"))$journals
temas    <- read_yaml_utf8(file.path(root, "config", "temas.yaml"))$groups
wlset    <- load_whitelist_issns(file.path(root, "config", "sjr_political_science.csv"), top_n = TOP_SJR)
seen_path <- file.path(root, "state", "seen.csv")
seen      <- load_seen(seen_path)

fmt_item <- function(w) {
  glue("- **[{w$title}]({w$url})** — {ifelse(nchar(w$authors)>0, w$authors, 'autores n/d')} ",
       "— *{w$journal %||% 'revista n/d'}* ({w$date %||% 's/f'})")
}

new_ids <- list()  # acumula ids nuevos por sección para guardarlos al final

# ---- Sección 1: novedades por revista ---------------------------------------
message("[1/3] Novedades por revista…")
sec1 <- character(0)
for (j in revistas) {
  works <- lapply(oa_works_by_issn(j$issn, from_date), extract_work)
  for (w in works) {
    if (is.na(w$id) || w$id %in% seen) next
    sec1 <- c(sec1, fmt_item(w))
    new_ids$articulos <- c(new_ids$articulos, w$id)
  }
  Sys.sleep(0.15)
}

# ---- Sección 2: convocatorias (desactivada por ahora) -----------------------
# La detección automática de CFP/special issues/symposiums está aparcada: las
# webs de convocatorias de las editoriales comerciales bloquean el acceso (403)
# y no existe API. Las funciones para retomarlo siguen en utils.R.

# ---- Sección 3: temas en las top-80 revistas SJR ----------------------------
message(glue("[3/3] Temas en las top-{TOP_SJR} revistas SJR…"))
sec3 <- character(0)
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
    sec3 <- c(sec3, glue("\n**{g$label}**\n"),
              vapply(hits, fmt_item, character(1)))
    new_ids$temas <- c(new_ids$temas, names(hits))
  }
}

# ---- Montaje del correo ------------------------------------------------------
wl_note <- if (is.null(wlset))
  "> ⚠️ Falta `config/sjr_political_science.csv` (export de Scimago). Sin él no puedo aplicar el filtro top-80; ver README.\n" else ""

body <- c(
  glue("## 📚 Alerta de revistas — semana del {Sys.Date()}"),
  glue("Ventana: últimos {DAYS_BACK} días. Fuente: OpenAlex. Filtro temático: top-{TOP_SJR} SJR.\n"),
  "### 1) Novedades en tus revistas",
  if (length(sec1)) sec1 else "_Sin novedades esta semana._",
  "\n### 2) Convocatorias (calls / special issues / symposiums)",
  "_Detección automática pendiente (las webs de convocatorias bloquean el acceso). De momento revísalas a mano._",
  glue("\n### 3) Papers nuevos sobre tus temas (top-{TOP_SJR} SJR)"),
  wl_note,
  if (length(sec3)) sec3 else "_Sin novedades esta semana._"
)
md <- paste(body, collapse = "\n")

total_new <- length(unlist(new_ids))
message(glue("Resultados: {length(sec1)} arts. revista | ",
             "{length(unlist(new_ids$temas))} temas | {total_new} novedades en total."))

if (DRY_RUN) {
  cat("\n========== DRY RUN (no se envía, no se guarda estado) ==========\n\n")
  cat(md, "\n")
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
append_seen(seen_path, unique(new_ids$articulos), "articulos")
append_seen(seen_path, unique(new_ids$temas),     "temas")
message("Estado actualizado.")
