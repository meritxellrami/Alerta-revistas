# Alerta de revistas — Ciencia Política

Sistema que cada **lunes** envía un correo con:

1. **Novedades** en tus 24 revistas (últimos días).
2. **Convocatorias** (calls / special issues / symposiums) — *pendiente de automatizar* (ver nota abajo).
3. **Papers nuevos** sobre tus temas (*affective polarization* y amenaza externa × polarización) en las **top-80 revistas por ranking SJR**.

Datos vía **OpenAlex** (gratis, sin clave). Envío vía **Gmail**. Ejecución automática en **GitHub Actions** (no necesitas el ordenador encendido). Lenguaje: **R**.

---

## Puesta en marcha (una sola vez)

### 1. Contraseña de aplicación de Gmail
1. Activa la verificación en 2 pasos en tu cuenta Google.
2. Ve a <https://myaccount.google.com/apppasswords> y crea una contraseña para "Correo".
3. Guarda los 16 caracteres (sin espacios).

### 2. Lista SJR de Scimago (filtro de la sección 3)
Scimago bloquea la descarga automática, así que se baja a mano (1 clic, se actualiza 1 vez al año):
1. Abre <https://www.scimagojr.com/journalrank.php?area=3300> (Political Science & International Relations).
2. Botón **"Download data"** (arriba a la derecha) → te bajas un CSV.
3. Renómbralo a `sjr_political_science.csv` y déjalo en `config/`.

El CSV usa `;` como separador y trae la columna `Rank`; el sistema se queda con las **primeras 80** revistas (cambia el número con la variable `TOP_SJR`).

### 3. Subir a GitHub y configurar secretos
1. Crea un repositorio en GitHub y sube esta carpeta.
2. En el repo: **Settings → Secrets and variables → Actions → New repository secret**. Crea:
   - `SMTP_USERNAME` = la cuenta de Google que generó la contraseña de aplicación
   - `SMTP_PASSWORD` = la contraseña de aplicación del paso 1
   - `MAIL_TO` = `meritxell.rami@upf.edu` (o donde quieras recibirlo)
3. En **Settings → Actions → General → Workflow permissions**, marca **Read and write permissions** (para que el bot pueda guardar el estado).
4. Ve a la pestaña **Actions**, elige *"Alerta revistas semanal"* y pulsa **Run workflow** para probarlo ya.

A partir de ahí corre solo cada lunes a las 06:00 UTC (cambia el `cron` en `.github/workflows/weekly.yml`).

---

## Probar en local (sin enviar correo)

```bash
DRY_RUN=1 DAYS_BACK=14 Rscript R/run.R
```

Imprime el correo por consola y no toca el estado. Requiere los paquetes R `jsonlite`, `yaml`, `glue` (para envío real además `blastula`):

```r
install.packages(c("jsonlite","yaml","glue","blastula"))
```

---

## Personalizar

- **Revistas:** edita `config/revistas.yaml` (añade/quita; el `issn` lo puedes sacar de Crossref).
- **Temas/keywords:** edita `config/temas.yaml`.
- **Frecuencia/ventana/nº de revistas:** `cron` en el workflow, y las variables `DAYS_BACK` y `TOP_SJR`.

## Nota sobre la sección 2 (convocatorias)
Aparcada de momento: las páginas web de "Call for Papers" de las editoriales comerciales
(T&F, Sage, Wiley, Oxford, Elsevier…) bloquean el acceso automatizado (403) y no hay API.
La vía robusta sería leer por IMAP los avisos que las revistas envían por correo. Las funciones
para retomarlo siguen en `R/utils.R` (`http_get`, `extract_call_items`, `diff_calls`).

## Cómo evita duplicados
`state/seen.csv` guarda los IDs ya enviados; el workflow lo vuelve a subir tras cada ejecución, así nunca repite un artículo.

## Archivos
```
config/revistas.yaml              tus revistas + ISSN
config/temas.yaml                 keywords de la sección temática
config/sjr_political_science.csv  (lo añades tú) ranking SJR de Scimago
R/utils.R                         acceso a OpenAlex + estado
R/run.R                           orquesta las 3 tareas y envía el correo
.github/workflows/weekly.yml      ejecución semanal en la nube
state/seen.csv                    memoria de lo ya enviado
```
