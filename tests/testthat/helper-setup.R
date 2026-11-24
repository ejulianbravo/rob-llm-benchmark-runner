# Loaded before every test file. Nothing here sends anything to an API.

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in c(list.files(file.path(root, "R"), full.names = TRUE),
            list.files(file.path(root, "parse"), full.names = TRUE))) source(f)

models <- yaml::read_yaml(file.path(root, "config", "models.yaml"))$models
names(models) <- vapply(models, `[[`, "", "label")

make_pdf <- function(lines = "Test report") {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  for (l in lines) { plot.new(); text(0.5, 0.5, l) }
  grDevices::dev.off()
  path
}

mock_json <- function(body, status = 200) {
  httr2::response(status_code = status, headers = list(`Content-Type` = "application/json"),
                  body = charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE))))
}

call_args <- list(system = "SYSTEM", manuals = c(m1 = "MANUAL ONE", m2 = "MANUAL TWO"),
                  instrument_prompt = "PROMPT", target = "TARGET")
