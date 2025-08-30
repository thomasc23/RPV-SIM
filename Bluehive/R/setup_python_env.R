suppressPackageStartupMessages(library(reticulate))
py <- Sys.getenv("RETICULATE_PYTHON", unset = "")
if (nzchar(py)) use_python(py, required = TRUE)
py_config()

