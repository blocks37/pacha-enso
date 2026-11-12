raiz <- testthat::test_path("..", "..")
invisible(lapply(list.files(file.path(raiz, "R"), full.names = TRUE), source, encoding = "UTF-8"))
cfg <- cargar_config(file.path(raiz, "config.yml"))
