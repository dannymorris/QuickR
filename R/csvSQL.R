csvSQL <- function(x) {
  paste0("'", dplyr::pull(x), "'", collapse = ",")
}
