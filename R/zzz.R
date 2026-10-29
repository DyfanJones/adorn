#' @importFrom S7 S7_on_load S7_on_unload S7_on_build

.onLoad <- function(...) {
  S7_on_load()
}

.onUnload <- function(...) {
  S7_on_unload()
}

S7_on_build()
