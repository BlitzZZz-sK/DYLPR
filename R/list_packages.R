#' List the contents of the package
#'
#' @export
list_contents <- function() {
  # List all files in the R directory of the package
  files <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
  print(files)
}


#' List the contents of the package
#'
#' This function lists all R script files in the package's R directory.
#'
#' @export
list_contents <- function() {
  # List all files in the R directory of the package
  files <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
  print(files)
}
