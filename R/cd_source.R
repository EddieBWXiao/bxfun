#' Source R Scripts with path relative to current working directory
#'
#' This function changes the working directory to the specified path, sources all R scripts (or files matching a given pattern) in that directory, and then restores the original working directory.
#'
#' @param path A character string specifying the directory to change to.
#' @param pattern A regular expression string to match the filenames to be sourced. Defaults to \code{"\\.R$"} to source all R scripts.
#'
#' @details
#' This function allows users to batch source multiple R scripts from a specified directory. It temporarily changes the working directory to the given \code{path}, sources files matching the \code{pattern}, and then reverts to the original working directory.
#'
#' @return Not applicable
#'
#' @examples
#' # Example usage:
#' # Source all R scripts in a directory
#' cd.source("path/to/scripts")
#'
#' # Source files with a custom pattern (e.g., only files starting with 'test_')
#' cd.source("path/to/scripts", pattern = "^test_.*\\.R$")
#'
#' @seealso \code{\link[base]{setwd}}, \code{\link[base]{list.files}}, \code{\link[base]{source}}
#'
#' @export
#'
cd.source<-function(path,pattern="\\.R$"){
  # source all
  CurrentSourceWD<-getwd()
  setwd(path)
  files.sources = list.files(pattern = pattern)
  if (length(files.sources) == 0) {
    message("No files matching the pattern were found in the specified directory.")
  }
  sapply(files.sources, source)
  setwd(CurrentSourceWD)
  return(invisible(NULL))
}
