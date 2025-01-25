#' Set Working Directory to the Script's Location (requires RStudio but not RProj)
#'
#' This function sets the working directory to the directory containing the currently active R script in RStudio.
#' It mimics MATLAB's behavior of automatically using the script's directory as the working directory when running the script.
#'
#' @details
#' This function may be particularly useful for users transitioning from MATLAB to R. The working directory in MATLAB is automatically set to the script's location.
#' Unlike \code{here::here()}, this function does not depend on the script being within an R project (\code{.RProj}) structure but instead relies on RStudio's API to identify the script's directory.
#'
#' @return The new absolute working directory (path to current script), as a character string.
#'
#' @examples
#' # To set the working directory to the script's location, simply call:
#' wd.here()
#'
#' @seealso \code{\link[here]{here}}, \code{\link[base]{setwd}}
#'
#' @export
wd.here<-function(){
  #stemmed from the MATLAB habits...
  #in MATLAB, when a script is run, the directory is naturally its own directory, unless otherwise specified
  #putting wd.here() at the top of the script should achieve the same function
  #unlike here::here(), does not depend on the script being in an RProj, just that the person is using RStudio

  CurrentSourceWD<-dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(CurrentSourceWD)
  return(CurrentSourceWD)
}
