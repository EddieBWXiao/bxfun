#' To z-score or mean-center, but only output as numeric
#'
#' This function scales and/or centers a numeric matrix or vector using the `scale()` function and then converts the result to a numeric vector.
#' It includes additional error handling for common mistakes, such as using British spelling for the `center` argument.
#'
#' @param x A numeric matrix or vector to be scaled and converted; can be a data frame column
#' @param center Logical. Indicates whether the values should be centered by subtracting their mean. Defaults to \code{TRUE}.
#' @param scale Logical. Indicates whether the values should be scaled by dividing by their standard deviation. Defaults to \code{TRUE}.
#' @param ... Additional arguments passed to \code{scale()}.
#'
#' @details
#' This function performs scaling and centering on the input data and returns it as a numeric vector. Note the following downsides:
#' \itemize{
#'   \item \strong{Loss of original scaling attributes}: \code{as.numeric()} removes metadata about the scaling, such as the mean and standard deviation.
#'   \item \strong{Breaks reproducibility}: Without the original scaling parameters, it is not possible to reverse the scaling.
#'   \item \strong{Potential precision loss}: Converting from a matrix to numeric might introduce minor floating-point precision changes.
#' }
#'
#'
#' @return A numeric vector containing the scaled and centered values.
#'
#' @examples
#' # Example usage:
#' data <- data.frame(a=c(1,2,3))
#' data$a_z <- scale2num(data$a)
#' data$a_c <- scale2num(data$a, scale = FALSE) #to mean-center
#'
#' @seealso \code{\link[base]{scale}}, \code{\link[base]{as.numeric}}
#'
#' @export
scale2num <- function(x,center=T,scale=T,...){
  # downsides of function (Claude-generated):
    # Loss of original scaling attributes: as.numeric() removes metadata about the scaling, such as the mean and standard deviation used for centering and scaling.
    # Breaks reproducibility: If you need to reverse the scaling later, you've lost the original scaling parameters.
    # Potential precision loss: Converting from matrix to numeric might introduce minor floating-point precision changes.
  # Check if user accidentally used British spelling 'centre'

  if ("centre" %in% names(as.list(match.call()))) {
    stop("argument 'centre' not found. Did you mean 'center'?")
  }

  return(as.numeric(scale(x,center=center,scale=scale)))
}
