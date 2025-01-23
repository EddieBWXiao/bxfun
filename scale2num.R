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
