#' transform_result
#' @export transform_result
transform_result <- function(data) {
  split_x <- list()
  
  split_x <- lapply(data, function(x) {
    ntrees <- ceiling(length(x)/5)
    names(x) <- rep(c("edge","edge.length","Nnode","tip.label","Unknown"), times = ntrees)
    split_x <- split(x, ceiling(seq_along(x)/5))
    split_x <- lapply(split_x, function(x) {class(x) <- "phylo"; return(x)})
  })
  
  return(split_x)
}
