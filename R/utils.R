#' Find n max values in a vector
#'
#' \code{n_max} takes a vector and finds the n largest numbers
#'
#' @param x Numerical vector
#' @param n Number of largest elements to return
#' @export
n_max <- function(x, n=1) {
  tail(sort(x), n)
}

n_min <- function(x, n=1) {
  head(sort(x), n)
}

count_long_resp <- function(df, id_col, var) {
  id_list <- unique(df[[id_col]])
  for (id in id_list) {
    idx <- which(df[id_col]==id)
    n_val <- sum(!is.na(df[idx, var]))
    df[idx, paste(var, 'long_count', sep='_')] <- n_val
  }
  return(df)
}
