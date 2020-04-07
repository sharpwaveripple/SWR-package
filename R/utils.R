count_long_resp <- function(df, id_col, var) {
  id_list <- unique(df[[id_col]])
  for (id in id_list) {
    idx <- which(df[id_col]==id)
    n_val <- sum(!is.na(df[idx, var]))
    df[idx, paste(var, 'long_count', sep='_')] <- n_val
  }
  return(df)
}
