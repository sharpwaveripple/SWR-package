count_categorical <- function(dataset, variable, precision) {
  frequencies <- table(dataset[variable])
  percentages <- round(frequencies / nrow(dataset) * 100, precision)
  counts <- matrix(paste(frequencies, " (", percentages, "%)", sep = ""))
  rownames(counts) <- paste(variable, names(frequencies), sep = "- ")
  return(counts)
}


format_categorical <- function(dataset, cat_vars, precision) {
  global_counts <- c()
  for (i in cat_vars) {
    counts <- count_categorical(dataset, i, precision)
    global_counts <- rbind(global_counts, counts)
  }
  colnames(global_counts) <- paste("n =", nrow(dataset))
  return(global_counts)
}


format_continuous <- function(dataset, cont_vars, precision) {
  means <- colMeans(dataset[cont_vars])
  means <- format(means, digits = precision, trim = T)
  std <- sapply(dataset[cont_vars], sd)
  std <- format(std, digits = precision, trim = T)
  meanstd <- matrix(paste(means, " (", std, ")", sep = ""))
  rownames(meanstd) <- cont_vars
  return(meanstd)
}


describe <- function(dataset, cat_thresh = 5, precision = 1) {
  n_unique <- sapply(dataset[names(dataset)], function(x) length(unique(x)))
  cat_cols <- as.vector(n_unique < cat_thresh)
  cat_vars <- names(dataset[cat_cols])
  cont_vars <- names(dataset[!cat_cols])
  cat_rows <- format_categorical(dataset, cat_vars, precision)
  cont_rows <- format_continuous(dataset, cont_vars, precision)
  out_list <- rbind(cat_rows, cont_rows)
  return(out_list)
}
