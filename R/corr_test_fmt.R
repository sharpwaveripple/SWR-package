format_correlation <- function(corr_mat, r_precision=2,
                               p_precision=3, p_equals=F, p_whitespace=F) {
  format_str <- paste("%.", r_precision, "f", sep = "")
  # var_names <- list(dict_keys(colnames(corr_mat$r)))
  var_names <- list(colnames(corr_mat$r))
  r <- sprintf(eval(format_str), corr_mat$r)
  p <- SWR::format_pvals(corr_mat$p, precision = p_precision,
                         p_equals = p_equals, whitespace = p_whitespace)
  tab <- matrix(paste(r, " (", p, ")", sep = ""),
                nrow = nrow(corr_mat$r), dimnames = c(var_names, var_names))
  return(tab)
}

corr_test_fmt <- function(dataset, cat_thresh = 5, precision = 2,
                          correct = "both",
                          use="pairwise", method="pearson",
                          adjust="holm", alpha=.05, ci=T) {

  n_unique <- sapply(dataset[names(dataset)], function(x) length(unique(x)))
  cat_cols <- as.vector(n_unique < cat_thresh)
  cont_vars <- dataset[!cat_cols]
  corr <- psych::corr.test(cont_vars, use=use, method=method,
                           adjust=adjust, alpha=alpha, ci=ci)

  corr_fmt <- format_correlation(corr, precision)

  if (correct == "corrected") {
    tri <- lower.tri(corr_fmt, diag = T)
  } else if (correct == "uncorrected") {
    tri <- upper.tri(corr_fmt, diag = T)
  } else if (correct == "both") {
    tri <- corr_fmt != corr_fmt
  }

  corr_fmt[tri] <- ""
  diag(tri) <- ""
  return(corr_fmt)
}
