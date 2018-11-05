format_p <- function(p, digits=3, p_under=.001) {
  fmt_str <- paste("%.", digits, "f", sep="")
  p_less_than <- p < p_under
  p[] <- sprintf(fmt_str, p)
  p_under_str <- paste("<", p_under)
  p[p_less_than] <- p_under_str
  return(p)
}

format_corr <- function(corr_test_obj, digits = 3, p_under = .001) {
  # Takes the output of psych::corr.test() and formats it as a r (p) matrix
  fmt_str <- paste("%.", digits, "f", sep="")
  r <- corr_test_obj$r
  r[] <- sprintf(fmt_str, r)
  p <- format_p(corr_test_obj$p, digits, p_under)
  corr_table <- r
  corr_table[] <- paste(r, " (", p, ")", sep="")
  return(corr_table)
}
