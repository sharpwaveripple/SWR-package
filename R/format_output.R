format_p <- function(p, digits = 3, p_under = .001) {
  fmt_str <- paste("%.", digits, "f", sep="")
  p_less_than <- p < p_under
  p[] <- sprintf(fmt_str, p)
  p_under_str <- paste("<", p_under, sep="")
  p[p_less_than] <- p_under_str
  return(p)
}

format_corr <- function(corr_test_obj, digits = 2, p_digits = 3, p_under = .001) {
  # Takes the output of psych::corr.test() and formats it as a r (p) matrix
  fmt_str <- paste("%.", digits, "f", sep="")
  r <- corr_test_obj$r
  r[] <- sprintf(fmt_str, r)
  p <- format_p(corr_test_obj$p, p_digits, p_under)
  corr_table <- r
  corr_table[] <- paste(r, " (", p, ")", sep="")
  diag(corr_table) <- ""
  return(corr_table)
}

format_alpha <- function(alpha_obj, digits = 3) {
  fmt_str <- paste("%.", digits, "f", sep="")
  response_freq <- (alpha_obj$response.freq) * 100
  item_stats <- cbind(response_freq, alpha_obj$alpha.drop['std.alpha'])
  item_stats[] <- sprintf(fmt_str, as.matrix(item_stats))
  return(item_stats)
}

extract_fit <- function(lavaan_fit, digits = 2, p_digits = 3, scaled = TRUE) {
  fmt_str <- paste("%.", digits, "f", sep="")
  fit_ind <- c("chisq", "pvalue", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
               "cfi", "tli", "srmr")
  if (scaled) {
    print("Using scaled fit indices...")
    fit_ind <- paste(fit_ind, "scaled", sep=".")
  }
  fits <- lavaan::fitmeasures(fit, fit_ind)
  fits <- sprintf(fmt_str, fits)
  chisq <- paste("x2 = ", fits[1], ", P = ", fits[2], sep="")
  rmsea <- paste("RMSEA = ", fits[3],
                 ", 95% CI = [", fits[4], ", ", fits[5], "]", sep="")
  cfi <- paste("CFI = ", fits[6])
  tli <- paste("TLI = ", fits[7])
  srmr <- paste("SRMR = ", fits[8])
  fit_list <- list(chisq=chisq, rmsea=rmsea, cfi=cfi, tli=tli, srmr=srmr)
  return(fit_list)
}

