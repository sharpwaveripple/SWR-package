format_p <- function(p, digits = 3, p_under = .001) {
  fmt_str <- paste("%.", digits, "f", sep="")
  p_less_than <- p < p_under
  p[] <- sprintf(fmt_str, p)
  p_under_str <- paste("<", p_under, sep="")
  p[p_less_than] <- p_under_str
  return(p)
}

format_corr <- function(corr_test_obj, p_digits = 3, p_under = .001) {
  # Takes the output of psych::corr.test() and formats it as a r (p) matrix
  r <- corr_test_obj$r
  r[] <- sprintf("%.2f", r)
  p <- format_p(corr_test_obj$p, p_digits, p_under)
  corr_table <- r
  corr_table[] <- paste(r, " (", p, ")", sep="")
  diag(corr_table) <- ""
  return(corr_table)
}

format_alpha <- function(alpha_obj) {
  # Summarizes output from psych::alpha()
  scale_measures <- c("std.alpha", "G6(smc)", "average_r", "mean", "sd")
  scale_stats <- alpha_obj$total[scale_measures]
  scale_stats[1:3] <- sprintf("%.2f", scale_stats[1:3])
  scale_stats[4:5] <- sprintf("%.1f", scale_stats[4:5])
  response_freq <- alpha_obj$response.freq * 100
  response_freq[] <- sprintf("%.1f", as.matrix(response_freq))
  alpha_drop <- alpha_obj$alpha.drop["std.alpha"]
  alpha_drop[] <- sprintf("%.2f", as.matrix(alpha_drop))
  stats <- list(alpha=scale_stats[[1]],
                lambda=scale_stats[[2]],
                interitem_r=scale_stats[[3]],
                avg_score=paste(scale_stats[[4]],
                                " (", scale_stats[[5]], ")", sep=""),
                items=cbind(response_freq, alpha_drop))
  return(stats)
}

extract_fit <- function(lavaan_fit, p_digits = 3, scaled = TRUE) {
  # Summarizes output from lavaan::lavaan()
  fit_ind <- c("chisq", "pvalue", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
               "cfi", "tli", "srmr")
  if (scaled) {
    print("Using scaled fit indices...")
    fit_ind <- paste(fit_ind, "scaled", sep=".")
  }
  fits <- as.numeric(lavaan::fitmeasures(fit, fit_ind))
  chisq <- paste("χ^2 = ", sprintf("%.1f", fits[1]),
                 ", P = ", format_p(fits[2], p_digits), sep="")
  fits[3:8] <- sprintf("%.2f", fits[3:8])
  rmsea <- paste("RMSEA = ", fits[3],
                 ", 95% CI = [", fits[4], ", ", fits[5], "]", sep="")
  cfi <- paste("CFI = ", fits[6])
  tli <- paste("TLI = ", fits[7])
  srmr <- paste("SRMR = ", fits[8])
  fit_list <- list(chisq=chisq, rmsea=rmsea, cfi=cfi, tli=tli, srmr=srmr)
  return(fit_list)
}

