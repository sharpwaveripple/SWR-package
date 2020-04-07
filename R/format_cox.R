cox2table <- function(fit, digits = 2, p_digits = 3, clean_d = NULL) {
  summ <- summary(fit)
  est <- exact_dec(summ$conf.int, digits)
  est_fmt <- paste(est[, 1], " [", est[, 3], ", ", est[, 4], "]", sep = "")
  p <- format_p(summ$coefficients[, ncol(summ$coefficients)]) # columns will change if robust variance is added
  tab <- cbind(est_fmt, p)
  colnames(tab) <- c("HR [95% CI]", "P")
  if (!is.null(clean_d)) {
    rownames(tab) <- clean_names(rownames(est), clean_d)
  } else {
    rownames(tab) <- rownames(est)
  }
  return(tab)
}
