cox2table <- function(fit, digits = 2, p_digits = 3, clean_d = NULL) {
  summ <- summary(fit)
  exp_coef <- as.data.frame(summ$conf.int)[c("exp(coef)", "lower .95", "upper .95")]
  exp_coef <- sapply(exp_coef, exact_dec, digits)
  cox_tab <- cbind(
    exp_coef[, 1],
    paste(exp_coef[, 2], exp_coef[, 3], sep = "-"),
    format_p(as.data.frame(summ$coefficients)[["Pr(>|z|)"]])
  )
  colnames(cox_tab) <- c("HR", "95% CI", "P")
  rownames(cox_tab) <- rhs
  return(cox_tab)
}
