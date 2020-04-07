vector_lm <- function(lhs, rhs, df) {
  if (typeof(rhs) == "list") {
    lm_form <- lapply(lhs, function(x) {
      lapply(rhs, function(y) make_formula(x, y))
    })
  } else {
    lm_form <- lapply(lhs, function(x) make_formula(x, rhs))
  }
  lm_form <- unlist(lm_form)
  fit <- lapply(lm_form, function(x) lm(x, df))
  return(fit)
}

lm_pvals <- function(lm, p_adj_method = "none") {
  p_col <- "Pr(>|t|)"
  p <- as.data.frame(summary(lm)$coefficients)[p_col]
  p_adj <- p.adjust(p[[p_col]], p_adj_method)
  p[[p_col]] <- p_adj
  return(p)
}

format_lm <- function(lm, digits = 2, p_adj_method = "none", clean_d = NULL) {
  b <- exact_dec(cbind(coef(lm), confint(lm)), digits)
  b_fmt <- paste(b[, 1], " [", b[, 2], ", ", b[, 3], "]", sep = "")
  p_fmt <- format_p(as.matrix(lm_pvals(lm, p_adj_method)))
  results <- cbind(b_fmt, p_fmt)
  r2 <- exact_dec(summary(lm)$adj.r.squared)
  results <- rbind(results, c(r2, ""))
  rownames(results) <- c(rownames(b), "R^2")
  colnames(results) <- c("β [95% CI]", "P")
  if (!is.null(clean_d)) rownames(results) <- clean_names(rownames(results), clean_d)
  return(results)
}

combine_lm <- function(lms, p_adj_method = "none", clean_d = NULL) {
  seen <- c()
  for (fit in lms) {
    iv <- names(fit$coefficients)
    unseen <- iv %in% seen
    seen <- c(seen, iv[!unseen])
  }
  seen <- c(seen[which(seen != "(Intercept)")], "R2")
  results <- c()
  for (fit in lms) {
    dv <- names(fit$model)[1]
    summ <- matrix(,
      nrow = length(seen), ncol = 2,
      dimnames = list(seen, c("b", "p"))
    )
    clean_lm <- format_lm(fit, p_adj_method = p_adj_method)
    for (row in rownames(clean_lm)) {
      summ[row, "b"] <- clean_lm[row, "b"]
      summ[row, "p"] <- clean_lm[row, "p"]
    }
    results <- cbind(results, summ)
  }
  results[which(is.na(results))] <- ""
  if (!is.null(clean_d)) rownames(results) <- clean_names(rownames(results), clean_d)
  colnames(results) <- rep(c("β (95% CI)", "P"), length(lms))
  return(results)
}
