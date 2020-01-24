format_p <- function(p) {
  p_3_digits <- p < .01 & p >= .001
  p_001 <- p < .001
  #p_0001 <- p < .0001
  #p_0001 <- p < .0001 & p > .00001
  #p_00001 <- p < .00001
  p_fmt <- sprintf("%.3f", p)
  #p_fmt[p_3_digits] <- sprintf("%.3f", p[p_3_digits])
  p_fmt[p_001] <- "<0.001"
  #p_fmt[p_0001] <- "<0.0001"
  #p_fmt[p_00001] <- "<0.00001"
  return(p_fmt)
}

format_p_old <- function(p, digits = 3, p_under = .001) {
  fmt_str <- paste("%.", digits, "f", sep="")
  p_less_than <- p < p_under
  p[] <- sprintf(fmt_str, p)
  p_under_str <- paste("<", p_under, sep="")
  p[p_less_than] <- p_under_str
  return(p)
}

dodgy_p <- function(p) {
    p_05 <- p > .01 & p < .055
    p_01 <- p <= .01 & p > .001
    p_001 <- p <= .001 & p > .0001
    p_0001 <- p <= .0001 & p > .00001
    p_00001 <- p <= .00001
    p_fmt <- sprintf("%.2f", p)
    p_fmt[p_05] <- "≤0.05"
    p_fmt[p_01] <- "≤0.01"
    p_fmt[p_001] <- "≤0.001"
    p_fmt[p_0001] <- "≤0.0001"
    p_fmt[p_00001] <- "≤0.00001"
    return(p_fmt)
}

paren <- function(s, sq=F) {
  if (sq) {
    return(paste("[", s, "]", sep=""))
  } else {
    return(paste("(", s, ")", sep=""))
  }
}

format_corr <- function(corr_test_obj, p_digits = 3,
                        rounded_p = F, p_report=NULL) {
  # Takes the output of psych::corr.test() and formats it as a r (p) matrix
  r <- corr_test_obj$r
  r[] <- sprintf("%.2f", r)
  ## p <- format_p(corr_test_obj$p, p_digits, p_under)
  if (rounded_p) {
    p <- dodgy_p(corr_test_obj$p)
  } else {
    p <- format_p(corr_test_obj$p)
  }

  corr_table <- r
  corr_table[] <- paste(r, " (", p, ")", sep="")
  diag(corr_table) <- ""

  if (p_report=='corrected') {
      corr_table[lower.tri(corr_table)] <- ""
  } else if (p_report=='uncorrected') {
      corr_table[upper.tri(corr_table)] <- ""
  }
  return(corr_table)
}

exact_dec <- function(number, digits=2) {
  format_str <- paste("%.", paste0(digits), "f", sep="")
  rounded <- round(number, digits)
  format_num <- sprintf(format_str, rounded)
  return(format_num)
}

mean_sd <- function(col, digits=1, as_string=TRUE) {
  mean_val <- mean(col, na.rm=T)
  sd_val <- sd(col, na.rm=T)
  if (as_string) {
    val <- paste(exact_dec(mean_val, digits), paren(exact_dec(sd_val, digits)))
  } else {
    val <- matrix(c(mean_val, sd_val), ncol=2)
  }
  return(val)
}

format_t <- function(t_stat, rounded_p = F) {
  t <- paste('t_{', exact_dec(t_stat$parameter), '}=',
             exact_dec(t_stat$statistic), sep='')
  if (rounded_p) {
    p <- paste('P=', dodgy_p(t_stat$p.value), sep='')
  } else {
    p <- paste('P=', format_p(t_stat$p.value), sep='')
  }
  stat <- paste(t, p, sep=', ')
  return(stat)
}

format_u <- function(u_stat, rounded_p = F) {
  u <- paste('U=', u_stat$statistic, sep='')
  if (rounded_p) {
    p <- paste('P=', dodgy_p(u_stat$p.value), sep='')
  } else {
    p <- paste('P=', format_p(u_stat$p.value), sep='')
  }
  stat <- paste(u, p, sep=', ')
  return(stat)
}

make_formula <- function(lhs, rhs) {
  form <- paste(lhs, '~', paste(rhs, collapse='+'))
  return(form)
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

rmsea <- function(lavaan_fit, scaled = TRUE, robust = FALSE, pvalue = FALSE) {
    fit_ind <- c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper")
    if (scaled && robust) stop("Cannot specify scaled and robust")
    if (pvalue) {
        fit_ind <- c(fit_ind, "rmsea.pvalue")
    }
    if (scaled) {
        print("Using scaled fit indices")
        fit_ind <- paste(fit_ind, "scaled", sep=".")
    }
    if (robust) {
        print("Using robust fit indices")
        fit_ind <- paste(fit_ind, "robust", sep=".")
    }
    fits <- as.numeric(lavaan::fitmeasures(lavaan_fit, fit_ind))
    fits <- sprintf("%.2f", fits)
    rmsea_format <- paste(fits[1], " [", fits[2], ", ", fits[3], "]",
                          sep="")
    return(rmsea_format)
}

extract_fit <- function(lavaan_fit, p_digits = 3, scaled = TRUE) {
  # Summarizes output from lavaan::lavaan()
  fit_ind <- c("chisq", "pvalue", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
               "cfi", "tli", "srmr")
  if (scaled) {
    print("Using scaled fit indices...")
    fit_ind <- paste(fit_ind, "scaled", sep=".")
  }
  fits <- as.numeric(lavaan::fitmeasures(lavaan_fit, fit_ind))
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


clean_names <- function(names, d) {
    new_names <- c()
    for (i in names) {
        if (i %in% names(d)) {
            new_names <- c(new_names, d[[i]])
        } else {
            new_names <- c(new_names, i)
        }
    }
    return(new_names)
}
