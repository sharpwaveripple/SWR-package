vector_lm <- function(lhs, rhs, df) {
    if (typeof(rhs)=="list") {
        lm_form <- lapply(lhs, function(x) {
            lapply(rhs, function(y) make_formula(x, y))})
    } else {
        lm_form <- lapply(lhs, function(x) make_formula(x, rhs))
    }
    lm_form <- unlist(lm_form)
    fit <- lapply(lm_form, function(x) lm(x, df))
    return(fit)
}

format_lm <- function(lm, p_adj_method='none') {
    require(sjstats)
    require(parameters)
    std <- sjstats::std_beta(lm)
    b <- exact_dec(std$std.estimate)
    ci <- paste(exact_dec(std$conf.low), exact_dec(std$conf.high), sep=', ')
    b <- paste(b, paren(ci))
    p <- p.adjust(parameters::p_value(lm)[-1,]$p, p_adj_method)
    results <- cbind(b, format_p(p))
    r2 <- exact_dec(summary(lm)$adj.r.squared)
    results <- rbind(results, c(r2, ''))
    rownames(results) <- c(std$term, 'R2')
    colnames(results) <- c('b', 'p')
    return(results)
}

combine_lm <- function(lms, p_adj_method='none', clean_d=NULL) {
    seen <- c()
    for (fit in lms) {
        iv <- names(fit$coefficients)
        unseen <- iv %in% seen
        seen <- c(seen, iv[!unseen])
    }
    seen <- c(seen[which(seen != '(Intercept)')], 'R2')
    results <- c()
    for (fit in lms) {
        dv <- names(fit$model)[1]
        summ <- matrix(, nrow=length(seen), ncol=2,
                       dimnames=list(seen, c('b', 'p')))
        clean_lm <- format_lm(fit, p_adj_method=p_adj_method)
        for (row in rownames(clean_lm)) {
            summ[row, 'b'] <- clean_lm[row, 'b']
            summ[row, 'p'] <- clean_lm[row, 'p']
        }
        results <- cbind(results, summ)
    }
    results[which(is.na(results))] <- ''
    if (!is.null(clean_d)) rownames(results) <- clean_names(rownames(results), clean_d)
    colnames(results) <- rep(c('β (95% CI)', 'P'), length(lms))
    return(results)
}
