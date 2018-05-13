format_continuous <- function(vals, var_name, sig_fig) {
  var_mean <- round(mean(vals, na.rm = T), sig_fig)
  mean_fmt <- format(var_mean, nsmall = sig_fig)
  var_sd <- round(sd(vals, na.rm = T), sig_fig)
  sd_fmt <- format(var_sd, nsmall = sig_fig)
  descr_fmt <- paste(mean_fmt, " (", sd_fmt, ")", sep = "")
  var_name <- paste(var_name, ", mean (SD)", sep = "")
  descr_fmt <- matrix(descr_fmt, dimnames = list(var_name, ""))
  return(descr_fmt)
}

format_ordinal <- function(vals, var_name, sig_fig) {
  var_median <- median(vals, na.rm = T)
  median_fmt <- format(var_median, nsmall = sig_fig)
  var_iqr <- round(IQR(vals, na.rm = T), sig_fig)
  iqr_fmt <- format(var_iqr, nsmall = sig_fig)
  descr_fmt <- paste(median_fmt, " (", iqr_fmt, ")", sep = "")
  var_name <- paste(var_name, ", median (IQR)", sep = "")
  descr_fmt <- matrix(descr_fmt, dimnames = list(var_name, ""))
  return(descr_fmt)
}

format_nominal <- function(vals, var_name, sig_fig) {
  frequencies <- table(vals)
  percentages <- round(frequencies / length(vals) * 100, sig_fig)
  percentages_fmt <- format(percentages, nsmall = sig_fig, trim = T)
  counts <- matrix(paste(frequencies, " (", percentages_fmt, "%)", sep = ""))
  level_names <- paste(var_name, names(frequencies), sep = ": ")
  level_names <- paste(level_names, ", n (%)", sep = "")
  rownames(counts) <- level_names
  return(counts)
}

na_report <- function(na_vals, var_name, subj_list) {
  na_vals <- which(na_vals)
  num_na <- length(na_vals)
  per_missing <- round(num_na / length(subj_list) * 100, 1)
  missingid <- paste(subj_list[na_vals], collapse = ", ")
  report <- paste(num_na,
                  " (", per_missing, "%) ",
                  "missing for ", var_name, ": ",
                  missingid,
                  sep = "")
  return(report)
}

guess_vartype <- function(vals,
                          nominal_thr = 5,
                          ordinal_thr = 15) {
  dtype <- typeof(vals)
  if (dtype == "double") {
    vartype <- "continuous"
  } else if (dtype == "integer") {
    n_uniq <- length(unique(vals))
    if (n_uniq < nominal_thr) {
      vartype <- "nominal"
    } else if (nominal_thr < n_uniq & n_uniq < ordinal_thr) {
      vartype <- "ordinal"
    } else {
      vartype <- "continuous"
    }
  } else {
    vartype <- "nominal"
  }
  return(vartype)
}

save_descriptives <- function(descriptives, missing_report, output_basename) {
  descriptives_file <- paste(output_basename, ".tsv", sep = "")
  write.table(descriptives, descriptives_file, sep = "\t",
              col.names = NA, quote = F)
  if (!is.null(missing_report)) {
    missing_report_file <- paste(output_basename, "_missing.txt", sep = "")
    write(missing_report, file = missing_report_file)
  }
}

describe_vartype <- function(vals, var_name, vartype, sig_fig) {
  if (vartype == "continuous") {
    formatted_vals <- format_continuous(vals, var_name, sig_fig)
  } else if (vartype == "ordinal") {
    formatted_vals <- format_ordinal(vals, var_name, sig_fig)
  } else if (vartype == "nominal") {
    formatted_vals <- format_nominal(vals, var_name, sig_fig)
  }
  return(formatted_vals)
}

describe <- function(dataset, subj_var, sig_fig = 1,
                     clean_names = NULL, output_basename = NULL) {
  if (!is.null(clean_names)) {
    names(dataset) <- clean_names
  }
  subj_list <- dataset[[subj_var]]
  missing_report <- c()
  descriptives <- c()
  for (var_name in names(dataset)) {
    if (var_name == subj_var) next
    values <- dataset[[var_name]]
    na_vals <- is.na(values)
    if (any(na_vals)) {
      missing_report <- c(missing_report,
                          na_report(na_vals, var_name, subj_list))
    }
    vartype <- guess_vartype(values)
    descriptives <- rbind(descriptives,
                          describe_vartype(values,
                                           var_name,
                                           vartype,
                                           sig_fig))
  }
  colnames(descriptives) <- paste("n =", nrow(dataset))
  if (!is.null(output_basename)) {
    save_descriptives(descriptives, missing_report, output_basename)
  }
  return(list(descriptives, missing_report))
}
