format_pvals <- function(p_vector, precision = 3,
                         p_equals = F, whitespace = T,
                         leading_zeroes = T) {

  lowest <- paste(".",
                  paste(integer(precision), collapse = ""),
                  sep = "")
  second_lowest <- paste(".",
                         paste(integer(precision - 1), collapse = ""),
                         "1",
                         sep = "")
  p <- format(round(p_vector, precision), nsmall = precision)

  if (!leading_zeroes) {
    p <- sub("^(-?)0.", "\\1.", p)
  } else {
    lowest <- sub(".", "0.", lowest)
    second_lowest <- sub(".", "0.", second_lowest)
  }

  if (p_equals) {
    p <- paste("P =", p)
  }

  p <- sub(lowest, paste("<", second_lowest), p)
  p <- sub("= <", "<", p)

  if (!whitespace) {
    p <- gsub(" ", "", p)
  }
  return(p)
}

