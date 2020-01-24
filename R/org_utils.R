orgify <- function(tab, incl_colnames=T, incl_rownames=T) {
    if (incl_rownames) tab <- cbind(rownames(tab), tab)
    if (incl_colnames) tab <- rbind(colnames(tab), tab)
    rows <- apply(tab, 1, paste, collapse="|")
    rows <- paste("|", rows, sep="")
    org_tab <- paste(rows, collapse="\n")
    org_tab <- sub("\n", "\n|-\n", org_tab)
    if (incl_rownames & incl_colnames) org_tab <- sub("NA", "", org_tab)
    return(org_tab)
}

org_table <- function(tab, fname,
                      number=NULL, title=NULL, note=NULL) {
    tab <- orgify(tab)
    name <- c()
    if (!is.null(number)) name <- paste("*Table ", as.character(number), "*.", sep="")
    if (!is.null(title)) name <- paste(name, title)
    if (!is.null(number) | !is.null(table)) tab <- paste(name, tab, sep="\n")
    if (!is.null(note)) {
        note <- paste("/Note/. ", note, ".", sep="")
        tab <- paste(tab, note, sep="\n")
    }
    write.table(tab, fname, col.names=F, row.names=F, quote=F,
                fileEncoding = "utf-8")
}
