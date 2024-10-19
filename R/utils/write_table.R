write_table <- function(data, output_file = "profiler_report.md") {
    if (ncol(data) != 2) {
        stop("The data frame must have exactly two columns.")
    }

    header <- paste0("| ", paste(names(data), collapse = " | "), " |")

    separator <- paste0("|", paste(rep("------", ncol(data)), collapse = "|"), "|")

    rows <- apply(data, 1, function(row) {
        paste0("| ", paste(row, collapse = " | "), " |")
    })

    table <- c(header, separator, rows)
    cat(table, file = output_file, sep = "\n", append = TRUE)
}

# write_table(data = data.frame(a = 1:10, b = 11:20), output_file = "test.md")
