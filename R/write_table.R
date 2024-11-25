#' Write Data Frame as Markdown Table
#' 
#' @description
#' Converts a two-column data frame into a markdown-formatted table and appends it
#' to a specified output file. This utility function is designed specifically for
#' creating two-column tables in markdown format, typically used for key-value pair
#' representations.
#' 
#' @param data A data frame with exactly two columns
#' @param output_file Character string specifying the output markdown file path.
#'                    Defaults to "profiler_report.md"
#' 
#' @return NULL invisibly. The function writes results to the specified output file
#'         as a side effect.
#' 
#' @details
#' The function creates a markdown table with the following format:
#' ```
#' | Column1 | Column2 |
#' |---------|---------|
#' | value1  | value2  |
#' ```
#' 
#' The function performs the following steps:
#' 1. Validates that the input data frame has exactly two columns
#' 2. Creates the table header using column names
#' 3. Adds a markdown separator row
#' 4. Converts each data row to markdown format
#' 5. Appends the resulting table to the specified output file
#' 
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Statistic = c("Mean", "Median"),
#'   Value = c(25.5, 26.0)
#' )
#' write_table(data, "output.md")
#' }
#' 
#' @export
write_to_table <- function(data, output_file = "profiler_report.md") {
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
