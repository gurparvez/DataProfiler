#' Write Dataset Statistics to Markdown
#'
#' This function generates a markdown table containing statistics about a given dataset
#' and appends it to a specified markdown file. The statistics include the number of variables,
#' number of observations, missing cells, duplicate rows, and memory size.
#'
#' @param data A data frame for which the statistics will be calculated.
#' @param output_file A character string specifying the name of the markdown file to which
#'                    the statistics will be appended. Default is "profiler_report.md".
#'
#' @details The function first validates that the input is a data frame and that the output
#'          file name is a character string. It then calculates various statistics using
#'          the helper function `.get_data_statistics()`.
#'          Finally, it formats these statistics into a markdown table format and appends it
#'          to the specified markdown file.
#'
#' @return None. The function does not return a value but modifies the specified markdown file.
#'
#' @examples
#' # Example usage
#' df <- data.frame(
#'   a = c(1, 2, 3, NA, 5),
#'   b = c("x", "y", "z", "x", "y")
#' )
#' write_statistics(df, output_file = "my_statistics.md")
#'
#' @export
write_statistics <- function(data, output_file = "profiler_report.md") {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(output_file))

    cat("\n## Dataset Statistics\n", file = output_file, append = TRUE)

    statistics <- .get_data_statistics(data)
    
    header <- "| Statistic                          | Value     |"
    separator <- "|------------------------------------|-----------|"

    rows <- c()
    for (i in seq_along(statistics)) {
        statistic_name <- names(statistics)[i]
        statistic_value <- statistics[i]
        rows <- c(rows, paste0("| ", statistic_name, " | ", statistic_value, " |"))
    }

    table <- c(header, separator, rows)
    cat(table, file = output_file, sep = "\n", append = TRUE)
}


.get_data_statistics <- function(df) {
    print("Getting data statistics...")
    # Number of variables (columns)
    num_vars <- ncol(df)
    
    # Number of observations (rows)
    num_obs <- nrow(df)
    
    # Missing cells and percentage
    missing_cells <- sum(is.na(df))
    total_cells <- num_vars * num_obs
    missing_percentage <- (missing_cells / total_cells) * 100
    
    # Duplicate rows and percentage
    duplicate_rows <- sum(duplicated(df))
    duplicate_percentage <- (duplicate_rows / num_obs) * 100
    
    # Memory size in bytes
    total_size <- object.size(df)
    avg_record_size <- total_size / num_obs
    
    # Create a data frame to hold the statistics
    stats <- data.frame(
        `Number of Variables` = num_vars,
        `Number of Observations` = num_obs,
        `Missing Cells` = missing_cells,
        `Missing Cells (%)` = missing_percentage,
        `Duplicate Rows` = duplicate_rows,
        `Duplicate Rows (%)` = duplicate_percentage,
        `Total Size in Memory (bytes)` = as.numeric(total_size),
        `Average Record Size (bytes)` = as.numeric(avg_record_size),
        check.names = FALSE
    )
    
    return(stats)
}
