source("R/write_table.R")

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
    
    formatted_data <- data.frame(
        Statistic = rownames(t(statistics)),
        Value = as.vector(t(statistics)),
        row.names = NULL
    )

    write_to_table(data = formatted_data, output_file = output_file)
    
    # header <- "| Statistic                          | Value     |"
    # separator <- "|------------------------------------|-----------|"

    # rows <- c()
    # for (i in seq_along(statistics)) {
    #     statistic_name <- names(statistics)[i]
    #     statistic_value <- statistics[i]
    #     rows <- c(rows, paste0("| ", statistic_name, " | ", statistic_value, " |"))
    # }

    # table <- c(header, separator, rows)
    # cat(table, file = output_file, sep = "\n", append = TRUE)
}


#' Write Variable Type Summary to Markdown Report
#' 
#' @description
#' Analyzes and writes a summary of variable types in a data frame to a markdown file.
#' The function counts and categorizes variables into numeric, categorical, and text types,
#' then creates a detailed table showing the type of each variable.
#' 
#' @param data A data frame containing the variables to analyze
#' @param output_file Character string specifying the output markdown file path.
#'                    Defaults to "profiler_report.md"
#' 
#' @return NULL invisibly. The function writes results to the specified output file
#'         as a side effect.
#' 
#' @details
#' The function performs the following:
#' 1. Counts the number of variables by type:
#'    * Numeric (using is.numeric)
#'    * Categorical (using is.factor)
#'    * Text (using is.character)
#' 2. Creates a summary showing counts for each type
#' 3. Generates a detailed table showing the type of each variable
#' 
#' Input validation is performed using stopifnot() to ensure:
#' * data is a data frame
#' * output_file is a character string
#' 
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   age = c(25, 30, 35),
#'   name = c("John", "Jane", "Bob"),
#'   category = factor(c("A", "B", "A"))
#' )
#' write_variable_types(data, "my_report.md")
#' }
#' 
#' @seealso 
#' \code{\link{write_table}} for the underlying table writing functionality
#' 
#' @export
write_variable_types <- function(data, output_file = "profiler_report.md") {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(output_file))

    cat("\n## Variable Types\n", file = output_file, append = TRUE)

    # get numeric variables
    numeric_vars <- sapply(data, is.numeric)
    numeric_count <- sum(numeric_vars)
    cat(paste0("Numeric: ", numeric_count, "  \n"), file = output_file, append = TRUE)

    # get categorical variables
    categorical_vars <- sapply(data, is.factor)
    categorical_count <- sum(categorical_vars)
    cat(paste0("Categorical: ", categorical_count, "  \n"), file = output_file, append = TRUE)

    # get text variables
    text_vars <- sapply(data, is.character)
    text_count <- sum(text_vars)
    cat(paste0("Text: ", text_count, "  \n"), file = output_file, append = TRUE)

    # Get variable types
    variable_types <- sapply(data, class)

    # Create a data frame to hold the variable types
    types <- data.frame(
        Variable = names(variable_types),
        Type = variable_types,
        check.names = FALSE
    )

    write_to_table(data = types, output_file = output_file)
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

