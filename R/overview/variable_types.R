source("R/utils/write_table.R")

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

    write_table(data = types, output_file = output_file)
}