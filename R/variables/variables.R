source("R/variables/handle_numeric.R")

write_variables <- function(data, output_file = "profiler_report.md") {
    if (!is.data.frame(data)) {
        print("Error: write_variables : data is not a data frame")
        return(NULL)
    }
    if (!is.character(output_file)) {
        print("Error: write_variables : output_file is not a character")
        return(NULL)
    }

    # print("In write_variables")

    # loop through columns and call the function based on the type
    for (col_name in colnames(data)) {
        print(paste("Column name:", col_name))
        column <- data[[col_name]]
        print(paste("Getting variable statistics for", col_name))
        cat("\n### ", col_name, "\n", file = output_file, append = TRUE)

        if (is.numeric(column)) {
            print("Handling numeric variables")
            .handle_numeric_variables(column, output_file)
        # } else if (is.character(data[, i])) {
        #     write_character_variables(data, i, output_file)
        # } else if (is.factor(data[, i])) {
        #     write_factor_variables(data, i, output_file)
        # } else if (is.logical(data[, i])) {
        #     write_logical_variables(data, i, output_file)
        }
    }
    
}

