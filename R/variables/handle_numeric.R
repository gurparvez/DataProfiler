.handle_numeric_variables <- function(column, output_file) {
    if (!is.vector(column)) {
        print("Error: column is not a vector")
        return(NULL)
    }
    if (!is.numeric(column)) {
        print("Error: column is not numeric")
        return(NULL)
    }
    if (!is.character(output_file)) {
        print("Error: .handle_numeric_variable : output_file is not a character")
        return(NULL)
    }

    # Number of distinct values
    num_distinct_values <- length(unique(column))
    percent_distinct_values <- (length(unique(column)) / length(column)) * 100
    
    # Missing values
    missing_values <- sum(is.na(column))
    percent_missing_values <- sum(is.na(column)) / length(column)

    # Infinite values
    infinite_values <- sum(is.infinite(column))
    percent_infinite_values <- (sum(is.infinite(column)) / length(column)) * 100

    # Mean
    mean_values <- mean(column, na.rm = TRUE)

    # Median
    median_values <- median(column, na.rm = TRUE)

    # Mode
    mode_values <- names(which.max(table(column)))

    # Standard deviation
    sd_values <- sd(column, na.rm = TRUE)

    # Variance
    variance_values <- var(column, na.rm = TRUE)

    # Minimum
    min_values <- min(column, na.rm = TRUE)

    # Maximum
    max_values <- max(column, na.rm = TRUE)

    # Zeros
    zeros <- sum(column == 0)
    percent_zeros <- (sum(column == 0) / length(column)) * 100

    # Negative values
    negative_values <- sum(column < 0)
    percent_negative_values <- (sum(column < 0) / length(column)) * 100

    # Memory size in bytes
    total_size <- object.size(column)

    # Create a data frame to hold the statistics
    stats <- data.frame(
        `Number of Distinct Values` = num_distinct_values,
        `Percent Distinct Values` = percent_distinct_values,
        `Missing Values` = missing_values,
        `Percent Missing Values` = percent_missing_values,
        `Infinite Values` = infinite_values,
        `Percent Infinite Values` = percent_infinite_values,
        `Mean` = mean_values,
        `Median` = median_values,
        `Mode` = mode_values,
        `Standard Deviation` = sd_values,
        `Variance` = variance_values,
        `Minimum` = min_values,
        `Maximum` = max_values,
        `Zeros` = zeros,
        `Percent Zeros` = percent_zeros,
        `Negative Values` = negative_values,
        `Percent Negative Values` = percent_negative_values,
        `Total Size in Memory (bytes)` = as.numeric(total_size),
        check.names = FALSE
    )
    
    # Format the statistics to write as a table in md
    formatted_data <- data.frame(
        Statistic = rownames(t(stats)),
        Value = as.vector(t(stats)),
        row.names = NULL
    )

    write_table(data = formatted_data, output_file = output_file)

}
