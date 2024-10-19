write_variables <- function(data, output_file = "profiler_report.md") {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(output_file))

    # loop through columns
    
}

.handle_numeric_variables <- function(column) {
    stopifnot(is.vector(column))
    print("Getting variable statistics...")

    # Number of distinct values
    num_distinct_values <- sapply(column, function(x) length(unique(x)))
    percent_distinct_values <- sapply(column, function(x) length(unique(x)) / length(x))
    
    # Missing values
    missing_values <- sapply(column, function(x) sum(is.na(x)))
    percent_missing_values <- sapply(column, function(x) sum(is.na(x)) / length(x))

    # Infinite values
    infinite_values <- sapply(column, function(x) sum(is.infinite(x)))
    percent_infinite_values <- sapply(column, function(x) sum(is.infinite(x)) / length(x))

    # Mean
    mean_values <- sapply(column, function(x) mean(x, na.rm = TRUE))

    # Median
    median_values <- sapply(column, function(x) median(x, na.rm = TRUE))

    # Mode
    mode_values <- sapply(column, function(x) names(which.max(table(x))))

    # Standard deviation
    sd_values <- sapply(column, function(x) sd(x, na.rm = TRUE))

    # Variance
    variance_values <- sapply(column, function(x) var(x, na.rm = TRUE))

    # Minimum
    min_values <- sapply(column, function(x) min(x, na.rm = TRUE))

    # Maximum
    max_values <- sapply(column, function(x) max(x, na.rm = TRUE))

    # Zeros
    zeros <- sapply(column, function(x) sum(x == 0))
    percent_zeros <- sapply(column, function(x) sum(x == 0) / length(x))

    # Negative values
    negative_values <- sapply(column, function(x) sum(x < 0))
    percent_negative_values <- sapply(column, function(x) sum(x < 0) / length(x))

    # Memory size in bytes
    total_size <- sapply(column, function(x) object.size(x))

    # Create a data frame to hold the statistics
    stats <- data.frame(
        `Number of Distinct Values` = num_distinct_values,
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
    
    
}
