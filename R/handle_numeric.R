library(ggplot2)

.handle_numeric_variables <- function(column, column_name, output_file) {
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

    # Extract column name
    col_name <- column_name

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

    # Write the stats table to the markdown file
    write_to_table(data = formatted_data, output_file = output_file)

    # Plotting: Histogram and Boxplot
    if (!dir.exists("plots")) {
        dir.create("plots")
    }

    # 1. Generate Histogram
    histogram_file <- paste0("plots/histogram_", col_name, ".png")
    print(paste("saving ", histogram_file, " of ", col_name, " column..."))
    ggplot(data.frame(x = column), aes(x)) +
        geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
        labs(title = paste("Histogram of", col_name), x = col_name, y = "Frequency") +
        theme_minimal() -> hist_plot
    ggsave(histogram_file, plot = hist_plot, width = 6, height = 4)

    # 2. Generate Boxplot
    boxplot_file <- paste0("plots/boxplot_", col_name, ".png")
    print(paste("saving ", boxplot_file, " of ", col_name, " column..."))
    ggplot(data.frame(x = column), aes(y = x)) +
        geom_boxplot(fill = "orange", color = "black") +
        labs(title = paste("Boxplot of", col_name), y = col_name) +
        theme_minimal() -> box_plot
    ggsave(boxplot_file, plot = box_plot, width = 4, height = 6)

    # 3. Write Markdown Links to the Images
    cat("\n#### Histogram\n", file = output_file, append = TRUE)
    cat("![Histogram](./", histogram_file, ")\n", sep = "", file = output_file, append = TRUE)

    cat("\n#### Boxplot\n", file = output_file, append = TRUE)
    cat("![Boxplot](./", boxplot_file, ")\n", sep = "", file = output_file, append = TRUE)
}
