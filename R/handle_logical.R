library(ggplot2)

.handle_logical_variables <- function(column, output_file) {
    if (!is.logical(column)) {
        print("Error: column is not logical")
        return(NULL)
    }
    if (!is.character(output_file)) {
        print("Error: output_file is not a character")
        return(NULL)
    }

    col_name <- deparse(substitute(column))

    # Count True and False values
    true_count <- sum(column, na.rm = TRUE)
    false_count <- sum(!column, na.rm = TRUE)
    total_count <- length(column)

    # Calculate percentages
    true_percentage <- (true_count / total_count) * 100
    false_percentage <- (false_count / total_count) * 100

    # Missing values
    missing_values <- sum(is.na(column))
    percent_missing_values <- (missing_values / total_count) * 100

    # Memory size
    total_size <- object.size(column)

    # Create a data frame to hold the statistics
    stats <- data.frame(
        `True Values` = true_count,
        `True Percentage` = true_percentage,
        `False Values` = false_count,
        `False Percentage` = false_percentage,
        `Missing Values` = missing_values,
        `Percent Missing Values` = percent_missing_values,
        `Total Size in Memory (bytes)` = as.numeric(total_size),
        check.names = FALSE
    )

    # Format the statistics to write as a table in markdown
    formatted_data <- data.frame(
        Statistic = rownames(t(stats)),
        Value = as.vector(t(stats)),
        row.names = NULL
    )

    # Write to the markdown file
    write_to_table(data = formatted_data, output_file = output_file)

    # Plotting: Bar Plot for TRUE/FALSE Values
    if (!dir.exists("plots")) {
        dir.create("plots")
    }

    # 1. Generate Bar Plot
    barplot_file <- paste0("plots/barplot_", col_name, ".png")
    ggplot(data.frame(x = column), aes(x = factor(x))) +
        geom_bar(fill = "lightcoral", color = "black") +
        labs(title = paste("Bar Plot of", col_name), x = col_name, y = "Count") +
        theme_minimal() -> bar_plot
    ggsave(barplot_file, plot = bar_plot, width = 6, height = 4)

    # 2. Write Markdown Links to the Image
    cat("\n#### Bar Plot\n", file = output_file, append = TRUE)
    cat("![Bar Plot](./", barplot_file, ")\n", sep = "", file = output_file, append = TRUE)
}
