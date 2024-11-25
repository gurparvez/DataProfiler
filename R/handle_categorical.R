library(ggplot2)

.handle_categorical_variables <- function(column, output_file) {
    if (!is.factor(column)) {
        print("Error: column is not a factor")
        return(NULL)
    }
    if (!is.character(output_file)) {
        print("Error: output_file is not a character")
        return(NULL)
    }

    col_name <- deparse(substitute(column))

    # Number of distinct values (levels)
    num_distinct_values <- length(levels(column))
    percent_distinct_values <- (num_distinct_values / length(column)) * 100

    # Missing values
    missing_values <- sum(is.na(column))
    percent_missing_values <- (missing_values / length(column)) * 100

    # Memory size
    total_size <- object.size(column)

    # Create a data frame to hold the statistics
    stats <- data.frame(
        `Number of Distinct Values` = num_distinct_values,
        `Percent Distinct Values` = percent_distinct_values,
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

    # Plotting: Bar Plot for Category Frequencies
    if (!dir.exists("plots")) {
        dir.create("plots")
    }

    # 1. Generate Bar Plot
    barplot_file <- paste0("plots/barplot_", col_name, ".png")
    ggplot(data.frame(x = column), aes(x = x)) +
        geom_bar(fill = "skyblue", color = "black") +
        labs(title = paste("Bar Plot of", col_name), x = col_name, y = "Count") +
        theme_minimal() -> bar_plot
    ggsave(barplot_file, plot = bar_plot, width = 6, height = 4)

    # 2. Write Markdown Links to the Image
    cat("\n#### Bar Plot\n", file = output_file, append = TRUE)
    cat("![Bar Plot](./", barplot_file, ")\n", sep = "", file = output_file, append = TRUE)
}
