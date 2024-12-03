library(ggplot2)

.handle_character_variables <- function(column, column_name, output_file) {
    if (!is.vector(column)) {
        print("Error: column is not a vector")
        return(NULL)
    }
    if (!is.character(column)) {
        print("Error: column is not character")
        return(NULL)
    }
    if (!is.character(output_file)) {
        print("Error: .handle_character_variables : output_file is not a character")
        return(NULL)
    }

    col_name <- column_name

    # Number of distinct values
    num_distinct_values <- length(unique(column))
    percent_distinct_values <- (num_distinct_values / length(column)) * 100
    
    # Missing values
    missing_values <- sum(is.na(column))
    percent_missing_values <- (missing_values / length(column)) * 100

    # Memory size in bytes
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
    
    # Format the statistics to write as a table in md
    formatted_data <- data.frame(
        Statistic = rownames(t(stats)),
        Value = as.vector(t(stats)),
        row.names = NULL
    )

    write_to_table(data = formatted_data, output_file = output_file)

    # Plotting: Bar Plot for Value Frequencies
    if (!dir.exists("plots")) {
        dir.create("plots")
    }

    # 1. Generate Bar Plot
    barplot_file <- paste0("plots/barplot_", col_name, ".png")
    paste0("saving ", barplot_file, " of ", col_name, " column...")
    ggplot(data.frame(x = column), aes(x = x)) +
        geom_bar(fill = "lightgreen", color = "black") +
        labs(title = paste("Bar Plot of", col_name), x = col_name, y = "Count") +
        theme_minimal() -> bar_plot
    ggsave(barplot_file, plot = bar_plot, width = 6, height = 4)

    # 2. Write Markdown Links to the Image
    cat("\n#### Bar Plot\n", file = output_file, append = TRUE)
    cat("![Bar Plot](./", barplot_file, ")\n", sep = "", file = output_file, append = TRUE)
}
