library(corrplot)

# Function to plot and save the correlation heatmap
plot_correlation_heatmap <- function(dataframe, output_file) {
  # Check if the dataframe is valid
  if (!is.data.frame(dataframe)) {
    print("Error: Input is not a data frame")
    return(NULL)
  }
  
  # Select only numerical columns for correlation calculation
  numeric_data <- dataframe[sapply(dataframe, is.numeric)]
  
  # Check if there are numeric columns to compute correlation
  if (ncol(numeric_data) == 0) {
    print("Error: No numeric columns found for correlation")
    return(NULL)
  }

  # Calculate the correlation matrix for numeric columns only
  correlation_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Create the "plots" directory if it doesn't exist
  if (!dir.exists("plots")) {
    dir.create("plots")
  }

  # Save the correlation heatmap to a file
  plot_file <- "plots/correlation_heatmap.png"
  png(plot_file, width = 800, height = 600)
  corrplot(correlation_matrix, method = "color", 
           col = colorRampPalette(c("red", "white", "blue"))(200), 
           type = "full", 
           tl.cex = 0.8, # text label size
           number.cex = 0.7, # number size
           addCoef.col = "black", # add coefficient values
           tl.srt = 45, # rotate the text labels
           cl.cex = 0.8) # color legend text size
  dev.off()

  # Append the plot link to the markdown output file
  cat("\n#### Correlation Heatmap\n", file = output_file, append = TRUE)
  cat("![Correlation Heatmap](./", plot_file, ")\n", sep = "", file = output_file, append = TRUE)
}

# Example usage:
# df <- read.csv("your_data.csv") # Load your data
# plot_correlation_heatmap(df, "output_report.md")  # Specify your markdown file name
