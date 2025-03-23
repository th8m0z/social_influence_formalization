source("script.R")
library(truncnorm)

# Function to create a 2D heatmap for Weight of Advice by Confidence and Distance
plot_woa_heatmap <- function() {
  # Create grid of confidence and distance values
  # Focus on distance range 0-3 where most meaningful changes occur
  confidence_values <- seq(0, 1, by = 0.01)
  distance_values <- seq(0, 3, by = 0.03)  # Reduced upper limit to 3
  
  # Create matrix to store weights
  weights <- matrix(nrow = length(distance_values), ncol = length(confidence_values))
  
  # Calculate weights for each combination
  for (i in 1:length(distance_values)) {
    for (j in 1:length(confidence_values)) {
      confidence <- confidence_values[j]
      distance <- distance_values[i]
      first_estimate <- 100
      social_information <- 100 * exp(distance)
      
      weights[i, j] <- weightOfAdvice(confidence, first_estimate, social_information)
    }
  }
  
  # Use default margins to avoid errors
  par(mar = c(5, 4, 4, 2) + 0.1)
  
  # Create heatmap - blue (low WOA) to red (high WOA)
  image(confidence_values, distance_values, weights, 
        col = colorRampPalette(c("blue", "purple", "red"))(100),
        xlab = "Confidence", ylab = "Distance (log ratio)",
        main = "Weight of Advice by Confidence and Distance")
  
  # Add more contour lines
  contour(confidence_values, distance_values, weights, 
          levels = seq(0, 1, by = 0.1),  
          add = TRUE, col = "black", lwd = 0.5)
  
  legend("topright", 
         legend = c("WOA = 0.0", "WOA = 0.5", "WOA = 1.0"), 
         fill = colorRampPalette(c("blue", "purple", "red"))(3), 
         title = "Weight of Advice",
         cex = 0.8)
}



plot_dunning_kruger <- function() {
  # Generate sequence of PK values for smooth curve
  PK_values <- seq(0, 1, by=0.01)
  confidence_values <- dunning_kruger_confidence(PK_values)
  
  # Calculate the actual percentile rank for each PK value in our distribution
  library(truncnorm)
  percentile_ranks <- ptruncnorm(PK_values, a=0, b=1, mean=0.5, sd=0.15)
  
  # Find where the DK curve crosses the perfect calibration curve
  diff_values <- confidence_values - percentile_ranks
  crossings <- which(diff(sign(diff_values)) != 0)
  
  # Properly interpolate to find the exact crossing point
  if(length(crossings) > 0) {
    # Get the indices around the crossing
    idx1 <- crossings[1]
    idx2 <- idx1 + 1
    
    # Linear interpolation to find exact crossing
    t <- diff_values[idx1] / (diff_values[idx1] - diff_values[idx2])
    crossing_pk <- PK_values[idx1] + t * (PK_values[idx2] - PK_values[idx1])
    
    # At the crossing point, confidence equals percentile rank
    crossing_confidence <- approx(PK_values, confidence_values, crossing_pk)$y
  } else {
    crossing_pk <- NA
    crossing_confidence <- NA
  }
  
  # Create plot
  plot(PK_values, confidence_values, type="l", lwd=2, col="blue",
       main="Dunning-Kruger Effect", 
       xlab="Prior Knowledge", ylab="Perceived Percentile Rank",
       xlim=c(0,1), ylim=c(0,1))
  
  # Add "perfect calibration" line - maps PK to actual percentile ranks
  lines(PK_values, percentile_ranks, lty=2, col="gray")
  
  # Add key points from the paper
  points(0.324, 0.62, pch=19, col="red", cex=1.5)  # 12th percentile knowledge
  points(0.692, 0.75, pch=19, col="green", cex=1.5)  # 90th percentile knowledge
  
  # Mark crossing point if it exists
  if(!is.na(crossing_pk)) {
    points(crossing_pk, crossing_confidence, pch=19, col="purple", cex=1.5)
    # Label the crossing point
    text(crossing_pk, crossing_confidence, 
         sprintf("  (%.2f, %.2f)", crossing_pk, crossing_confidence), 
         pos=4, cex=0.8)
  }
  
  # Add legend
  legend("bottomright", 
         legend=c("Dunning-Kruger Curve", "Perfect Calibration Line", 
                  "12th Percentile (PK=0.324)", "90th Percentile (PK=0.692)", 
                  "Crossing Point"),
         col=c("blue", "gray", "red", "green", "purple"), 
         lty=c(1,2,NA,NA,NA), pch=c(NA,NA,19,19,19), 
         cex=0.7)
  
  # Add annotation
  text(0.324, 0.62, "  (0.324, 0.62)", pos=4, cex=0.8)
  text(0.692, 0.75, "  (0.692, 0.75)", pos=4, cex=0.8)
  
  # Add explanatory note
  mtext("Calibrated for truncated normal distribution with mean=0.5, sd=0.15", 
        side=1, line=4, cex=0.8)
}
plot_dunning_kruger()

plot_weight_vs_confidence <- function(distances = c(0.1, 0.5, 1, 2, 5)) {
  # Create a sequence of confidence values
  conf_values <- seq(0, 1, by = 0.01)
  
  # Set up the plot
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Confidence", ylab = "Weight of Advice",
       main = "Weight of Advice vs. Confidence for Different Distances")
  
  # Define a color palette for different distances
  colors <- rainbow(length(distances))
  
  # Add a line for each distance
  for (i in 1:length(distances)) {
    dist <- distances[i]
    weights <- sapply(conf_values, function(conf) {
      # For a fixed distance, calculate weight directly
      weight <- (1 - conf) * (1 + conf * tanh(dist))
      return(max(0, min(1, weight)))
    })
    
    lines(conf_values, weights, col = colors[i], lwd = 2)
  }
  
  # Add a legend
  legend("topright", legend = paste("Distance =", distances),
         col = colors, lwd = 2, cex = 0.8)
}

plot_weight_vs_confidence()

library(ggplot2)

# Create a data frame from the aggregate results
comparison_data <- data.frame(
  Metric = c(
    "Group Distance (First)",
    "Group Distance (Second)",
    "Individual Distance (First)",
    "Individual Distance (Second)",
    "WOC Benefit (First)",
    "WOC Benefit (Second)",
    "WOC Benefit Change",
    "% WOC Benefit (First)",
    "% WOC Benefit (Second)",
    "% WOC Benefit Change",
    "% Trials Group Improved",
    "% Trials Individual Improved",
    "% Trials WOC Strengthened"
  ),
  Normal = c(
    normal_results$aggregate_results$mean_group_distance_first,
    normal_results$aggregate_results$mean_group_distance_second,
    normal_results$aggregate_results$mean_individual_distance_first,
    normal_results$aggregate_results$mean_individual_distance_second,
    normal_results$aggregate_results$mean_woc_benefit_first,
    normal_results$aggregate_results$mean_woc_benefit_second,
    normal_results$aggregate_results$mean_woc_benefit_change,
    normal_results$aggregate_results$mean_woc_percent_benefit_first,
    normal_results$aggregate_results$mean_woc_percent_benefit_second,
    normal_results$aggregate_results$mean_woc_percent_benefit_change,
    normal_results$aggregate_results$proportion_group_improved * 100,
    normal_results$aggregate_results$proportion_individual_improved * 100,
    normal_results$aggregate_results$proportion_woc_strengthened * 100
  ),
  DK = c(
    dk_results$aggregate_results$mean_group_distance_first,
    dk_results$aggregate_results$mean_group_distance_second,
    dk_results$aggregate_results$mean_individual_distance_first,
    dk_results$aggregate_results$mean_individual_distance_second,
    dk_results$aggregate_results$mean_woc_benefit_first,
    dk_results$aggregate_results$mean_woc_benefit_second,
    dk_results$aggregate_results$mean_woc_benefit_change,
    dk_results$aggregate_results$mean_woc_percent_benefit_first,
    dk_results$aggregate_results$mean_woc_percent_benefit_second,
    dk_results$aggregate_results$mean_woc_percent_benefit_change,
    dk_results$aggregate_results$proportion_group_improved * 100,
    dk_results$aggregate_results$proportion_individual_improved * 100,
    dk_results$aggregate_results$proportion_woc_strengthened * 100
  )
)

# Format numeric values to 2 decimal places
comparison_data$Normal <- round(comparison_data$Normal, 2)
comparison_data$DK <- round(comparison_data$DK, 2)

# Create a difference column to highlight the effect of DK
comparison_data$Difference <- round(comparison_data$DK - comparison_data$Normal, 2)

# Create a ggplot table visualization
# Convert to long format for plotting
library(tidyr)
plot_data <- pivot_longer(comparison_data, 
                          cols = c("Normal", "DK", "Difference"),
                          names_to = "Condition", 
                          values_to = "Value")

# Create grids and text for table
p <- ggplot(plot_data, aes(x = Condition, y = reorder(Metric, desc(row.names(comparison_data))))) +
  geom_tile(fill = "white", color = "black") +
  geom_text(aes(label = Value), size = 3) +
  labs(title = "Comparison of Normal vs. Dunning-Kruger Results",
       x = "", y = "") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 1, size = 8),
    axis.text.x = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Save the plot as a PNG file with white background
ggsave("results_comparison.png", p, width = 8, height = 10, dpi = 300, bg = "white")

cat("Table image saved as 'results_comparison.png'\n")


