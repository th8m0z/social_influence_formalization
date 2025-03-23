# evaluation.R
# Script to evaluate the social influence model with visualization

# Set seed for reproducibility
set.seed(40)

# Load required functions from model file
source("script.R")

# Load packages for visualization
library(ggplot2)
library(gridExtra)

# Function to calculate weight of advice statistics
calculate_woa_stats <- function(first_estimates, social_info, confidence) {
  n <- length(first_estimates)
  woa_values <- numeric(n)
  
  for(i in 1:n) {
    woa_values[i] <- weightOfAdvice(confidence[i], first_estimates[i], social_info)
  }
  
  return(list(
    mean_woa = mean(woa_values),
    median_woa = median(woa_values),
    sd_woa = sd(woa_values)
  ))
}

# Function to run comparative simulations for normal vs DK confidence
compare_confidence_models <- function(n_individuals, true_value, n_trials = 100) {
  # Run simulations for normal confidence
  normal_results <- runSimulation(
    n_individuals = n_individuals,
    true_value = true_value,
    knowledge_distribution = "truncnorm",
    social_info_type = "mean",
    n_trials = n_trials,
    dk = FALSE
  )
  
  # Run simulations for Dunning-Kruger confidence
  dk_results <- runSimulation(
    n_individuals = n_individuals,
    true_value = true_value,
    knowledge_distribution = "truncnorm", 
    social_info_type = "mean",
    n_trials = n_trials,
    dk = TRUE
  )
  
  # Calculate percentage improvement in MAE
  normal_first_mae <- normal_results$aggregate_results$mean_individual_distance_first
  normal_second_mae <- normal_results$aggregate_results$mean_individual_distance_second
  normal_pct_improvement <- ((normal_first_mae - normal_second_mae) / normal_first_mae) * 100
  
  dk_first_mae <- dk_results$aggregate_results$mean_individual_distance_first
  dk_second_mae <- dk_results$aggregate_results$mean_individual_distance_second
  dk_pct_improvement <- ((dk_first_mae - dk_second_mae) / dk_first_mae) * 100
  
  # Calculate absolute MAE improvement
  normal_mae_improvement <- normal_first_mae - normal_second_mae
  dk_mae_improvement <- dk_first_mae - dk_second_mae
  
  # Calculate relative effect of DK
  relative_dk_effect <- (normal_pct_improvement - dk_pct_improvement) / normal_pct_improvement
  
  # Calculate WOC effect differences
  normal_woc_effect_first <- normal_results$aggregate_results$mean_woc_percent_benefit_first
  normal_woc_effect_second <- normal_results$aggregate_results$mean_woc_percent_benefit_second
  
  dk_woc_effect_first <- dk_results$aggregate_results$mean_woc_percent_benefit_first
  dk_woc_effect_second <- dk_results$aggregate_results$mean_woc_percent_benefit_second
  
  return(list(
    normal_results = normal_results$aggregate_results,
    dk_results = dk_results$aggregate_results,
    effect_sizes = list(
      normal_pct_improvement = normal_pct_improvement,
      dk_pct_improvement = dk_pct_improvement,
      normal_mae_improvement = normal_mae_improvement,
      dk_mae_improvement = dk_mae_improvement,
      relative_dk_effect = relative_dk_effect,
      normal_woc_change = normal_woc_effect_second - normal_woc_effect_first,
      dk_woc_change = dk_woc_effect_second - dk_woc_effect_first
    )
  ))
}

# Function to measure egocentric discounting
measure_egocentric_discounting <- function(n_individuals, true_value, n_trials = 100) {
  normal_woa_values <- numeric(n_trials)
  dk_woa_values <- numeric(n_trials)
  
  for(i in 1:n_trials) {
    # Generate data for one trial with normal confidence
    prior_knowledge <- dPriorKnowledge(n_individuals, "truncnorm")
    normal_confidence <- sapply(prior_knowledge, calculateConfidence, dk = FALSE)
    dk_confidence <- sapply(prior_knowledge, calculateConfidence, dk = TRUE)
    first_estimates <- dGroupFirstEstimate(prior_knowledge, true_value)
    
    # Calculate mean social info (excluding own estimate for each individual)
    social_info_vector <- numeric(n_individuals)
    for(j in 1:n_individuals) {
      social_info_vector[j] <- mean(first_estimates[-j])
    }
    
    # Calculate WOA for normal confidence
    normal_woa_stats <- calculate_woa_stats(first_estimates, social_info_vector, normal_confidence)
    normal_woa_values[i] <- normal_woa_stats$mean_woa
    
    # Calculate WOA for DK confidence
    dk_woa_stats <- calculate_woa_stats(first_estimates, social_info_vector, dk_confidence)
    dk_woa_values[i] <- dk_woa_stats$mean_woa
  }
  
  return(list(
    normal_mean_woa = mean(normal_woa_values),
    normal_sd_woa = sd(normal_woa_values),
    dk_mean_woa = mean(dk_woa_values),
    dk_sd_woa = sd(dk_woa_values),
    egocentric_discounting_normal = 0.5 - mean(normal_woa_values),
    egocentric_discounting_dk = 0.5 - mean(dk_woa_values)
  ))
}

# Run evaluations
cat("Running model evaluations...\n")

# 1. Compare normal and Dunning-Kruger confidence models
confidence_comparison <- compare_confidence_models(100, 100, n_trials = 100)

# 2. Measure egocentric discounting
egocentric_results <- measure_egocentric_discounting(100, 100, n_trials = 100)

# Print results
cat("\n======= CONFIDENCE MODEL COMPARISON =======\n")
cat("Normal confidence MAE % improvement:", confidence_comparison$effect_sizes$normal_pct_improvement, "%\n")
cat("DK confidence MAE % improvement:", confidence_comparison$effect_sizes$dk_pct_improvement, "%\n")
cat("Relative DK effect (negative = worse):", confidence_comparison$effect_sizes$relative_dk_effect, "\n")
cat("Normal WOC change:", confidence_comparison$effect_sizes$normal_woc_change, "\n")
cat("DK WOC change:", confidence_comparison$effect_sizes$dk_woc_change, "\n")

cat("\n======= EGOCENTRIC DISCOUNTING =======\n")
cat("Normal confidence WOA:", egocentric_results$normal_mean_woa, "\n")
cat("DK confidence WOA:", egocentric_results$dk_mean_woa, "\n")
cat("Normal egocentric discounting:", egocentric_results$egocentric_discounting_normal, "\n")
cat("DK egocentric discounting:", egocentric_results$egocentric_discounting_dk, "\n")

# Create directory for saving plots if it doesn't exist
if (!dir.exists("photos")) {
  dir.create("photos")
}

#=====================================================
# Plot 1: Accuracy Improvement Comparison (% MAE reduction)
#=====================================================
acc_data <- data.frame(
  Model = c("Normal Confidence", "Dunning-Kruger"),
  Improvement = c(
    confidence_comparison$effect_sizes$normal_pct_improvement,
    confidence_comparison$effect_sizes$dk_pct_improvement
  )
)

acc_plot <- ggplot(acc_data, aes(x = Model, y = Improvement, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", Improvement)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(
    title = "Accuracy Improvement from Social Influence",
    subtitle = "Percentage reduction in mean absolute error after social exchange",
    y = "Error Reduction (%)",
    caption = "Percentage reduction in mean absolute error from first to second estimates.\nHigher values indicate greater improvement in accuracy after social influence.",
    x = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9),
    panel.background = element_rect(fill = "aliceblue", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot
ggsave("photos/accuracy_improvement.png", acc_plot, width = 7, height = 5, dpi = 300)

#=====================================================
# Plot 2: Weight on Advice Comparison
#=====================================================
woa_data <- data.frame(
  Model = c("Normal", "Dunning-Kruger", "Theoretical Optimal"),
  WOA = c(
    egocentric_results$normal_mean_woa,
    egocentric_results$dk_mean_woa,
    0.5
  )
)

woa_plot <- ggplot(woa_data, aes(x = Model, y = WOA, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", WOA)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("steelblue", "firebrick", "darkgreen")) +
  labs(
    title = "Egocentric Discounting in Weight on Advice",
    subtitle = "How much individuals adjust toward social information",
    y = "Weight on Advice",
    caption = "Weight on Advice (WOA) represents the proportion that individuals move toward\nsocial information. WOA = 0 means no adjustment, WOA = 1 means complete adoption\nof social information. Values below 0.5 indicate egocentric discounting.",
    x = ""
  ) +
  ylim(0, 0.6) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9),
    panel.background = element_rect(fill = "aliceblue", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot
ggsave("photos/weight_on_advice.png", woa_plot, width = 7, height = 5, dpi = 300)

#=====================================================
# Plot 3: WOC Effect Changes
#=====================================================
woc_data <- data.frame(
  Model = rep(c("Normal", "Dunning-Kruger"), each = 2),
  Estimate = rep(c("First Estimates", "Second Estimates"), 2),
  Benefit = c(
    confidence_comparison$normal_results$mean_woc_percent_benefit_first,
    confidence_comparison$normal_results$mean_woc_percent_benefit_second,
    confidence_comparison$dk_results$mean_woc_percent_benefit_first,
    confidence_comparison$dk_results$mean_woc_percent_benefit_second
  )
)

woc_plot <- ggplot(woc_data, aes(x = Estimate, y = Benefit, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Benefit)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(
    title = "Wisdom of Crowds Benefit",
    subtitle = "How much aggregation improves accuracy beyond average individual",
    y = "Error Reduction (%)",
    caption = "Percentage reduction in error achieved by using the aggregated group estimate\ninstead of the average individual estimate. Higher values indicate stronger\nWisdom of Crowds effect. The decline from first to second estimates shows\nhow social influence reduces diversity-driven error cancellation.",
    x = ""
  ) +
  scale_y_continuous(limits = c(0, 100)) +  # Set y-axis limit to ensure labels are visible
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9),
    panel.background = element_rect(fill = "aliceblue", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot
ggsave("photos/woc_effect.png", woc_plot, width = 7, height = 5, dpi = 300)

