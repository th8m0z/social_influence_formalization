#' Set seed for reproducibility
set.seed(40)

#' Generate distribution of prior knowledge for a group of individuals
#'
#' @description Generates a vector of prior knowledge values for a group of individuals
#' based on the specified distribution type and parameters.
#'
#' @param n_individuals Integer. Number of individuals to generate prior knowledge for.
#' @param distribution_type Character. Type of distribution to use: "uniform", "normal", "truncnorm", "beta", or "bimodal".
#' @param params List. Parameters specific to the chosen distribution type.
#'
#' @return Numeric vector. Values between 0 and 1 representing prior knowledge for each individual.
#'
#' @examples
#' # Generate uniform prior knowledge for 10 individuals
#' uniform_pk <- dPriorKnowledge(10, "uniform")
#'
#' # Generate normal prior knowledge with custom mean and sd
#' normal_pk <- dPriorKnowledge(10, "normal", list(mean = 0.6, sd = 0.1))
#'
#' # Generate beta distribution with custom shape parameters
#' beta_pk <- dPriorKnowledge(10, "beta", list(alpha = 3, beta = 2))
dPriorKnowledge <- function(n_individuals, distribution_type = "uniform", params = list()) {
  switch(distribution_type,
         "uniform" = {
           # Default uniform distribution between 0 and 1
           min_val <- if(is.null(params$min)) 0 else params$min
           max_val <- if(is.null(params$max)) 1 else params$max
           return(runif(n_individuals, min = min_val, max = max_val))
         },
         "normal" = {
           # Normal distribution with specified mean and sd
           mean_val <- if(is.null(params$mean)) 0.5 else params$mean
           sd_val <- if(is.null(params$sd)) 0.15 else params$sd
           # Clip values to be between 0 and 1
           knowledge <- pmin(pmax(rnorm(n_individuals, mean = mean_val, sd = sd_val), 0), 1)
           return(knowledge)
         },
         "truncnorm" = {
           # Truncated normal distribution between between 0 and 1
           # with specified mean and sd
           mean_val <- if(is.null(params$mean)) 0.5 else param$mean
           sd_val <- if(is.null(params$sd)) 0.15 else param$sd
           truncnorm::rtruncnorm(n_individuals, a = 0, b = 1, mean = mean_val, sd = sd_val)
         },
         "beta" = {
           # Beta distribution for more flexible shapes
           alpha <- if(is.null(params$alpha)) 2 else params$alpha
           beta <- if(is.null(params$beta)) 2 else params$beta
           return(rbeta(n_individuals, shape1 = alpha, shape2 = beta))
         },
         "bimodal" = {
           # Simple bimodal distribution (mix of two normals)
           prop1 <- if(is.null(params$prop1)) 0.5 else params$prop1
           mean1 <- if(is.null(params$mean1)) 0.25 else params$mean1
           mean2 <- if(is.null(params$mean2)) 0.75 else params$mean2
           sd1 <- if(is.null(params$sd1)) 0.1 else params$sd1
           sd2 <- if(is.null(params$sd2)) 0.1 else params$sd2
           
           n1 <- round(n_individuals * prop1)
           n2 <- n_individuals - n1
           
           group1 <- rnorm(n1, mean = mean1, sd = sd1)
           group2 <- rnorm(n2, mean = mean2, sd = sd2)
           
           # Clip values to be between 0 and 1
           knowledge <- pmin(pmax(c(group1, group2), 0), 1)
           return(knowledge)
         },
         # Default to uniform if distribution type not recognized
         runif(n_individuals, min = 0, max = 1)
  )
}

#' Calculate confidence based on the Dunning-Kruger effect
#'
#' @description Maps prior knowledge to perceived confidence following the Dunning-Kruger effect,
#' where individuals with low knowledge overestimate their abilities while experts underestimate theirs.
#'
#' @param PK Numeric vector. Prior knowledge values between 0 and 1.
#'
#' @return Numeric vector. Perceived confidence values adjusted according to the Dunning-Kruger effect.
#'
#' @examples
#' # Generate confidence for a range of prior knowledge values
#' pk_values <- seq(0, 1, by = 0.1)
#' dk_conf <- dunning_kruger_confidence(pk_values)
dunning_kruger_confidence <- function(PK) {
  # Validate input
  if (!is.numeric(PK) || any(PK < 0) || any(PK > 1)) {
    stop("PK must be a numeric value between 0 and 1")
  }
  
  # Recalibrated coefficients to exactly hit both key points
  a = 0.4      # Base confidence at zero knowledge
  b = 1.0      # Controls initial rise
  c = -1.2     # Creates the curve shape
  d = 0.7      # Adjusts high-end behavior
  
  # Calculate perceived percentile rank
  perceived_rank <- a + b*PK + c*PK^2 + d*PK^3
  
  # Ensure all values stay in [0,1] range
  perceived_rank <- pmin(pmax(perceived_rank, 0), 1)
  
  return(perceived_rank)
}

#' Calculate confidence based on prior knowledge
#'
#' @description Determines an individual's confidence level based on their prior knowledge.
#' Can use either a simple linear mapping or the Dunning-Kruger effect.
#'
#' @param prior_knowledge Numeric. Prior knowledge value between 0 and 1.
#' @param dk Logical. Whether to apply the Dunning-Kruger effect. Default is FALSE.
#'
#' @return Numeric. Confidence value between 0 and 1.
#'
#' @examples
#' # Simple linear confidence
#' conf_linear <- calculateConfidence(0.7)
#'
#' # Confidence with Dunning-Kruger effect
#' conf_dk <- calculateConfidence(0.7, dk = TRUE)
calculateConfidence <- function(prior_knowledge, dk=FALSE) {
  # Simple linear relationship between knowledge and confidence
  if (dk == FALSE) {
    return(prior_knowledge)
  } else {
    return(dunning_kruger_confidence(prior_knowledge))
  }
}

#' Generate an individual's first estimate based on prior knowledge
#'
#' @description Generates a first estimate for an individual based on their prior knowledge level
#' and the true value. Higher prior knowledge leads to estimates closer to the true value with less variance.
#'
#' @param prior_knowledge Numeric. Individual's prior knowledge value between 0 and 1.
#' @param true_value Numeric. The correct answer to the estimation task.
#'
#' @return Numeric. The individual's first estimate.
#'
#' @examples
#' # Generate first estimates for individuals with different knowledge levels
#' estimate_novice <- dIndividualFirstEstimate(0.2, 100)
#' estimate_expert <- dIndividualFirstEstimate(0.9, 100)
dIndividualFirstEstimate <- function(prior_knowledge, true_value) {
  # The higher prior knowledge, the smaller the distribution of individual estimates
  # For maximum prior knowledge = 1, sd_of_log = 0.05
  # For minimum prior knowledge = 0, sd_of_log = 1
  sd_of_log = -0.95 * prior_knowledge + 1
  mean_of_log = log(true_value) - sd_of_log^2/2
  
  # Generate a sample from this distribution
  estimate <- rlnorm(1, meanlog = mean_of_log, sdlog = sd_of_log)
  
  return(estimate)
}

#' Generate first estimates for a group of individuals
#'
#' @description Generates first estimates for all individuals in a group based on
#' their prior knowledge and the true value.
#'
#' @param prior_knowledge_vector Numeric vector. Prior knowledge values for each individual.
#' @param true_value Numeric. The correct answer to the estimation task.
#'
#' @return Numeric vector. First estimates for all individuals in the group.
#'
#' @examples
#' # Generate first estimates for a group of 10 individuals
#' pk_values <- runif(10, 0, 1)
#' first_estimates <- dGroupFirstEstimate(pk_values, 100)
dGroupFirstEstimate <- function(prior_knowledge_vector, true_value) {
  # Generate first estimates for all individuals
  first_estimates <- sapply(prior_knowledge_vector, dIndividualFirstEstimate, true_value = true_value)
  return(first_estimates)
}

#' Calculate the weight an individual places on advice
#'
#' @description Determines how much weight an individual places on advice (social information)
#' based on their confidence and the distance between their first estimate and the advice.
#'
#' @param confidence Numeric. Individual's confidence in their first estimate (0-1).
#' @param first_estimate Numeric. Individual's first estimate.
#' @param social_information Numeric. The advice or social information provided.
#'
#' @return Numeric. Weight placed on advice, between 0 and 1.
#'
#' @examples
#' # Calculate weight of advice for different scenarios
#' weight1 <- weightOfAdvice(0.8, 100, 120)  # High confidence, moderate distance
#' weight2 <- weightOfAdvice(0.3, 100, 200)  # Low confidence, large distance
weightOfAdvice <- function(confidence, first_estimate, social_information) {
  # Calculate log-based distance
  log_ratio <- log(social_information / first_estimate)
  distance <- abs(log_ratio)
  
  # tanh of large distances approaches 1
  weight_of_advice <- (1 - confidence) * (1 + confidence * tanh(distance))
  weight_of_advice <- max(0, min(1, weight_of_advice))
  
  return(weight_of_advice)
}

#' Calculate expected second estimate based on first estimate and social information
#'
#' @description Computes the expected value for an individual's second estimate
#' by integrating their first estimate with social information, weighted by confidence.
#'
#' @param first_estimate Numeric. Individual's first estimate.
#' @param social_info Numeric. The social information or advice provided.
#' @param confidence Numeric. Individual's confidence in their first estimate (0-1).
#'
#' @return Numeric. Expected value for the individual's second estimate.
#'
#' @examples
#' # Calculate expected second estimate
#' expected_second <- psi(100, 120, 0.7)
psi <- function(first_estimate, social_info, confidence) {
  # Weight on advice from confidence and the distance between first estimate and social info
  weight_of_advice <- weightOfAdvice(confidence, first_estimate, social_info)
  # Weight on the inividual first estimate
  self_weight <- 1 - weight_of_advice
  
  # The expected value for the second estimate based on weight on advice and self weight
  # Interpreted as a persons tendency to a second estimate value, it can be seen
  # as the central tendency of her second estimate distribution
  expected_second_estimate <- weight_of_advice * social_info + self_weight * first_estimate
  return(expected_second_estimate)
}

#' Generate an individual's second estimate
#'
#' @description Generates a second estimate for an individual after they have received social information,
#' based on their first estimate, confidence, and the social information.
#'
#' @param first_estimate Numeric. Individual's first estimate.
#' @param social_info Numeric. The social information or advice provided.
#' @param confidence Numeric. Individual's confidence in their first estimate (0-1).
#'
#' @return Numeric. The individual's second estimate.
#'
#' @examples
#' # Generate a second estimate
#' second_est <- dIndividualSecondEstimate(100, 120, 0.7)
dIndividualSecondEstimate <- function(first_estimate, social_info, confidence) {
  # Returns the second estimate after applying the Psi function and random noise
  expected_second_estimate <- psi(first_estimate, social_info, confidence)
  second_estimate <- rnorm(1, mean = expected_second_estimate, sd = 1)
  return(second_estimate)
}

#' Generate second estimates for a group of individuals
#'
#' @description Generates second estimates for all individuals in a group after
#' they have received social information.
#'
#' @param first_estimate_vector Numeric vector. First estimates for each individual.
#' @param social_info Numeric. The social information or advice provided to all individuals.
#' @param confidence_vector Numeric vector. Confidence values for each individual.
#'
#' @return Numeric vector. Second estimates for all individuals in the group.
#'
#' @examples
#' # Generate second estimates for a group
#' first_ests <- c(90, 110, 105, 95)
#' conf_vals <- c(0.7, 0.8, 0.6, 0.9)
#' second_ests <- dGroupSecondEstimates(first_ests, 100, conf_vals)
dGroupSecondEstimates <- function(first_estimate_vector, social_info, confidence_vector) {
  # Generate second estimates for all individuals
  second_estimates <- mapply(dIndividualSecondEstimate,
                             first_estimate = first_estimate_vector,
                             confidence = confidence_vector,
                             MoreArgs = list(social_info = social_info))
  return(second_estimates)
}

#' Determine Wisdom of Crowds effects with social influence
#'
#' @description Calculates various metrics to evaluate the impact of social influence 
#' on group estimation accuracy and Wisdom of Crowds effects.
#'
#' @param first_estimate_vector Numeric vector. First estimates for each individual.
#' @param second_estimate_vector Numeric vector. Second estimates for each individual.
#' @param true_value Numeric. The correct answer to the estimation task.
#'
#' @return List. Contains multiple metrics comparing the accuracy of first and second estimates
#' at both individual and group levels, including Wisdom of Crowds benefits.
#'
#' @examples
#' # Evaluate WOC effects
#' first_ests <- c(90, 110, 105, 95)
#' second_ests <- c(95, 105, 102, 96)
#' woc_effects <- determineWOCSocialInfluence(first_ests, second_ests, 100)
determineWOCSocialInfluence <- function(first_estimate_vector, second_estimate_vector, true_value) {
  # Calculate mean estimates
  mean_first <- mean(first_estimate_vector)
  mean_second <- mean(second_estimate_vector)
  
  # Calculate individual means (average of individual estimates)
  mean_individual_first <- mean(first_estimate_vector)
  mean_individual_second <- mean(second_estimate_vector)
  
  # Calculate individual average distances (average error of individuals)
  mean_individual_distance_first <- mean(abs(first_estimate_vector - true_value))
  mean_individual_distance_second <- mean(abs(second_estimate_vector - true_value))
  
  # Calculate group distances (error of the aggregated estimate)
  group_distance_first <- abs(mean_first - true_value)
  group_distance_second <- abs(mean_second - true_value)
  
  # Calculate WOC benefit in absolute units (how many units closer the group is vs avg individual)
  woc_benefit_first <- mean_individual_distance_first - group_distance_first
  woc_benefit_second <- mean_individual_distance_second - group_distance_second
  
  # Change in WOC benefit
  woc_benefit_change <- woc_benefit_second - woc_benefit_first
  
  # Percentage WOC benefit (what % of individual error is eliminated by aggregation)
  woc_percent_benefit_first <- (woc_benefit_first / mean_individual_distance_first) * 100
  woc_percent_benefit_second <- (woc_benefit_second / mean_individual_distance_second) * 100
  woc_percent_benefit_change <- woc_percent_benefit_second - woc_percent_benefit_first
  
  # Also calculate the squared error metrics for completeness
  error_first <- (mean_first - true_value)^2
  error_second <- (mean_second - true_value)^2
  individual_error_first <- mean((first_estimate_vector - true_value)^2)
  individual_error_second <- mean((second_estimate_vector - true_value)^2)
  
  return(list(
    # Group estimates
    group_mean_first = mean_first,
    group_mean_second = mean_second,
    group_distance_first = group_distance_first,
    group_distance_second = group_distance_second,
    
    # Individual averages
    individual_mean_distance_first = mean_individual_distance_first,
    individual_mean_distance_second = mean_individual_distance_second,
    
    # WOC benefits in original units
    woc_benefit_first = woc_benefit_first,
    woc_benefit_second = woc_benefit_second,
    woc_benefit_change = woc_benefit_change,
    
    # WOC benefits as percentages
    woc_percent_benefit_first = woc_percent_benefit_first,
    woc_percent_benefit_second = woc_percent_benefit_second,
    woc_percent_benefit_change = woc_percent_benefit_change,
    
    # Original squared error metrics
    error_first = error_first,
    error_second = error_second,
    individual_error_first = individual_error_first,
    individual_error_second = individual_error_second,
    improvement = error_first - error_second,
    individual_improvement = individual_error_first - individual_error_second
  ))
}

#' Run a simulation of group estimation with social influence
#'
#' @description Runs a complete simulation of group estimation with social influence,
#' generating prior knowledge, confidence, first estimates, social information, and second
#' estimates for a group of individuals over multiple trials.
#'
#' @param n_individuals Integer. Number of individuals in the group.
#' @param true_value Numeric. The correct answer to the estimation task.
#' @param knowledge_distribution Character. Type of distribution for prior knowledge. Default is "uniform".
#' @param knowledge_params List. Parameters for the prior knowledge distribution. Default is empty list.
#' @param social_info_type Character. Method for calculating social information: "mean", "median", or "trimmed_mean". Default is "mean".
#' @param manipulated_social_info Numeric. If provided, uses this fixed value as social information for all individuals. Default is NULL.
#' @param n_trials Integer. Number of simulation trials to run. Default is 1.
#' @param dk Logical. Whether to apply the Dunning-Kruger effect. Default is FALSE.
#'
#' @return List or list of lists. For a single trial, returns detailed results. For multiple trials,
#' returns both individual trial results and aggregated statistics.
#'
#' @examples
#' # Run a single simulation trial
#' sim_result <- runSimulation(n_individuals = 50, true_value = 100)
#'
#' # Run multiple trials with Dunning-Kruger effect
#' multi_sim <- runSimulation(n_individuals = 100, true_value = 100, 
#'                            knowledge_distribution = "normal", n_trials = 10, dk = TRUE)
runSimulation <- function(n_individuals, true_value, knowledge_distribution = "uniform",
                          knowledge_params = list(), social_info_type = "mean",
                          manipulated_social_info = NULL, n_trials = 1, dk=FALSE) {
  
  results <- list()
  
  for(i in 1:n_trials) {
    # Generate prior knowledge
    prior_knowledge <- dPriorKnowledge(n_individuals, knowledge_distribution, knowledge_params)
    
    # Calculate confidence
    confidence <- sapply(prior_knowledge, calculateConfidence,dk=dk)
    
    # Generate first estimates
    first_estimates <- dGroupFirstEstimate(prior_knowledge, true_value)
    
    # Calculate personalized social information and second estimates
    if(!is.null(manipulated_social_info)) {
      # If social_info is manipulated, use the same value for everyone
      social_info <- manipulated_social_info
      second_estimates <- dGroupSecondEstimates(first_estimates, social_info, confidence)
    } else {
      # Create vectors to store personalized social info and second estimates
      social_info_vector <- numeric(n_individuals)
      second_estimates <- numeric(n_individuals)
      
      # Calculate personalized social info for each individual (excluding their own estimate)
      for(j in 1:n_individuals) {
        # Get all estimates except this person's
        other_estimates <- first_estimates[-j]
        
        # Calculate social information based on specified type
        switch(social_info_type,
               "mean" = {
                 social_info_vector[j] <- mean(other_estimates)
               },
               "median" = {
                 social_info_vector[j] <- median(other_estimates)
               },
               "trimmed_mean" = {
                 # Remove the top and bottom 10%
                 social_info_vector[j] <- mean(other_estimates, trim = 0.1)
               },
               # Default to mean
               {
                 social_info_vector[j] <- mean(other_estimates)
               }
        )
        
        # Calculate second estimate for this person using their personalized social info
        second_estimates[j] <- dIndividualSecondEstimate(
          first_estimate = first_estimates[j],
          social_info = social_info_vector[j],
          confidence = confidence[j]
        )
      }
      
      # For compatibility with the rest of the code, store the average social info
      social_info <- mean(social_info_vector)
    }
    
    # Determine WOC effects
    woc_effects <- determineWOCSocialInfluence(first_estimates, second_estimates, true_value)
    
    # Store results for this trial
    results[[i]] <- list(
      prior_knowledge = prior_knowledge,
      confidence = confidence,
      first_estimates = first_estimates,
      social_info = social_info,
      second_estimates = second_estimates,
      woc_effects = woc_effects
    )
  }
  
  if(n_trials == 1) {
    return(results[[1]])
  } else {
    aggregate_results <- list(
      # Group estimates
      mean_group_first = mean(sapply(results, function(x) x$woc_effects$group_mean_first)),
      mean_group_second = mean(sapply(results, function(x) x$woc_effects$group_mean_second)),
      mean_group_distance_first = mean(sapply(results, function(x) x$woc_effects$group_distance_first)),
      mean_group_distance_second = mean(sapply(results, function(x) x$woc_effects$group_distance_second)),
      
      # Individual averages
      mean_individual_distance_first = mean(sapply(results, function(x) x$woc_effects$individual_mean_distance_first)),
      mean_individual_distance_second = mean(sapply(results, function(x) x$woc_effects$individual_mean_distance_second)),
      
      # WOC benefits in original units
      mean_woc_benefit_first = mean(sapply(results, function(x) x$woc_effects$woc_benefit_first)),
      mean_woc_benefit_second = mean(sapply(results, function(x) x$woc_effects$woc_benefit_second)),
      mean_woc_benefit_change = mean(sapply(results, function(x) x$woc_effects$woc_benefit_change)),
      
      # WOC benefits as percentages
      mean_woc_percent_benefit_first = mean(sapply(results, function(x) x$woc_effects$woc_percent_benefit_first)),
      mean_woc_percent_benefit_second = mean(sapply(results, function(x) x$woc_effects$woc_percent_benefit_second)),
      mean_woc_percent_benefit_change = mean(sapply(results, function(x) x$woc_effects$woc_percent_benefit_change)),
      
      # Proportion of trials showing improvement
      proportion_group_improved = mean(sapply(results, function(x) x$woc_effects$group_distance_first > x$woc_effects$group_distance_second)),
      proportion_individual_improved = mean(sapply(results, function(x) x$woc_effects$individual_mean_distance_first > x$woc_effects$individual_mean_distance_second)),
      proportion_woc_strengthened = mean(sapply(results, function(x) x$woc_effects$woc_benefit_change > 0))
    )
    
    return(list(
      trial_results = results,
      aggregate_results = aggregate_results
    ))
  }
}

normal_results <- runSimulation(
  n_individuals = 100,
  true_value = 100,
  knowledge_distribution = "truncnorm",
  social_info_type = "mean",
  n_trials = 100,
  dk = TRUE)
normal_results$aggregate_results

