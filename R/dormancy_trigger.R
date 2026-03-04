#' @title Identify Trigger Conditions for Dormant Patterns
#' @name dormancy_trigger
#' @description
#' Analyzes detected dormant patterns to identify the specific conditions under
#' which they would activate. This function goes beyond detection to characterize
#' the precise trigger mechanisms and their sensitivity.
#'
#' @param dormancy_result An object of class "dormancy" from \code{\link{dormancy_detect}}.
#' @param sensitivity Numeric. The sensitivity level for trigger detection, ranging
#'   from 0 (low sensitivity, only major triggers) to 1 (high sensitivity, minor
#'   triggers included). Default is 0.5.
#' @param n_bootstrap Integer. Number of bootstrap samples for confidence intervals.
#'   Default is 100.
#' @param verbose Logical. Whether to print progress messages. Default is FALSE.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{triggers} - A data frame with trigger details:
#'     pattern_id, trigger_variable, trigger_type, threshold_value,
#'     sensitivity_score, confidence_lower, confidence_upper
#'   \item \code{trigger_map} - A matrix showing trigger relationships
#'   \item \code{recommendations} - Character vector of actionable insights
#' }
#'
#' @details
#' Trigger identification is crucial for risk management and early warning systems.
#' A dormant pattern might be triggered by:
#'
#' \itemize{
#'   \item \strong{Threshold triggers}: When a variable crosses a specific value
#'   \item \strong{Region triggers}: When observations fall within specific data regions
#'   \item \strong{Categorical triggers}: When a categorical condition is met
#'   \item \strong{Compound triggers}: When multiple conditions align
#'   \item \strong{Temporal triggers}: When time-dependent patterns emerge
#' }
#'
#' The function uses bootstrap resampling to provide confidence intervals around
#' trigger estimates, ensuring robust identification even with limited data.
#'
#' @seealso \code{\link{dormancy_detect}}, \code{\link{dormancy_risk}}
#'
#' @examples
#' set.seed(42)
#' n <- 500
#' x <- rnorm(n)
#' z <- sample(c(0, 1), n, replace = TRUE)
#' y <- ifelse(z == 1, 0.8 * x + rnorm(n, 0, 0.3), rnorm(n))
#' data <- data.frame(x = x, y = y, z = factor(z))
#'
#' result <- dormancy_detect(data, method = "conditional")
#' triggers <- dormancy_trigger(result)
#' print(triggers)
#'
#' @export
dormancy_trigger <- function(dormancy_result, sensitivity = 0.5,
                             n_bootstrap = 100, verbose = FALSE) {
  
  if (!inherits(dormancy_result, "dormancy")) {
    stop("dormancy_result must be an object of class 'dormancy'")
  }
  
  if (nrow(dormancy_result$patterns) == 0) {
    warning("No dormant patterns detected. Run dormancy_detect first.")
    return(list(
      triggers = data.frame(),
      trigger_map = matrix(),
      recommendations = "No dormant patterns to analyze."
    ))
  }
  
  if (verbose) message("Analyzing triggers for ", nrow(dormancy_result$patterns), " patterns...")
  
  data <- dormancy_result$data
  patterns <- dormancy_result$patterns
  
  triggers_list <- list()
  
  for (i in seq_len(nrow(patterns))) {
    pattern <- patterns[i, ]
    
    if (verbose) message("  Pattern ", i, ": ", pattern$variable_1, " ~ ", pattern$variable_2)
    
    # Get the variables involved
    v1 <- pattern$variable_1
    v2 <- pattern$variable_2
    trigger_var <- pattern$trigger_variable
    
    # Bootstrap confidence intervals
    boot_results <- matrix(NA, nrow = n_bootstrap, ncol = 4)
    colnames(boot_results) <- c("threshold", "sensitivity", "effect_size", "activation_prob")
    
    for (b in seq_len(n_bootstrap)) {
      # Resample data
      boot_idx <- sample(seq_len(nrow(data)), replace = TRUE)
      boot_data <- data[boot_idx, ]
      
      # Estimate trigger characteristics
      if (trigger_var %in% colnames(boot_data)) {
        trigger_vals <- boot_data[[trigger_var]]
      } else {
        # Self-triggered pattern
        trigger_vals <- boot_data[[v1]]
      }
      
      # Find optimal threshold
      thresholds <- quantile(trigger_vals, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
      
      best_threshold <- NA
      best_effect <- 0
      
      x <- boot_data[[v1]]
      y <- boot_data[[v2]]
      
      for (th in thresholds) {
        below <- trigger_vals <= th
        above <- trigger_vals > th
        
        if (sum(below) < 5 || sum(above) < 5) next
        
        cor_below <- cor(x[below], y[below], use = "complete.obs")
        cor_above <- cor(x[above], y[above], use = "complete.obs")
        
        effect <- abs(cor_above - cor_below)
        
        if (effect > best_effect) {
          best_effect <- effect
          best_threshold <- th
        }
      }
      
      boot_results[b, 1] <- best_threshold
      boot_results[b, 2] <- best_effect
      boot_results[b, 3] <- best_effect
      boot_results[b, 4] <- mean(trigger_vals > best_threshold, na.rm = TRUE)
    }
    
    # Compute confidence intervals
    ci_threshold <- quantile(boot_results[, 1], probs = c(0.025, 0.975), na.rm = TRUE)
    ci_sensitivity <- quantile(boot_results[, 2], probs = c(0.025, 0.975), na.rm = TRUE)
    
    # Determine trigger type
    trigger_type <- determine_trigger_type(pattern, dormancy_result$method)
    
    triggers_list[[i]] <- data.frame(
      pattern_id = i,
      pattern_variables = paste0(v1, " ~ ", v2),
      trigger_variable = trigger_var,
      trigger_type = trigger_type,
      threshold_value = median(boot_results[, 1], na.rm = TRUE),
      sensitivity_score = median(boot_results[, 2], na.rm = TRUE),
      confidence_lower = ci_threshold[1],
      confidence_upper = ci_threshold[2],
      activation_probability = median(boot_results[, 4], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
  
  triggers_df <- do.call(rbind, triggers_list)
  
  # Filter by sensitivity
  triggers_df <- triggers_df[triggers_df$sensitivity_score >= (1 - sensitivity) * 0.3, ]
  
  # Create trigger map
  all_vars <- unique(c(patterns$variable_1, patterns$variable_2, patterns$trigger_variable))
  trigger_map <- matrix(0, nrow = length(all_vars), ncol = length(all_vars),
                        dimnames = list(all_vars, all_vars))
  
  for (i in seq_len(nrow(triggers_df))) {
    trigger_var <- triggers_df$trigger_variable[i]
    pattern_vars <- strsplit(triggers_df$pattern_variables[i], " ~ ")[[1]]
    trigger_map[trigger_var, pattern_vars] <- trigger_map[trigger_var, pattern_vars] + 1
  }
  
  # Generate recommendations
  recommendations <- generate_trigger_recommendations(triggers_df, sensitivity)
  
  result <- list(
    triggers = triggers_df,
    trigger_map = trigger_map,
    recommendations = recommendations,
    metadata = list(
      n_bootstrap = n_bootstrap,
      sensitivity = sensitivity,
      analysis_time = Sys.time()
    )
  )
  
  class(result) <- "dormancy_trigger"
  return(result)
}


#' Determine the type of trigger
#' @keywords internal
determine_trigger_type <- function(pattern, method) {
  
  trigger_region <- pattern$trigger_region
  trigger_var <- pattern$trigger_variable
  
  if (grepl("^x > |^x < |^x = ", trigger_region)) {
    return("threshold")
  }
  
  if (grepl("phase", trigger_var)) {
    return("phase")
  }
  
  if (grepl("extreme|normal", trigger_region)) {
    return("cascade")
  }
  
  if (grepl("^[0-9]+,[0-9]+", trigger_region)) {
    return("region")
  }
  
  return("conditional")
}


#' Generate actionable recommendations from trigger analysis
#' @keywords internal
generate_trigger_recommendations <- function(triggers_df, sensitivity) {
  
  if (nrow(triggers_df) == 0) {
    return("No significant triggers detected at current sensitivity level.")
  }
  
  recommendations <- character()
  
  # High-risk triggers
  high_risk <- triggers_df[triggers_df$activation_probability > 0.3, ]
  if (nrow(high_risk) > 0) {
    recommendations <- c(recommendations,
                         paste0("HIGH RISK: ", nrow(high_risk),
                                " pattern(s) have >30% activation probability. ",
                                "Monitor: ", paste(unique(high_risk$trigger_variable),
                                                   collapse = ", ")))
  }
  
  # Sensitive triggers
  sensitive <- triggers_df[triggers_df$sensitivity_score > 0.5, ]
  if (nrow(sensitive) > 0) {
    recommendations <- c(recommendations,
                         paste0("SENSITIVE: ", nrow(sensitive),
                                " pattern(s) are highly sensitive to trigger conditions. ",
                                "Small changes in ", paste(unique(sensitive$trigger_variable),
                                                           collapse = ", "), " could activate them."))
  }
  
  # Cascade potential
  cascade <- triggers_df[triggers_df$trigger_type == "cascade", ]
  if (nrow(cascade) > 0) {
    recommendations <- c(recommendations,
                         paste0("CASCADE POTENTIAL: ", nrow(cascade),
                                " pattern(s) could trigger chain reactions. ",
                                "Key variables: ", paste(unique(cascade$trigger_variable),
                                                         collapse = ", ")))
  }
  
  if (length(recommendations) == 0) {
    recommendations <- "All detected patterns have low immediate risk. Continue monitoring."
  }
  
  return(recommendations)
}


#' @export
print.dormancy_trigger <- function(x, ...) {
  cat("Dormancy Trigger Analysis\n")
  cat("=========================\n\n")
  
  cat("Number of patterns analyzed:", nrow(x$triggers), "\n\n")
  
  if (nrow(x$triggers) > 0) {
    cat("Top Triggers:\n")
    print(head(x$triggers[, c("pattern_variables", "trigger_variable",
                              "trigger_type", "activation_probability")], 10))
    cat("\n")
  }
  
  cat("Recommendations:\n")
  for (rec in x$recommendations) {
    cat("  -", rec, "\n")
  }
  
  invisible(x)
}
