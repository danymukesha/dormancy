#' @title Assess Risk of Dormant Pattern Activation
#' @name dormancy_risk
#' @description
#' Quantifies the risk associated with dormant patterns, including the probability
#' of activation, potential impact, and uncertainty in risk estimates. This function
#' provides actionable risk metrics for decision-making and monitoring priorities.
#'
#' @param dormancy_result An object of class "dormancy" from \code{\link{dormancy_detect}}.
#' @param depth_result Optional. An object of class "dormancy_depth" from
#'   \code{\link{dormancy_depth}}. If provided, uses depth information for
#'   more accurate risk assessment.
#' @param impact_weights Optional named vector of weights for different impact
#'   types. Default considers symmetric positive/negative impacts.
#' @param time_horizon Numeric. The time horizon for risk assessment (in abstract
#'   units). Longer horizons increase activation probability. Default is 1.
#' @param risk_tolerance Numeric. Risk tolerance threshold for flagging.
#'   Default is 0.3.
#' @param verbose Logical. Whether to print progress messages. Default is FALSE.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{risk_scores} - Data frame with risk metrics for each pattern
#'   \item \code{risk_matrix} - Matrix of activation probability x impact
#'   \item \code{priorities} - Ordered list of patterns by risk priority
#'   \item \code{recommendations} - Risk management recommendations
#'   \item \code{summary} - Overall risk summary statistics
#' }
#'
#' @details
#' Risk assessment for dormant patterns considers multiple dimensions:
#'
#' \itemize{
#'   \item \strong{Activation Probability}: Likelihood that trigger conditions
#'     will be met in the given time horizon
#'   \item \strong{Impact Magnitude}: Expected effect size if the pattern activates
#'   \item \strong{Impact Direction}: Whether activation would be beneficial,
#'     harmful, or neutral
#'   \item \strong{Cascade Potential}: Risk of triggering other patterns
#'   \item \strong{Uncertainty}: Confidence in risk estimates
#' }
#'
#' The risk score combines these dimensions into an actionable metric:
#' \deqn{Risk = P(activation) \times Impact \times CascadeFactor \times (1 + Uncertainty)}
#'
#' @examples
#' set.seed(42)
#' n <- 500
#' x <- rnorm(n)
#' z <- sample(c(0, 1), n, replace = TRUE)
#' y <- ifelse(z == 1, 0.8 * x + rnorm(sum(z), 0, 0.3), rnorm(n))
#' data <- data.frame(x = x, y = y, z = factor(z))
#'
#' result <- dormancy_detect(data, method = "conditional")
#' risk <- dormancy_risk(result, time_horizon = 2)
#' print(risk)
#'
#' @export
dormancy_risk <- function(dormancy_result, depth_result = NULL,
                          impact_weights = NULL, time_horizon = 1,
                          risk_tolerance = 0.3, verbose = FALSE) {
  
  if (!inherits(dormancy_result, "dormancy")) {
    stop("dormancy_result must be an object of class 'dormancy'")
  }
  
  if (nrow(dormancy_result$patterns) == 0) {
    warning("No dormant patterns to assess risk for.")
    return(list(
      risk_scores = data.frame(),
      risk_matrix = NULL,
      priorities = character(),
      recommendations = "No dormant patterns detected.",
      summary = NULL
    ))
  }
  
  if (verbose) message("Assessing risk for ", nrow(dormancy_result$patterns), " patterns...")
  
  patterns <- dormancy_result$patterns
  data <- dormancy_result$data
  
  # Initialize risk scores
  risk_scores <- data.frame(
    pattern_id = seq_len(nrow(patterns)),
    variable_1 = patterns$variable_1,
    variable_2 = patterns$variable_2,
    activation_prob = numeric(nrow(patterns)),
    impact_magnitude = numeric(nrow(patterns)),
    impact_direction = character(nrow(patterns)),
    cascade_potential = numeric(nrow(patterns)),
    uncertainty = numeric(nrow(patterns)),
    risk_score = numeric(nrow(patterns)),
    risk_category = character(nrow(patterns)),
    stringsAsFactors = FALSE
  )
  
  # Compute risk components for each pattern
  for (i in seq_len(nrow(patterns))) {
    pattern <- patterns[i, ]
    v1 <- pattern$variable_1
    v2 <- pattern$variable_2
    
    x <- data[[v1]]
    y <- data[[v2]]
    
    # Activation probability (adjusted by time horizon)
    base_prob <- pattern$activation_risk
    if (is.na(base_prob)) base_prob <- 0.5
    activation_prob <- 1 - (1 - base_prob)^time_horizon
    risk_scores$activation_prob[i] <- activation_prob
    
    # Impact magnitude
    impact_mag <- compute_impact_magnitude(x, y, pattern, data)
    risk_scores$impact_magnitude[i] <- impact_mag
    
    # Impact direction
    impact_dir <- determine_impact_direction(x, y, pattern, data)
    risk_scores$impact_direction[i] <- impact_dir
    
    # Cascade potential
    cascade <- compute_cascade_potential(i, patterns, data)
    risk_scores$cascade_potential[i] <- cascade
    
    # Uncertainty (higher for deeper dormancy)
    if (!is.null(depth_result) && i <= nrow(depth_result$depths)) {
      uncertainty <- depth_result$depths$combined_depth[i] * 0.5
    } else {
      uncertainty <- 0.3
    }
    risk_scores$uncertainty[i] <- uncertainty
    
    # Composite risk score
    risk <- activation_prob * impact_mag * (1 + cascade) * (1 + uncertainty)
    risk_scores$risk_score[i] <- risk
    
    # Risk category
    risk_scores$risk_category[i] <- categorize_risk(risk, risk_tolerance)
  }
  
  # Create risk matrix
  risk_matrix <- create_risk_matrix(risk_scores)
  
  # Order by risk score
  priorities <- risk_scores[order(-risk_scores$risk_score),
                            c("pattern_id", "variable_1", "variable_2",
                              "risk_score", "risk_category")]
  
  # Generate recommendations
  recommendations <- generate_risk_recommendations(risk_scores, risk_tolerance)
  
  # Summary statistics
  summary <- list(
    n_patterns = nrow(patterns),
    n_high_risk = sum(risk_scores$risk_category == "High"),
    n_medium_risk = sum(risk_scores$risk_category == "Medium"),
    n_low_risk = sum(risk_scores$risk_category == "Low"),
    mean_risk = mean(risk_scores$risk_score, na.rm = TRUE),
    max_risk = max(risk_scores$risk_score, na.rm = TRUE),
    time_horizon = time_horizon,
    risk_tolerance = risk_tolerance
  )
  
  result <- list(
    risk_scores = risk_scores,
    risk_matrix = risk_matrix,
    priorities = priorities,
    recommendations = recommendations,
    summary = summary,
    time_horizon = time_horizon,
    risk_tolerance = risk_tolerance
  )
  
  class(result) <- "dormancy_risk"
  return(result)
}


#' Compute impact magnitude
#' @keywords internal
compute_impact_magnitude <- function(x, y, pattern, data) {
  
  # Impact magnitude based on potential correlation strength
  # and the scale of the variables
  
  potential_cor <- pattern$dormancy_score
  
  # Also consider variable scales
  x_scale <- sd(x, na.rm = TRUE)
  y_scale <- sd(y, na.rm = TRUE)
  
  # Normalized impact
  impact <- min(potential_cor * sqrt(x_scale * y_scale), 1)
  
  return(max(0, impact))
}


#' Determine impact direction
#' @keywords internal
determine_impact_direction <- function(x, y, pattern, data) {
  
  # Look at the sign of the potential correlation
  trigger_var <- pattern$trigger_variable
  trigger_region <- pattern$trigger_region
  
  # Estimate the sign in the trigger region
  if (trigger_var %in% colnames(data)) {
    z <- data[[trigger_var]]
  } else {
    z <- x  # Self-triggered
  }
  
  # Simple heuristic: positive if variables increase together in trigger region
  overall_cor <- cor(x, y, use = "complete.obs")
  
  if (is.na(overall_cor)) return("Unknown")
  
  if (overall_cor > 0.1) {
    return("Positive")
  } else if (overall_cor < -0.1) {
    return("Negative")
  } else {
    return("Mixed/Neutral")
  }
}


#' Compute cascade potential
#' @keywords internal
compute_cascade_potential <- function(pattern_idx, patterns, data) {
  
  # How many other patterns could this one trigger?
  current_pattern <- patterns[pattern_idx, ]
  
  v1 <- current_pattern$variable_1
  v2 <- current_pattern$variable_2
  trigger_var <- current_pattern$trigger_variable
  
  # Count patterns that share variables
  shared_vars <- 0
  for (i in seq_len(nrow(patterns))) {
    if (i == pattern_idx) next
    
    other_v1 <- patterns$variable_1[i]
    other_v2 <- patterns$variable_2[i]
    
    if (v1 %in% c(other_v1, other_v2) || v2 %in% c(other_v1, other_v2)) {
      shared_vars <- shared_vars + 1
    }
    if (trigger_var %in% c(other_v1, other_v2, patterns$trigger_variable[i])) {
      shared_vars <- shared_vars + 0.5
    }
  }
  
  # Normalize to [0, 1]
  cascade <- min(shared_vars / max(1, nrow(patterns) - 1) * 2, 1)
  
  return(cascade)
}


#' Categorize risk level
#' @keywords internal
categorize_risk <- function(risk_score, tolerance) {
  
  if (risk_score >= tolerance * 2) {
    return("High")
  } else if (risk_score >= tolerance) {
    return("Medium")
  } else {
    return("Low")
  }
}


#' Create risk matrix
#' @keywords internal
create_risk_matrix <- function(risk_scores) {
  
  # Create a 2D matrix of activation probability vs impact
  n_bins <- 5
  
  risk_scores$prob_bin <- cut(risk_scores$activation_prob,
                               breaks = seq(0, 1, length.out = n_bins + 1),
                               labels = FALSE, include.lowest = TRUE)
  risk_scores$impact_bin <- cut(risk_scores$impact_magnitude,
                                breaks = seq(0, 1, length.out = n_bins + 1),
                                labels = FALSE, include.lowest = TRUE)
  
  matrix <- table(risk_scores$prob_bin, risk_scores$impact_bin)
  
  return(matrix)
}


#' Generate risk recommendations
#' @keywords internal
generate_risk_recommendations <- function(risk_scores, tolerance) {
  
  recommendations <- character()
  
  # High risk patterns
  high_risk <- risk_scores[risk_scores$risk_category == "High", ]
  if (nrow(high_risk) > 0) {
    recommendations <- c(recommendations,
                         paste0("CRITICAL: ", nrow(high_risk),
                                " high-risk dormant pattern(s) detected. Immediate monitoring required."))
    for (i in seq_len(min(3, nrow(high_risk)))) {
      recommendations <- c(recommendations,
                           paste0("  - Monitor relationship between ",
                                  high_risk$variable_1[i], " and ",
                                  high_risk$variable_2[i]))
    }
  }
  
  # Cascade warnings
  cascade_patterns <- risk_scores[risk_scores$cascade_potential > 0.5, ]
  if (nrow(cascade_patterns) > 0) {
    recommendations <- c(recommendations,
                         paste0("CASCADE WARNING: ", nrow(cascade_patterns),
                                " pattern(s) with high cascade potential. ",
                                "Activation could trigger chain reactions."))
  }
  
  # Directional warnings
  negative_impact <- risk_scores[risk_scores$impact_direction == "Negative", ]
  if (nrow(negative_impact) > 0) {
    recommendations <- c(recommendations,
                         paste0("NEGATIVE IMPACT: ", nrow(negative_impact),
                                " pattern(s) would have negative effects if activated."))
  }
  
  # Uncertainty warnings
  uncertain <- risk_scores[risk_scores$uncertainty > 0.4, ]
  if (nrow(uncertain) > 0) {
    recommendations <- c(recommendations,
                         paste0("UNCERTAINTY: ", nrow(uncertain),
                                " pattern(s) have high uncertainty in risk estimates. ",
                                "Consider additional data collection."))
  }
  
  if (length(recommendations) == 0) {
    recommendations <- "No immediate risk concerns. Continue routine monitoring."
  }
  
  return(recommendations)
}


#' @export
print.dormancy_risk <- function(x, ...) {
  cat("Dormancy Risk Assessment\n")
  cat("========================\n\n")
  
  cat("Time Horizon:", x$time_horizon, "\n")
  cat("Risk Tolerance:", x$risk_tolerance, "\n\n")
  
  cat("Risk Summary:\n")
  cat("  Total patterns:", x$summary$n_patterns, "\n")
  cat("  High risk:", x$summary$n_high_risk, "\n")
  cat("  Medium risk:", x$summary$n_medium_risk, "\n")
  cat("  Low risk:", x$summary$n_low_risk, "\n")
  cat("  Mean risk score:", round(x$summary$mean_risk, 3), "\n\n")
  
  if (nrow(x$priorities) > 0) {
    cat("Top Risk Priorities:\n")
    print(head(x$priorities, 10))
    cat("\n")
  }
  
  cat("Recommendations:\n")
  for (rec in x$recommendations) {
    cat("  -", rec, "\n")
  }
  
  invisible(x)
}
