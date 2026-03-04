#' @title Measure the Depth of Dormancy in Patterns
#' @name dormancy_depth
#' @description
#' Quantifies how "deeply asleep" a dormant pattern is - measuring the energy
#' required to activate it and the stability of its dormant state. Deeper
#' dormancy implies greater resistance to activation but potentially larger
#' effects when awakened.
#'
#' @param dormancy_result An object of class "dormancy" from \code{\link{dormancy_detect}}.
#' @param method Character. The depth measurement method:
#'   \itemize{
#'     \item "energy" - Measures the statistical "energy barrier" to activation
#'     \item "stability" - Measures how stable the dormant state is
#'     \item "entropy" - Measures information-theoretic dormancy depth
#'     \item "combined" - Weighted combination of all methods
#'   }
#'   Default is "combined".
#' @param normalize Logical. Whether to normalize depth scores to [0, 1]. Default is TRUE.
#' @param verbose Logical. Whether to print progress messages. Default is FALSE.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{depths} - Data frame with depth measurements for each pattern
#'   \item \code{depth_distribution} - Summary statistics of depth distribution
#'   \item \code{awakening_effort} - Estimated effort required to activate each pattern
#'   \item \code{stability_index} - Stability measure for each pattern's dormant state
#' }
#'
#' @details
#' Dormancy depth is a novel concept in statistical analysis, inspired by:
#'
#' \itemize{
#'   \item \strong{Physics}: Potential energy barriers in phase transitions
#'   \item \strong{Biology}: Depth of seed dormancy (stratification requirements)
#'   \item \strong{Geology}: Locked fault segments and earthquake potential
#' }
#'
#' A deeply dormant pattern:
#' \itemize{
#'   \item Requires significant change in conditions to activate
#'   \item Is stable against minor perturbations
#'   \item May have a larger effect when finally awakened
#'   \item Represents accumulated "potential energy" in the system
#' }
#'
#' The depth measurement helps prioritize which patterns to monitor and what
#' magnitude of change would be required to awaken them.
#'
#' @examples
#' set.seed(42)
#' n <- 500
#' x <- rnorm(n)
#' # Create a deeply dormant pattern (only active in extreme conditions)
#' z <- ifelse(abs(x) > 2, 1, 0)
#' y <- ifelse(z == 1, 0.9 * x + rnorm(sum(z), 0, 0.1), rnorm(n))
#' data <- data.frame(x = x, y = y)
#'
#' result <- dormancy_detect(data, method = "threshold")
#' depths <- dormancy_depth(result)
#' print(depths)
#'
#' @export
dormancy_depth <- function(dormancy_result, method = "combined",
                           normalize = TRUE, verbose = FALSE) {
  
  if (!inherits(dormancy_result, "dormancy")) {
    stop("dormancy_result must be an object of class 'dormancy'")
  }
  
  method <- match.arg(method, c("energy", "stability", "entropy", "combined"))
  
  if (nrow(dormancy_result$patterns) == 0) {
    warning("No dormant patterns to measure depth for.")
    return(list(
      depths = data.frame(),
      depth_distribution = NULL,
      awakening_effort = NULL,
      stability_index = NULL
    ))
  }
  
  if (verbose) message("Measuring dormancy depth using method: ", method)
  
  data <- dormancy_result$data
  patterns <- dormancy_result$patterns
  
  depths <- data.frame(
    pattern_id = seq_len(nrow(patterns)),
    variable_1 = patterns$variable_1,
    variable_2 = patterns$variable_2,
    energy_depth = numeric(nrow(patterns)),
    stability_depth = numeric(nrow(patterns)),
    entropy_depth = numeric(nrow(patterns)),
    combined_depth = numeric(nrow(patterns)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(patterns))) {
    pattern <- patterns[i, ]
    v1 <- pattern$variable_1
    v2 <- pattern$variable_2
    
    x <- data[[v1]]
    y <- data[[v2]]
    
    # Energy-based depth: how far from activation threshold?
    energy_depth <- compute_energy_depth(x, y, pattern, data)
    
    # Stability-based depth: how stable is the dormant state?
    stability_depth <- compute_stability_depth(x, y, pattern, data)
    
    # Entropy-based depth: information-theoretic measure
    entropy_depth <- compute_entropy_depth(x, y, pattern, data)
    
    depths$energy_depth[i] <- energy_depth
    depths$stability_depth[i] <- stability_depth
    depths$entropy_depth[i] <- entropy_depth
    depths$combined_depth[i] <- (energy_depth + stability_depth + entropy_depth) / 3
  }
  
  # Normalize if requested
  if (normalize) {
    for (col in c("energy_depth", "stability_depth", "entropy_depth", "combined_depth")) {
      min_val <- min(depths[[col]], na.rm = TRUE)
      max_val <- max(depths[[col]], na.rm = TRUE)
      if (max_val > min_val) {
        depths[[col]] <- (depths[[col]] - min_val) / (max_val - min_val)
      }
    }
  }
  
  # Compute depth distribution summary
  depth_dist <- list(
    mean = mean(depths$combined_depth, na.rm = TRUE),
    median = median(depths$combined_depth, na.rm = TRUE),
    sd = sd(depths$combined_depth, na.rm = TRUE),
    q25 = quantile(depths$combined_depth, 0.25, na.rm = TRUE),
    q75 = quantile(depths$combined_depth, 0.75, na.rm = TRUE),
    deep_patterns = sum(depths$combined_depth > 0.7, na.rm = TRUE),
    shallow_patterns = sum(depths$combined_depth < 0.3, na.rm = TRUE)
  )
  
  # Estimate awakening effort
  awakening_effort <- data.frame(
    pattern_id = depths$pattern_id,
    effort_score = 1 - depths$combined_depth,
    effort_category = cut(1 - depths$combined_depth,
                          breaks = c(-Inf, 0.3, 0.6, Inf),
                          labels = c("Low", "Medium", "High")),
    required_change = NA_character_
  )
  
  # Describe required change for awakening
  for (i in seq_len(nrow(awakening_effort))) {
    if (awakening_effort$effort_score[i] < 0.3) {
      awakening_effort$required_change[i] <- "Minor perturbation could activate"
    } else if (awakening_effort$effort_score[i] < 0.6) {
      awakening_effort$required_change[i] <- "Moderate change in trigger conditions needed"
    } else {
      awakening_effort$required_change[i] <- "Significant systemic shift required"
    }
  }
  
  # Stability index
  stability_index <- data.frame(
    pattern_id = depths$pattern_id,
    stability = depths$stability_depth,
    volatility_risk = 1 - depths$stability_depth,
    monitoring_priority = ifelse(depths$stability_depth > 0.7, "Low",
                                 ifelse(depths$stability_depth > 0.3, "Medium", "High"))
  )
  
  result <- list(
    depths = depths,
    method = method,
    depth_distribution = depth_dist,
    awakening_effort = awakening_effort,
    stability_index = stability_index,
    normalized = normalize
  )
  
  class(result) <- "dormancy_depth"
  return(result)
}


#' Compute energy-based depth
#' @keywords internal
compute_energy_depth <- function(x, y, pattern, data) {
  
  # Energy depth measures how far the current state is from activation
  # Higher = deeper dormancy = harder to activate
  
  overall_cor <- cor(x, y, use = "complete.obs")
  
  # Find maximum potential correlation
  trigger_region <- pattern$trigger_region
  trigger_var <- pattern$trigger_variable
  
  # Estimate the "energy barrier" based on the difference between
  # current correlation and potential correlation
  potential_cor <- pattern$dormancy_score + abs(overall_cor)
  
  # Energy depth is proportional to how suppressed the relationship is
  # and how rare the trigger condition is
  energy_barrier <- potential_cor^2 - overall_cor^2
  
  # Adjust by activation risk (lower risk = deeper dormancy)
  activation_risk <- pattern$activation_risk
  if (is.na(activation_risk)) activation_risk <- 0.5
  
  depth <- sqrt(max(0, energy_barrier)) * (1 - activation_risk)
  
  return(depth)
}


#' Compute stability-based depth
#' @keywords internal
compute_stability_depth <- function(x, y, pattern, data) {
  
  # Stability depth measures how stable the dormant state is
  # Higher = more stable = less likely to spontaneously activate
  
  # Add noise and see how stable the dormant state is
  n_sim <- 100
  cor_perturbed <- numeric(n_sim)
  
  for (i in seq_len(n_sim)) {
    # Add small perturbations
    x_perturbed <- x + rnorm(length(x), 0, 0.1 * sd(x, na.rm = TRUE))
    y_perturbed <- y + rnorm(length(y), 0, 0.1 * sd(y, na.rm = TRUE))
    
    cor_perturbed[i] <- cor(x_perturbed, y_perturbed, use = "complete.obs")
  }
  
  # Stability = low variance under perturbation
  stability <- 1 - sd(cor_perturbed, na.rm = TRUE)
  
  # Also consider how "balanced" the dormant state is
  overall_cor <- cor(x, y, use = "complete.obs")
  balance <- 1 - abs(overall_cor)  # Near-zero correlation = more balanced
  
  depth <- stability * balance
  return(max(0, min(1, depth)))
}


#' Compute entropy-based depth
#' @keywords internal
compute_entropy_depth <- function(x, y, pattern, data) {
  
  # Entropy-based depth uses information theory
  # Higher entropy in the relationship = deeper dormancy
  
  # Discretize for entropy calculation
  n_bins <- 10
  x_bins <- cut(x, breaks = n_bins, labels = FALSE)
  y_bins <- cut(y, breaks = n_bins, labels = FALSE)
  
  # Joint distribution
  joint_table <- table(x_bins, y_bins)
  joint_prob <- joint_table / sum(joint_table)
  
  # Marginal distributions
  x_prob <- rowSums(joint_table) / sum(joint_table)
  y_prob <- colSums(joint_table) / sum(joint_table)
  
  # Mutual information
  mi <- 0
  for (i in seq_len(nrow(joint_table))) {
    for (j in seq_len(ncol(joint_table))) {
      if (joint_prob[i, j] > 0 && x_prob[i] > 0 && y_prob[j] > 0) {
        mi <- mi + joint_prob[i, j] * log2(joint_prob[i, j] / (x_prob[i] * y_prob[j]))
      }
    }
  }
  
  # Entropy depth: higher when MI is low but conditional MI can be high
  # This indicates information is "locked" in conditional relationships
  
  # Normalize MI
  max_mi <- log2(min(n_bins, n_bins))
  normalized_mi <- mi / max_mi
  
  # Depth is inverse of current MI but accounts for potential
  depth <- (1 - normalized_mi) * pattern$dormancy_score
  
  return(max(0, min(1, depth)))
}


#' @export
print.dormancy_depth <- function(x, ...) {
  cat("Dormancy Depth Analysis\n")
  cat("=======================\n\n")
  
  cat("Method:", x$method, "\n")
  cat("Normalized:", x$normalized, "\n\n")
  
  if (nrow(x$depths) > 0) {
    cat("Depth Distribution:\n")
    cat("  Mean depth:", round(x$depth_distribution$mean, 3), "\n")
    cat("  Median depth:", round(x$depth_distribution$median, 3), "\n")
    cat("  SD:", round(x$depth_distribution$sd, 3), "\n")
    cat("  Deep patterns (>0.7):", x$depth_distribution$deep_patterns, "\n")
    cat("  Shallow patterns (<0.3):", x$depth_distribution$shallow_patterns, "\n\n")
    
    cat("Depth by Pattern:\n")
    print(x$depths[, c("variable_1", "variable_2", "combined_depth")])
  }
  
  invisible(x)
}
