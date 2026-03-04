#' @title Simulate Awakening of Dormant Patterns
#' @name awaken
#' @description
#' Simulates what would happen if a dormant pattern were to "awaken" - i.e.,
#' become active. This function allows exploration of potential future states
#' and scenario analysis without waiting for actual pattern activation.
#'
#' @param dormancy_result An object of class "dormancy" from \code{\link{dormancy_detect}}.
#' @param pattern_id Integer or "all". Which pattern(s) to awaken. Default is 1.
#' @param intensity Numeric. Intensity of awakening, from 0 (dormant) to 1 (fully
#'   active). Default is 1.
#' @param n_sim Integer. Number of simulation runs. Default is 100.
#' @param return_data Logical. Whether to return simulated data. Default is FALSE.
#' @param verbose Logical. Whether to print progress messages. Default is FALSE.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{awakening_effects} - Data frame describing the effects of awakening
#'   \item \code{simulated_stats} - Summary statistics from simulations
#'   \item \code{cascade_effects} - Effects on other patterns (if any)
#'   \item \code{simulated_data} - If \code{return_data = TRUE}, simulated datasets
#' }
#'
#' @details
#' Awakening simulation is valuable for:
#'
#' \itemize{
#'   \item Scenario planning and stress testing
#'   \item Understanding potential system behaviors
#'   \item Preparing for pattern activation events
#'   \item Testing the robustness of current strategies
#' }
#'
#' The simulation works by:
#' \enumerate{
#'   \item Identifying the dormant pattern's trigger conditions
#'   \item Simulating data where those conditions are met
#'   \item Applying the pattern's relationship to the simulated data
#'   \item Measuring the resulting effects on the system
#' }
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
#' awakening <- awaken(result, pattern_id = 1, n_sim = 50)
#' print(awakening)
#'
#' @export
awaken <- function(dormancy_result, pattern_id = 1, intensity = 1,
                   n_sim = 100, return_data = FALSE, verbose = FALSE) {
  
  if (!inherits(dormancy_result, "dormancy")) {
    stop("dormancy_result must be an object of class 'dormancy'")
  }
  
  if (nrow(dormancy_result$patterns) == 0) {
    warning("No dormant patterns to awaken.")
    return(list(
      awakening_effects = data.frame(),
      simulated_stats = NULL,
      cascade_effects = NULL
    ))
  }
  
  if (pattern_id == "all") {
    pattern_ids <- seq_len(nrow(dormancy_result$patterns))
  } else {
    pattern_ids <- pattern_id
    if (!all(pattern_ids %in% seq_len(nrow(dormancy_result$patterns)))) {
      stop("Invalid pattern_id. Must be between 1 and ",
           nrow(dormancy_result$patterns), " or 'all'.")
    }
  }
  
  if (verbose) message("Simulating awakening for ", length(pattern_ids), " pattern(s)...")
  
  data <- dormancy_result$data
  patterns <- dormancy_result$patterns
  
  awakening_effects <- list()
  simulated_stats <- list()
  cascade_effects <- list()
  simulated_data_list <- list()
  
  for (pid in pattern_ids) {
    if (verbose) message("  Pattern ", pid)
    
    pattern <- patterns[pid, ]
    v1 <- pattern$variable_1
    v2 <- pattern$variable_2
    
    x <- data[[v1]]
    y <- data[[v2]]
    
    # Original statistics
    original_cor <- cor(x, y, use = "complete.obs")
    original_mean_x <- mean(x, na.rm = TRUE)
    original_mean_y <- mean(y, na.rm = TRUE)
    original_sd_x <- sd(x, na.rm = TRUE)
    original_sd_y <- sd(y, na.rm = TRUE)
    
    # Simulate awakening
    sim_cors <- numeric(n_sim)
    sim_means_y <- numeric(n_sim)
    sim_sds_y <- numeric(n_sim)
    
    # Determine potential correlation from dormancy score
    potential_cor <- min(pattern$dormancy_score + abs(original_cor), 0.99)
    
    for (sim in seq_len(n_sim)) {
      # Simulate data with awakened pattern
      x_sim <- rnorm(length(x), original_mean_x, original_sd_x)
      
      # Apply the dormant relationship
      noise_sd <- original_sd_y * sqrt(1 - potential_cor^2) * (1 - intensity * 0.5)
      y_base <- original_mean_y + potential_cor * intensity * (x_sim - original_mean_x) * original_sd_y / original_sd_x
      y_sim <- y_base + rnorm(length(x_sim), 0, noise_sd)
      
      sim_cors[sim] <- cor(x_sim, y_sim)
      sim_means_y[sim] <- mean(y_sim)
      sim_sds_y[sim] <- sd(y_sim)
      
      if (return_data && sim == 1) {
        simulated_data_list[[paste0("pattern_", pid)]] <- data.frame(
          x_sim = x_sim,
          y_sim = y_sim
        )
        names(simulated_data_list[[paste0("pattern_", pid)]]) <- c(v1, v2)
      }
    }
    
    awakening_effects[[pid]] <- data.frame(
      pattern_id = pid,
      variable_1 = v1,
      variable_2 = v2,
      original_correlation = original_cor,
      awakened_correlation = mean(sim_cors),
      correlation_change = mean(sim_cors) - original_cor,
      intensity = intensity,
      effect_size = abs(mean(sim_cors) - original_cor)
    )
    
    simulated_stats[[pid]] <- list(
      correlation_dist = quantile(sim_cors, probs = c(0.025, 0.5, 0.975)),
      mean_y_dist = quantile(sim_means_y, probs = c(0.025, 0.5, 0.975)),
      sd_y_dist = quantile(sim_sds_y, probs = c(0.025, 0.5, 0.975))
    )
    
    # Check for cascade effects on other patterns
    if (length(pattern_ids) > 1 || pattern_id == "all") {
      other_patterns <- patterns[-pid, , drop = FALSE]
      if (nrow(other_patterns) > 0) {
        cascade_risk <- sum(other_patterns$variable_1 %in% c(v1, v2) |
                              other_patterns$variable_2 %in% c(v1, v2))
        cascade_effects[[pid]] <- data.frame(
          affected_patterns = cascade_risk,
          cascade_probability = min(cascade_risk / nrow(other_patterns), 1)
        )
      }
    }
  }
  
  # Compile results
  awakening_df <- do.call(rbind, awakening_effects)
  
  result <- list(
    awakening_effects = awakening_df,
    simulated_stats = simulated_stats,
    cascade_effects = cascade_effects,
    intensity = intensity,
    n_sim = n_sim
  )
  
  if (return_data) {
    result$simulated_data <- simulated_data_list
  }
  
  class(result) <- "awakening"
  return(result)
}


#' @export
print.awakening <- function(x, ...) {
  cat("Dormancy Awakening Simulation\n")
  cat("=============================\n\n")
  
  cat("Intensity:", x$intensity, "\n")
  cat("Simulations:", x$n_sim, "\n\n")
  
  if (nrow(x$awakening_effects) > 0) {
    cat("Awakening Effects:\n")
    print(x$awakening_effects)
    cat("\n")
    
    if (length(x$cascade_effects) > 0) {
      cat("Cascade Effects:\n")
      for (nm in names(x$cascade_effects)) {
        cat("  Pattern", nm, ":\n")
        print(x$cascade_effects[[nm]])
      }
    }
  }
  
  invisible(x)
}
