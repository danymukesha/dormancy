#' @title Scout for Dormant Pattern Regions in Data
#' @name dormancy_scout
#' @description
#' Systematically scans the data space to identify regions where dormant patterns
#' might emerge. Unlike \code{\link{dormancy_detect}} which identifies specific patterns,
#' \code{dormancy_scout} maps the "terrain" of dormancy potential.
#'
#' @param data A numeric matrix or data frame.
#' @param grid_resolution Integer. Resolution of the scanning grid. Higher values
#'   give finer resolution but slower computation. Default is 20.
#' @param scout_method Character. Scanning method:
#'   \itemize{
#'     \item "density" - Identifies low-density regions where patterns might hide
#'     \item "variance" - Identifies high-variance regions with pattern potential
#'     \item "correlation" - Maps local correlation landscapes
#'     \item "entropy" - Identifies high-entropy regions with dormancy potential
#'   }
#'   Default is "density".
#' @param return_map Logical. Whether to return the full dormancy map. Default is TRUE.
#' @param verbose Logical. Whether to print progress messages. Default is FALSE.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{scout_results} - Data frame with coordinates and dormancy potential
#'   \item \code{hotspots} - Regions with highest dormancy potential
#'   \item \code{dormancy_map} - If \code{return_map = TRUE}, a matrix representing
#'     the dormancy landscape
#'   \item \code{summary} - Summary statistics of the scan
#' }
#'
#' @details
#' Scout analysis is useful for:
#'
#' \itemize{
#'   \item Identifying regions to monitor for future pattern emergence
#'   \item Understanding the "geography" of your data's pattern space
#'   \item Finding data regions that are underexplored or anomalous
#'   \item Planning targeted data collection in high-potential regions
#' }
#'
#' The scout creates a map of "dormancy potential" - not actual patterns, but
#' locations where patterns are more likely to exist or emerge.
#'
#' @examples
#' set.seed(42)
#' n <- 500
#' x <- rnorm(n)
#' y <- rnorm(n)
#' # Create a region with hidden pattern
#' z <- ifelse(x > 1 & y > 1, 0.9 * x + rnorm(sum(x > 1 & y > 1), 0, 0.1), y)
#' data <- data.frame(x = x, y = z)
#'
#' scout <- dormancy_scout(data, grid_resolution = 15)
#' print(scout)
#'
#' @export
dormancy_scout <- function(data, grid_resolution = 20, scout_method = "density",
                           return_map = TRUE, verbose = FALSE) {
  
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("data must be a data frame or matrix")
  }
  
  scout_method <- match.arg(scout_method,
                            c("density", "variance", "correlation", "entropy"))
  
  data <- as.data.frame(data)
  numeric_cols <- sapply(data, is.numeric)
  num_data <- data[, numeric_cols, drop = FALSE]
  num_data <- na.omit(num_data)
  
  if (ncol(num_data) < 2) {
    stop("Need at least 2 numeric variables for scouting")
  }
  
  if (verbose) message("Scouting data space using method: ", scout_method)
  
  # For simplicity, focus on 2D projections
  var_pairs <- combn(colnames(num_data), 2, simplify = FALSE)
  
  all_results <- list()
  all_maps <- list()
  
  for (pair in var_pairs) {
    v1 <- pair[1]
    v2 <- pair[2]
    
    x <- num_data[[v1]]
    y <- num_data[[v2]]
    
    # Create grid
    x_range <- range(x, na.rm = TRUE)
    y_range <- range(y, na.rm = TRUE)
    
    x_grid <- seq(x_range[1], x_range[2], length.out = grid_resolution)
    y_grid <- seq(y_range[1], y_range[2], length.out = grid_resolution)
    
    # Compute dormancy potential at each grid point
    dormancy_map <- matrix(0, nrow = grid_resolution, ncol = grid_resolution)
    
    for (i in seq_along(x_grid)) {
      for (j in seq_along(y_grid)) {
        dormancy_map[i, j] <- compute_dormancy_potential(
          x, y, x_grid[i], y_grid[j], scout_method
        )
      }
    }
    
    # Find hotspots
    threshold <- quantile(dormancy_map, 0.9, na.rm = TRUE)
    hotspot_idx <- which(dormancy_map > threshold, arr.ind = TRUE)
    
    hotspots <- data.frame(
      x_coord = x_grid[hotspot_idx[, 1]],
      y_coord = y_grid[hotspot_idx[, 2]],
      potential = dormancy_map[hotspot_idx]
    )
    
    all_results[[paste(v1, v2, sep = "~")]] <- list(
      variables = c(v1, v2),
      hotspots = hotspots,
      max_potential = max(dormancy_map, na.rm = TRUE),
      mean_potential = mean(dormancy_map, na.rm = TRUE)
    )
    
    if (return_map) {
      all_maps[[paste(v1, v2, sep = "~")]] <- list(
        map = dormancy_map,
        x_grid = x_grid,
        y_grid = y_grid,
        variables = c(v1, v2)
      )
    }
  }
  
  # Compile summary
  summary_df <- data.frame(
    variable_pair = names(all_results),
    max_potential = sapply(all_results, function(x) x$max_potential),
    mean_potential = sapply(all_results, function(x) x$mean_potential),
    n_hotspots = sapply(all_results, function(x) nrow(x$hotspots))
  )
  
  result <- list(
    scout_results = summary_df,
    detailed_results = all_results,
    hotspots = do.call(rbind, lapply(names(all_results), function(nm) {
      df <- all_results[[nm]]$hotspots
      if (nrow(df) > 0) {
        df$variable_pair <- nm
      }
      return(df)
    })),
    dormancy_map = all_maps,
    method = scout_method,
    grid_resolution = grid_resolution
  )
  
  class(result) <- "dormancy_map"
  return(result)
}


#' Compute dormancy potential at a grid point
#' @keywords internal
compute_dormancy_potential <- function(x, y, x_center, y_center, method) {
  
  # Find nearby points
  bandwidth_x <- diff(range(x, na.rm = TRUE)) / 10
  bandwidth_y <- diff(range(y, na.rm = TRUE)) / 10
  
  nearby_idx <- (abs(x - x_center) < bandwidth_x) & (abs(y - y_center) < bandwidth_y)
  n_nearby <- sum(nearby_idx)
  
  if (n_nearby < 5) {
    return(0.5)  # Unknown potential in sparse regions
  }
  
  x_near <- x[nearby_idx]
  y_near <- y[nearby_idx]
  
  if (method == "density") {
    # Low density regions have higher dormancy potential
    potential <- 1 - n_nearby / length(x)
  } else if (method == "variance") {
    # High variance regions have higher potential
    potential <- var(x_near, na.rm = TRUE) * var(y_near, na.rm = TRUE)
    potential <- min(potential, 1)
  } else if (method == "correlation") {
    # Weak correlation regions have higher potential
    local_cor <- cor(x_near, y_near, use = "complete.obs")
    potential <- 1 - abs(local_cor)
  } else {  # entropy
    # Higher entropy = higher potential
    potential <- compute_local_entropy(x_near, y_near)
  }
  
  return(potential)
}


#' Compute local entropy
#' @keywords internal
compute_local_entropy <- function(x, y) {
  
  # Discretize
  n_bins <- 5
  x_bins <- cut(x, breaks = n_bins, labels = FALSE, include.lowest = TRUE)
  y_bins <- cut(y, breaks = n_bins, labels = FALSE, include.lowest = TRUE)
  
  # Joint distribution
  joint_table <- table(x_bins, y_bins)
  joint_prob <- joint_table / sum(joint_table)
  
  # Entropy
  entropy <- -sum(joint_prob * log2(joint_prob + 1e-10))
  
  # Normalize
  max_entropy <- log2(n_bins^2)
  
  return(entropy / max_entropy)
}


#' @export
print.dormancy_map <- function(x, ...) {
  cat("Dormancy Scout Results\n")
  cat("======================\n\n")
  
  cat("Method:", x$method, "\n")
  cat("Grid resolution:", x$grid_resolution, "\n\n")
  
  cat("Variable Pair Analysis:\n")
  print(x$scout_results)
  
  if (nrow(x$hotspots) > 0) {
    cat("\nTop Dormancy Hotspots:\n")
    print(head(x$hotspots[order(-x$hotspots$potential), ], 10))
  }
  
  invisible(x)
}
