#' @title Plot Methods for Dormancy Objects
#' @name plot.dormancy
#' @description
#' Visualization methods for dormancy analysis results. Creates informative
#' plots showing dormant patterns, trigger regions, and risk landscapes.
#'
#' @param x An object of class "dormancy", "dormancy_map", "dormancy_depth",
#'   "dormancy_risk", or "awakening".
#' @param type Character. Type of plot to generate:
#'   \itemize{
#'     \item "overview" - General overview of results
#'     \item "patterns" - Focus on detected patterns
#'     \item "risk" - Risk-focused visualization
#'     \item "timeline" - Time-based visualization (if applicable)
#'   }
#' @param ... Additional arguments passed to plot functions.
#'
#' @return Invisibly returns the plot data.
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
#' \donttest{plot(result)}
#'
#' @export
plot.dormancy <- function(x, type = "overview", ...) {
  
  if (!inherits(x, "dormancy")) {
    stop("x must be a dormancy object")
  }
  
  type <- match.arg(type, c("overview", "patterns", "risk", "timeline"))
  
  if (nrow(x$patterns) == 0) {
    plot.new()
    text(0.5, 0.5, "No dormant patterns detected", cex = 1.5)
    return(invisible(NULL))
  }
  
  if (type == "overview") {
    plot_dormancy_overview(x, ...)
  } else if (type == "patterns") {
    plot_dormancy_patterns(x, ...)
  } else if (type == "risk") {
    plot_dormancy_risk(x, ...)
  } else {
    plot_dormancy_timeline(x, ...)
  }
  
  invisible(x$patterns)
}


#' Plot dormancy overview
#' @keywords internal
plot_dormancy_overview <- function(x, ...) {
  
  patterns <- x$patterns
  
  # Set up plot layout
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  par(mfrow = c(2, 2))
  
  # 1. Dormancy score distribution
  hist(patterns$dormancy_score, main = "Dormancy Score Distribution",
       xlab = "Dormancy Score", col = "steelblue", border = "white",
       breaks = 20)
  abline(v = x$threshold, col = "red", lwd = 2, lty = 2)
  
  # 2. Activation risk vs dormancy score
  plot(patterns$dormancy_score, patterns$activation_risk,
       xlab = "Dormancy Score", ylab = "Activation Risk",
       main = "Dormancy vs Activation Risk",
       pch = 19, col = rgb(0, 0, 0.8, 0.6))
  
  # 3. Top patterns by dormancy score
  top_patterns <- head(patterns[order(-patterns$dormancy_score), ], 10)
  barplot(top_patterns$dormancy_score,
          names.arg = paste(top_patterns$variable_1, top_patterns$variable_2, sep = "~"),
          las = 2, cex.names = 0.7,
          main = "Top Dormant Patterns",
          col = "darkorange", border = NA)
  
  # 4. Trigger variable frequency
  trigger_counts <- table(patterns$trigger_variable)
  if (length(trigger_counts) > 0) {
    barplot(sort(trigger_counts, decreasing = TRUE),
            las = 2, cex.names = 0.7,
            main = "Trigger Variables",
            col = "forestgreen", border = NA)
  }
}


#' Plot dormancy patterns
#' @keywords internal
plot_dormancy_patterns <- function(x, ...) {
  
  patterns <- x$patterns
  data <- x$data
  
  # Create network-style plot of patterns
  all_vars <- unique(c(patterns$variable_1, patterns$variable_2))
  n_vars <- length(all_vars)
  
  # Position variables in a circle
  angles <- seq(0, 2 * pi, length.out = n_vars + 1)[1:n_vars]
  x_pos <- cos(angles)
  y_pos <- sin(angles)
  
  plot.new()
  plot.window(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
  
  # Draw connections
  for (i in seq_len(nrow(patterns))) {
    v1_idx <- which(all_vars == patterns$variable_1[i])
    v2_idx <- which(all_vars == patterns$variable_2[i])
    
    line_width <- patterns$dormancy_score[i] * 5
    line_col <- rgb(1, 0, 0, patterns$dormancy_score[i])
    
    lines(c(x_pos[v1_idx], x_pos[v2_idx]),
          c(y_pos[v1_idx], y_pos[v2_idx]),
          lwd = line_width, col = line_col)
  }
  
  # Draw nodes
  points(x_pos, y_pos, pch = 21, bg = "lightblue", cex = 3)
  text(x_pos * 1.2, y_pos * 1.2, all_vars, cex = 0.8)
  
  title(main = "Dormant Pattern Network")
}


#' Plot dormancy risk
#' @keywords internal
plot_dormancy_risk <- function(x, ...) {
  
  patterns <- x$patterns
  
  # Risk categories based on activation risk
  risk_cat <- cut(patterns$activation_risk,
                  breaks = c(0, 0.3, 0.6, 1),
                  labels = c("Low", "Medium", "High"),
                  include.lowest = TRUE)
  
  colors <- c("Low" = "green", "Medium" = "orange", "High" = "red")
  
  plot(patterns$dormancy_score, patterns$activation_risk,
       xlab = "Dormancy Score", ylab = "Activation Risk",
       main = "Dormancy Risk Matrix",
       pch = 19, col = colors[as.character(risk_cat)],
       cex = patterns$dormancy_score * 3 + 1)
  
  legend("topright", legend = names(colors), col = colors, pch = 19,
         title = "Risk Level")
  
  # Add threshold lines
  abline(h = c(0.3, 0.6), lty = 2, col = "gray")
  abline(v = x$threshold, lty = 2, col = "gray")
}


#' Plot dormancy timeline
#' @keywords internal
plot_dormancy_timeline <- function(x, ...) {
  
  # For timeline, we'd need time-based data
  # For now, show detection time and pattern summary
  
  patterns <- x$patterns
  
  plot(1:nrow(patterns), patterns$dormancy_score,
       type = "b", pch = 19,
       xlab = "Pattern Rank", ylab = "Dormancy Score",
       main = "Dormant Patterns by Rank",
       col = "purple")
  
  abline(h = x$threshold, col = "red", lty = 2)
}


#' @rdname plot.dormancy
#' @export
plot.dormancy_map <- function(x, type = "overview", ...) {
  
  if (!inherits(x, "dormancy_map")) {
    stop("x must be a dormancy_map object")
  }
  
  if (length(x$dormancy_map) == 0) {
    plot.new()
    text(0.5, 0.5, "No dormancy map available")
    return(invisible(NULL))
  }
  
  # Plot the first map as example
  map_data <- x$dormancy_map[[1]]
  
  image(map_data$x_grid, map_data$y_grid, map_data$map,
        col = rev(heat.colors(20)),
        xlab = map_data$variables[1],
        ylab = map_data$variables[2],
        main = "Dormancy Potential Map")
  
  contour(map_data$x_grid, map_data$y_grid, map_data$map, add = TRUE)
  
  # Mark hotspots
  if (nrow(x$hotspots) > 0) {
    relevant_hotspots <- x$hotspots[x$hotspots$variable_pair == names(x$dormancy_map)[1], ]
    if (nrow(relevant_hotspots) > 0) {
      points(relevant_hotspots$x_coord, relevant_hotspots$y_coord,
             pch = 19, col = "blue", cex = 2)
    }
  }
  
  invisible(x$dormancy_map)
}


#' @export
print.dormancy <- function(x, ...) {
  cat("Dormancy Detection Results\n")
  cat("==========================\n\n")
  
  cat("Method:", x$method, "\n")
  cat("Threshold:", x$threshold, "\n")
  cat("Observations:", x$metadata$n_observations, "\n")
  cat("Variables:", x$metadata$n_variables, "\n\n")
  
  cat("Patterns detected:", nrow(x$patterns), "\n")
  
  if (nrow(x$patterns) > 0) {
    cat("\nTop Dormant Patterns:\n")
    print(head(x$patterns[, c("variable_1", "variable_2", "dormancy_score",
                              "trigger_variable", "activation_risk")], 10))
  }
  
  invisible(x)
}


#' @export
summary.dormancy <- function(object, ...) {
  cat("Dormancy Analysis Summary\n")
  cat("=========================\n\n")
  
  cat("Detection Method:", object$method, "\n")
  cat("Detection Time:", format(object$metadata$detection_time), "\n\n")
  
  if (nrow(object$patterns) > 0) {
    cat("Pattern Statistics:\n")
    cat("  Mean dormancy score:", round(mean(object$patterns$dormancy_score), 3), "\n")
    cat("  Max dormancy score:", round(max(object$patterns$dormancy_score), 3), "\n")
    cat("  Mean activation risk:", round(mean(object$patterns$activation_risk, na.rm = TRUE), 3), "\n\n")
    
    cat("Top Trigger Variables:\n")
    trigger_table <- sort(table(object$patterns$trigger_variable), decreasing = TRUE)
    print(head(trigger_table, 5))
  } else {
    cat("No dormant patterns detected.\n")
  }
  
  invisible(object)
}
