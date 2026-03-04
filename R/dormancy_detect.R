#' @title Detect Dormant Patterns in Multivariate Data
#' @name dormancy_detect
#' @description
#' Identifies dormant patterns in multivariate data - statistical relationships
#' that exist but are currently inactive. Dormant patterns only manifest when
#' specific trigger conditions emerge in the data.
#'
#' @param data A numeric matrix or data frame with observations in rows and
#'   variables in columns.
#' @param threshold Numeric. The minimum dormancy score for a pattern to be
#'   considered significant. Default is 0.3.
#' @param method Character. The detection method to use. Options are:
#'   \itemize{
#'     \item "conditional" - Detects patterns that are conditionally suppressed
#'       (active only under specific conditions)
#'     \item "threshold" - Detects patterns that emerge when variables cross
#'       certain thresholds
#'     \item "phase" - Detects patterns that exist in specific phase regions
#'       of the data space
#'     \item "cascade" - Detects cascade-ready patterns that could trigger
#'       chain reactions
#'   }
#'   Default is "conditional".
#' @param n_bins Integer. Number of bins for discretizing continuous variables
#'   when searching for conditional patterns. Default is 10.
#' @param min_cluster Numeric. Minimum proportion of observations that must be
#'   in a region for it to be considered. Default is 0.05 (5\%).
#' @param parallel Logical. Whether to use parallel processing. Default is FALSE.
#' @param verbose Logical. Whether to print progress messages. Default is FALSE.
#'
#' @return A list of class "dormancy" containing:
#' \itemize{
#'   \item \code{patterns} - A data frame of detected dormant patterns with columns:
#'     variables, dormancy_score, trigger_variable, trigger_region, activation_risk
#'   \item \code{data} - The input data
#'   \item \code{method} - The detection method used
#'   \item \code{threshold} - The threshold used
#'   \item \code{conditional_regions} - List of regions where patterns are dormant
#'   \item \code{metadata} - Additional information about the detection process
#' }
#'
#' @details
#' Dormant patterns are fundamentally different from weak or spurious correlations.
#' A dormant pattern is a genuine relationship that is currently suppressed by
#' prevailing conditions in the data. The key insight is that:
#'
#' \enumerate{
#'   \item The pattern exists (verified through subset analysis)
#'   \item The pattern is currently inactive (not visible in aggregate statistics)
#'   \item The pattern can "awaken" when conditions change
#' }
#'
#' The detection algorithm works by:
#' \enumerate{
#'   \item Segmenting the data space into regions
#'   \item Computing pairwise relationships in each region
#'   \item Identifying relationships that vary significantly across regions
#'   \item Flagging relationships that are strong in some regions but weak overall
#'   \item Estimating the conditions under which dormant patterns would activate
#' }
#'
#' @seealso
#' \code{\link{dormancy_trigger}} for identifying activation triggers,
#' \code{\link{dormancy_depth}} for measuring dormancy depth,
#' \code{\link{awaken}} for simulating pattern activation
#'
#' @examples
#' # Create data with a dormant pattern
#' set.seed(42)
#' n <- 1000
#' x <- rnorm(n)
#' z <- sample(c(0, 1), n, replace = TRUE)
#' # Relationship between x and y only exists when z == 1
#' y <- ifelse(z == 1, 0.8 * x + rnorm(n, 0, 0.3), rnorm(n))
#' data <- data.frame(x = x, y = y, z = factor(z))
#'
#' # Detect dormant patterns
#' result <- dormancy_detect(data, method = "conditional")
#' print(result)
#'
#' @export
dormancy_detect <- function(data, threshold = 0.3, method = "conditional",
                            n_bins = 10, min_cluster = 0.05,
                            parallel = FALSE, verbose = FALSE) {
    # Input validation
    if (!is.data.frame(data) && !is.matrix(data)) {
        stop("data must be a data frame or matrix")
    }

    data <- as.data.frame(data)
    numeric_cols <- sapply(data, is.numeric)

    if (sum(numeric_cols) < 2) {
        stop("data must have at least 2 numeric columns")
    }

    method <- match.arg(method, c("conditional", "threshold", "phase", "cascade"))

    if (verbose) message("Starting dormant pattern detection using method: ", method)

    # Extract numeric data
    num_data <- data[, numeric_cols, drop = FALSE]
    num_data <- na.omit(num_data)
    n_obs <- nrow(num_data)
    n_vars <- ncol(num_data)

    if (n_obs < 10) {
        stop("Insufficient observations after removing NAs (need at least 10)")
    }

    # Initialize results
    patterns <- data.frame(
        variable_1 = character(),
        variable_2 = character(),
        dormancy_score = numeric(),
        trigger_variable = character(),
        trigger_region = character(),
        activation_risk = numeric(),
        stringsAsFactors = FALSE
    )

    conditional_regions <- list()

    # Generate all pairs of variables
    var_pairs <- combn(colnames(num_data), 2, simplify = FALSE)

    if (verbose) message("Analyzing ", length(var_pairs), " variable pairs...")

    # Compute overall correlations
    overall_cor <- cor(num_data)

    for (pair_idx in seq_along(var_pairs)) {
        pair <- var_pairs[[pair_idx]]
        v1 <- pair[1]
        v2 <- pair[2]

        if (verbose && pair_idx %% 10 == 0) {
            message("  Processing pair ", pair_idx, " of ", length(var_pairs))
        }

        x <- num_data[[v1]]
        y <- num_data[[v2]]

        # Detect dormant pattern based on method
        if (method == "conditional") {
            result <- detect_conditional_dormancy(
                x, y, v1, v2, data, numeric_cols,
                n_bins, min_cluster, overall_cor[v1, v2]
            )
        } else if (method == "threshold") {
            result <- detect_threshold_dormancy(
                x, y, v1, v2, n_bins, min_cluster,
                overall_cor[v1, v2]
            )
        } else if (method == "phase") {
            result <- detect_phase_dormancy(
                x, y, v1, v2, n_bins, min_cluster,
                overall_cor[v1, v2]
            )
        } else { # cascade
            result <- detect_cascade_dormancy(
                x, y, v1, v2, num_data, min_cluster,
                overall_cor[v1, v2]
            )
        }

        if (!is.null(result) && result$dormancy_score >= threshold) {
            patterns <- rbind(patterns, data.frame(
                variable_1 = v1,
                variable_2 = v2,
                dormancy_score = result$dormancy_score,
                trigger_variable = result$trigger_variable,
                trigger_region = result$trigger_region,
                activation_risk = result$activation_risk,
                stringsAsFactors = FALSE
            ))
            conditional_regions[[paste(v1, v2, sep = "~")]] <- result$regions
        }
    }

    # Sort by dormancy score
    if (nrow(patterns) > 0) {
        patterns <- patterns[order(-patterns$dormancy_score), ]
    }

    # Create result object
    result <- list(
        patterns = patterns,
        data = num_data,
        method = method,
        threshold = threshold,
        conditional_regions = conditional_regions,
        metadata = list(
            n_observations = n_obs,
            n_variables = n_vars,
            n_pairs_analyzed = length(var_pairs),
            n_patterns_detected = nrow(patterns),
            detection_time = Sys.time()
        )
    )

    class(result) <- "dormancy"

    if (verbose) {
        message("Detection complete. Found ", nrow(patterns), " dormant patterns.")
    }

    return(result)
}


#' Detect conditional dormancy
#' @keywords internal
detect_conditional_dormancy <- function(x, y, v1, v2, data, numeric_cols,
                                        n_bins, min_cluster, overall_cor) {
    # Find conditioning variables (other numeric columns)
    other_vars <- setdiff(names(data)[numeric_cols], c(v1, v2))

    if (length(other_vars) == 0) {
        # Use quantile-based regions of x and y themselves
        return(detect_self_conditional(x, y, v1, v2, n_bins, min_cluster, overall_cor))
    }

    best_result <- NULL
    best_dormancy <- 0

    for (cond_var in other_vars) {
        cond_vals <- data[[cond_var]]
        cond_vals <- as.numeric(cond_vals)

        # Bin the conditioning variable
        if (length(unique(cond_vals)) > n_bins) {
            breaks <- quantile(cond_vals,
                probs = seq(0, 1, length.out = n_bins + 1),
                na.rm = TRUE
            )
            breaks <- unique(breaks) # Handle ties
            bins <- cut(cond_vals, breaks = breaks, include.lowest = TRUE)
        } else {
            bins <- factor(cond_vals)
        }

        # Compute correlation in each bin
        bin_cors <- tapply(seq_along(x), bins, function(idx) {
            if (length(idx) < 5) {
                return(NA)
            }
            cor(x[idx], y[idx], use = "complete.obs")
        })

        # Find bins with significant correlation differences
        valid_cors <- bin_cors[!is.na(bin_cors)]

        if (length(valid_cors) < 2) next

        # Dormancy score: how much does correlation vary across bins?
        cor_range <- max(valid_cors) - min(valid_cors)
        cor_var <- var(valid_cors, na.rm = TRUE)

        # Check if overall correlation is weak but some bin correlations are strong
        strong_in_bin <- max(abs(valid_cors))
        weak_overall <- abs(overall_cor) < 0.3

        if (weak_overall && strong_in_bin > 0.4) {
            dormancy_score <- strong_in_bin * cor_range

            # Find the region with strongest correlation
            strongest_bin <- names(which.max(abs(valid_cors)))
            bin_props <- prop.table(table(bins))

            # Activation risk based on how often we're near the trigger region
            activation_risk <- as.numeric(bin_props[strongest_bin])

            if (dormancy_score > best_dormancy) {
                best_dormancy <- dormancy_score
                best_result <- list(
                    dormancy_score = dormancy_score,
                    trigger_variable = cond_var,
                    trigger_region = strongest_bin,
                    activation_risk = activation_risk,
                    regions = list(
                        bin_correlations = bin_cors,
                        bin_proportions = bin_props,
                        strongest_region = strongest_bin
                    )
                )
            }
        }
    }

    return(best_result)
}


#' Detect self-conditional dormancy (when no other variables available)
#' @keywords internal
detect_self_conditional <- function(x, y, v1, v2, n_bins, min_cluster, overall_cor) {
    # Divide x into quantile regions and check for varying correlation with y
    breaks <- quantile(x, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
    breaks <- unique(breaks)
    bins <- cut(x, breaks = breaks, include.lowest = TRUE)

    bin_cors <- tapply(seq_along(y), bins, function(idx) {
        if (length(idx) < 5) {
            return(NA)
        }
        cor(x[idx], y[idx], use = "complete.obs")
    })

    valid_cors <- bin_cors[!is.na(bin_cors)]

    if (length(valid_cors) < 2) {
        return(NULL)
    }

    cor_range <- max(valid_cors) - min(valid_cors)
    strong_in_bin <- max(abs(valid_cors))
    weak_overall <- abs(overall_cor) < 0.3

    if (weak_overall && strong_in_bin > 0.4 && cor_range > 0.3) {
        strongest_bin <- names(which.max(abs(valid_cors)))
        bin_props <- prop.table(table(bins))

        return(list(
            dormancy_score = strong_in_bin * cor_range,
            trigger_variable = paste0("region_of_", v1),
            trigger_region = strongest_bin,
            activation_risk = as.numeric(bin_props[strongest_bin]),
            regions = list(
                bin_correlations = bin_cors,
                bin_proportions = bin_props,
                strongest_region = strongest_bin
            )
        ))
    }

    return(NULL)
}


#' Detect threshold-based dormancy
#' @keywords internal
detect_threshold_dormancy <- function(x, y, v1, v2, n_bins, min_cluster, overall_cor) {
    # Look for threshold effects where correlation suddenly changes
    x_sorted <- sort(x, index.return = TRUE)
    x_order <- x_sorted$ix

    # Rolling correlation
    window_size <- max(20, floor(length(x) * min_cluster * 2))

    if (window_size >= length(x) - 10) {
        return(NULL)
    }

    rolling_cor <- sapply(1:(length(x) - window_size), function(i) {
        idx <- x_order[i:(i + window_size)]
        cor(x[idx], y[idx])
    })

    # Find sudden changes in rolling correlation
    cor_diff <- diff(rolling_cor)
    threshold_idx <- which.max(abs(cor_diff))

    if (length(threshold_idx) == 0 || abs(cor_diff[threshold_idx]) < 0.3) {
        return(NULL)
    }

    threshold_val <- x_sorted$x[threshold_idx + window_size / 2]

    # Split data at threshold and compute correlations
    below_idx <- x <= threshold_val
    above_idx <- x > threshold_val

    if (sum(below_idx) < 10 || sum(above_idx) < 10) {
        return(NULL)
    }

    cor_below <- cor(x[below_idx], y[below_idx])
    cor_above <- cor(x[above_idx], y[above_idx])
    cor_diff_val <- abs(cor_above - cor_below)

    if (cor_diff_val > 0.3 && abs(overall_cor) < 0.3) {
        dormancy_score <- cor_diff_val * max(abs(cor_below), abs(cor_above))

        return(list(
            dormancy_score = dormancy_score,
            trigger_variable = v1,
            trigger_region = paste0("x > ", round(threshold_val, 3)),
            activation_risk = mean(above_idx),
            regions = list(
                threshold = threshold_val,
                correlation_below = cor_below,
                correlation_above = cor_above,
                rolling_correlations = rolling_cor
            )
        ))
    }

    return(NULL)
}


#' Detect phase-based dormancy
#' @keywords internal
detect_phase_dormancy <- function(x, y, v1, v2, n_bins, min_cluster, overall_cor) {
    # Look for phase-based patterns (like oscillations with different correlations)
    # Compute phase angles
    if (any(is.na(x)) || any(is.na(y))) {
        valid_idx <- !is.na(x) & !is.na(y)
        x <- x[valid_idx]
        y <- y[valid_idx]
    }

    # Center and normalize
    x_norm <- (x - mean(x)) / sd(x)
    y_norm <- (y - mean(y)) / sd(y)

    # Compute phase angle in x-y space
    phase <- atan2(y_norm, x_norm)

    # Bin by phase and compute correlation
    phase_bins <- cut(phase,
        breaks = seq(-pi, pi, length.out = n_bins + 1),
        include.lowest = TRUE
    )

    bin_cors <- tapply(seq_along(x), phase_bins, function(idx) {
        if (length(idx) < 5) {
            return(NA)
        }
        cor(x[idx], y[idx])
    })

    valid_cors <- bin_cors[!is.na(bin_cors)]

    if (length(valid_cors) < 2) {
        return(NULL)
    }

    cor_range <- max(valid_cors) - min(valid_cors)
    strong_in_phase <- max(abs(valid_cors))

    if (cor_range > 0.4 && strong_in_phase > 0.4 && abs(overall_cor) < 0.3) {
        strongest_phase <- names(which.max(abs(valid_cors)))
        bin_props <- prop.table(table(phase_bins))

        return(list(
            dormancy_score = strong_in_phase * cor_range,
            trigger_variable = paste0("phase(", v1, ",", v2, ")"),
            trigger_region = strongest_phase,
            activation_risk = as.numeric(bin_props[strongest_phase]),
            regions = list(
                phase_correlations = bin_cors,
                phase_proportions = bin_props
            )
        ))
    }

    return(NULL)
}


#' Detect cascade-ready dormancy
#' @keywords internal
detect_cascade_dormancy <- function(x, y, v1, v2, num_data, min_cluster, overall_cor) {
    # Look for patterns that could trigger cascades through other variables
    other_vars <- setdiff(colnames(num_data), c(v1, v2))

    if (length(other_vars) == 0) {
        return(NULL)
    }

    # For each other variable, check if correlation changes when it's extreme
    cascade_scores <- c()
    cascade_vars <- c()
    cascade_regions <- c()

    for (other in other_vars) {
        z <- num_data[[other]]

        # Define "extreme" as top/bottom quartile
        q_lo <- quantile(z, 0.25)
        q_hi <- quantile(z, 0.75)

        extreme_idx <- z <= q_lo | z >= q_hi
        normal_idx <- !extreme_idx

        if (sum(extreme_idx) < 10 || sum(normal_idx) < 10) next

        cor_extreme <- cor(x[extreme_idx], y[extreme_idx])
        cor_normal <- cor(x[normal_idx], y[normal_idx])

        cor_diff <- abs(cor_extreme - cor_normal)

        if (cor_diff > 0.3 && abs(overall_cor) < 0.3) {
            cascade_scores <- c(cascade_scores, cor_diff)
            cascade_vars <- c(cascade_vars, other)
            cascade_regions <- c(
                cascade_regions,
                ifelse(abs(cor_extreme) > abs(cor_normal),
                    "extreme", "normal"
                )
            )
        }
    }

    if (length(cascade_scores) == 0) {
        return(NULL)
    }

    # Return the best cascade candidate
    best_idx <- which.max(cascade_scores)

    return(list(
        dormancy_score = cascade_scores[best_idx] * max(0.5, cascade_scores[best_idx]),
        trigger_variable = cascade_vars[best_idx],
        trigger_region = paste0(cascade_regions[best_idx], "_values"),
        activation_risk = 0.25, # Proportion in extreme regions
        regions = list(
            cascade_variable = cascade_vars[best_idx],
            cascade_scores = cascade_scores,
            cascade_variables = cascade_vars
        )
    ))
}
