#' @title Identify Patterns That Have Become Dormant Over Time
#' @name hibernate
#' @description
#' Analyzes time series or sequential data to identify patterns that were once
#' active but have become dormant. This is the inverse problem from
#' \code{\link{dormancy_detect}} - finding patterns that "went to sleep" rather
#' than patterns that are currently dormant.
#'
#' @param data A data frame with a time column or sequential index.
#' @param time_var Character. Name of the time/sequence variable. If NULL, uses
#'   row order as sequence. Default is NULL.
#' @param window_size Integer or numeric. Size of the rolling window for detecting
#'   changes. If integer, uses number of observations. If numeric < 1, uses
#'   proportion of data. Default is 0.2 (20\% of data).
#' @param threshold Numeric. Minimum change in pattern strength to be considered
#'   hibernation. Default is 0.3.
#' @param min_observations Integer. Minimum observations required for analysis.
#'   Default is 30.
#' @param verbose Logical. Whether to print progress messages. Default is FALSE.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{hibernated_patterns} - Patterns that have become dormant
#'   \item \code{timeline} - When patterns transitioned to dormancy
#'   \item \code{hibernation_depth} - How deeply patterns have hibernated
#'   \item \code{revival_potential} - Likelihood patterns could reawaken
#' }
#'
#' @details
#' Hibernation detection is important for:
#'
#' \itemize{
#'   \item Understanding system evolution and regime changes
#'   \item Identifying lost relationships that might return
#'   \item Detecting structural breaks in relationships
#'   \item Monitoring degradation of system components
#' }
#'
#' A pattern is considered to have "hibernated" if:
#' \enumerate{
#'   \item It was strong in an earlier time window
#'   \item It has weakened significantly in recent windows
#'   \item The weakening is not due to noise or reduced sample size
#' }
#'
#' @examples
#' set.seed(42)
#' n <- 500
#' time <- 1:n
#' x <- rnorm(n)
#' # Relationship that fades over time
#' effect_strength <- exp(-time / 200)
#' y <- effect_strength * 0.8 * x + (1 - effect_strength) * rnorm(n)
#' data <- data.frame(time = time, x = x, y = y)
#'
#' hib <- hibernate(data, time_var = "time", window_size = 0.15)
#' print(hib)
#'
#' @export
hibernate <- function(data, time_var = NULL, window_size = 0.2,
                      threshold = 0.3, min_observations = 30,
                      verbose = FALSE) {
    if (!is.data.frame(data) && !is.matrix(data)) {
        stop("data must be a data frame or matrix")
    }

    data <- as.data.frame(data)
    numeric_cols <- sapply(data, is.numeric)

    if (sum(numeric_cols) < 2) {
        stop("Need at least 2 numeric columns")
    }

    if (nrow(data) < min_observations) {
        stop("Need at least ", min_observations, " observations")
    }

    if (verbose) message("Analyzing pattern hibernation...")

    # Handle time variable
    if (!is.null(time_var)) {
        if (!(time_var %in% colnames(data))) {
            stop("time_var '", time_var, "' not found in data")
        }
        time <- data[[time_var]]
        numeric_cols[time_var] <- FALSE
    } else {
        time <- seq_len(nrow(data))
    }

    # Sort by time
    time_order <- order(time)
    data <- data[time_order, ]
    time <- time[time_order]

    num_data <- data[, numeric_cols, drop = FALSE]

    # Determine window size
    if (window_size < 1) {
        window_size <- max(floor(nrow(num_data) * window_size), 10)
    }

    # Analyze each pair of variables
    var_pairs <- combn(colnames(num_data), 2, simplify = FALSE)

    hibernated_patterns <- list()
    timelines <- list()

    for (pair in var_pairs) {
        v1 <- pair[1]
        v2 <- pair[2]

        x <- num_data[[v1]]
        y <- num_data[[v2]]

        # Rolling correlation
        n_windows <- nrow(num_data) - window_size + 1

        if (n_windows < 2) next

        rolling_cor <- numeric(n_windows)
        window_time <- numeric(n_windows)

        for (i in seq_len(n_windows)) {
            idx <- i:(i + window_size - 1)
            rolling_cor[i] <- cor(x[idx], y[idx], use = "complete.obs")
            window_time[i] <- mean(time[idx])
        }

        # Detect hibernation: strong correlation becoming weak
        early_cor <- mean(rolling_cor[1:min(5, length(rolling_cor))], na.rm = TRUE)
        late_cor <- mean(rolling_cor[(length(rolling_cor) - 4):length(rolling_cor)],
            na.rm = TRUE
        )

        cor_change <- abs(early_cor) - abs(late_cor)

        if (cor_change >= threshold && abs(early_cor) > 0.4 && abs(late_cor) < 0.3) {
            # Find approximate hibernation time
            hibernation_idx <- which(abs(rolling_cor) < 0.3)[1]
            if (is.na(hibernation_idx)) hibernation_idx <- length(rolling_cor)
            hibernation_time <- window_time[hibernation_idx]

            hibernated_patterns[[paste(v1, v2, sep = "~")]] <- data.frame(
                variable_1 = v1,
                variable_2 = v2,
                early_correlation = early_cor,
                late_correlation = late_cor,
                correlation_change = cor_change,
                hibernation_time = hibernation_time,
                hibernation_depth = cor_change,
                stringsAsFactors = FALSE
            )

            timelines[[paste(v1, v2, sep = "~")]] <- data.frame(
                time = window_time,
                rolling_correlation = rolling_cor
            )
        }
    }

    # Compile results
    if (length(hibernated_patterns) > 0) {
        hibernated_df <- do.call(rbind, hibernated_patterns)
        hibernated_df <- hibernated_df[order(-hibernated_df$correlation_change), ]

        # Compute revival potential
        revival_potential <- data.frame(
            variable_pair = rownames(hibernated_df),
            revival_potential = 1 - hibernated_df$hibernation_depth,
            revival_difficulty = cut(hibernated_df$hibernation_depth,
                breaks = c(-Inf, 0.4, 0.6, Inf),
                labels = c("Easy", "Moderate", "Difficult")
            )
        )
    } else {
        hibernated_df <- data.frame()
        revival_potential <- data.frame()
    }

    result <- list(
        hibernated_patterns = hibernated_df,
        timeline = timelines,
        hibernation_depth = if (nrow(hibernated_df) > 0) hibernated_df$hibernation_depth else NULL,
        revival_potential = revival_potential,
        window_size = window_size,
        threshold = threshold
    )

    class(result) <- "hibernation"
    return(result)
}


#' @export
print.hibernation <- function(x, ...) {
    cat("Pattern Hibernation Analysis\n")
    cat("============================\n\n")

    cat("Window size:", x$window_size, "\n")
    cat("Hibernation threshold:", x$threshold, "\n\n")

    if (nrow(x$hibernated_patterns) > 0) {
        cat("Hibernated Patterns:\n")
        print(x$hibernated_patterns)
        cat("\n")

        cat("Revival Potential:\n")
        print(x$revival_potential)
    } else {
        cat("No hibernated patterns detected.\n")
    }

    invisible(x)
}
