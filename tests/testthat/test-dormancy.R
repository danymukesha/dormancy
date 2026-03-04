# Tests for dormancy package
# Author: Statistical Innovation Lab

test_that("dormancy_detect works with basic data", {
    set.seed(42)
    n <- 200
    x <- rnorm(n)
    z <- sample(c(0, 1), n, replace = TRUE)
    y <- ifelse(z == 1, 0.8 * x + rnorm(n, 0, 0.3), rnorm(n))
    data <- data.frame(x = x, y = y, z = factor(z))

    result <- dormancy_detect(data, method = "conditional", threshold = 0.2)

    expect_s3_class(result, "dormancy")
    expect_true(is.data.frame(result$patterns))
    expect_true(result$metadata$n_observations > 0)
})

test_that("dormancy_detect returns empty data frame when no patterns", {
    set.seed(42)
    n <- 100
    x <- rnorm(n)
    y <- rnorm(n) # No relationship
    data <- data.frame(x = x, y = y)

    result <- dormancy_detect(data, method = "conditional", threshold = 0.9)

    expect_s3_class(result, "dormancy")
    expect_equal(nrow(result$patterns), 0)
})

test_that("dormancy_detect validates input", {
    expect_error(dormancy_detect("not a data frame"), "must be a data frame or matrix")
    expect_error(dormancy_detect(data.frame(a = 1:10)), "at least 2 numeric columns")
})

test_that("dormancy_trigger works with dormancy result", {
    set.seed(42)
    n <- 200
    x <- rnorm(n)
    z <- sample(c(0, 1), n, replace = TRUE)
    y <- ifelse(z == 1, 0.8 * x + rnorm(n, 0, 0.3), rnorm(n))
    data <- data.frame(x = x, y = y, z = factor(z))

    result <- dormancy_detect(data, method = "conditional", threshold = 0.2)

    if (nrow(result$patterns) > 0) {
        triggers <- dormancy_trigger(result, n_bootstrap = 10)
        expect_s3_class(triggers, "dormancy_trigger")
        expect_true(is.data.frame(triggers$triggers))
    } else {
        # No patterns to analyze
        triggers <- dormancy_trigger(result)
        expect_true(is.data.frame(triggers$triggers))
    }
})

test_that("dormancy_depth works correctly", {
    set.seed(42)
    n <- 200
    x <- rnorm(n)
    z <- sample(c(0, 1), n, replace = TRUE)
    y <- ifelse(z == 1, 0.8 * x + rnorm(n, 0, 0.3), rnorm(n))
    data <- data.frame(x = x, y = y, z = factor(z))

    result <- dormancy_detect(data, method = "conditional", threshold = 0.2)


    if (nrow(result$patterns) > 0) {
        depths <- dormancy_depth(result)
        expect_s3_class(depths, "dormancy_depth")
        expect_true(is.data.frame(depths$depths))
        expect_true(all(depths$depths$combined_depth >= 0))
        expect_true(all(depths$depths$combined_depth <= 1))
    }
})

test_that("dormancy_risk computes risk scores", {
    set.seed(42)
    n <- 200
    x <- rnorm(n)
    z <- sample(c(0, 1), n, replace = TRUE)
    y <- ifelse(z == 1, 0.8 * x + rnorm(n, 0, 0.3), rnorm(n))
    data <- data.frame(x = x, y = y, z = factor(z))

    result <- dormancy_detect(data, method = "conditional", threshold = 0.2)

    if (nrow(result$patterns) > 0) {
        risk <- dormancy_risk(result)
        expect_s3_class(risk, "dormancy_risk")
        expect_true(is.data.frame(risk$risk_scores))
        expect_true(all(risk$risk_scores$risk_score >= 0))
    }
})

test_that("awaken simulates pattern activation", {
    set.seed(42)
    n <- 200
    x <- rnorm(n)
    z <- sample(c(0, 1), n, replace = TRUE)
    y <- ifelse(z == 1, 0.8 * x + rnorm(n, 0, 0.3), rnorm(n))
    data <- data.frame(x = x, y = y, z = factor(z))

    result <- dormancy_detect(data, method = "conditional", threshold = 0.2)

    if (nrow(result$patterns) > 0) {
        awakening <- awaken(result, n_sim = 10)
        expect_s3_class(awakening, "awakening")
        expect_true(is.data.frame(awakening$awakening_effects))
    }
})

test_that("dormancy_scout maps data space", {
    set.seed(42)
    n <- 100
    x <- rnorm(n)
    y <- rnorm(n)
    data <- data.frame(x = x, y = y)

    scout <- dormancy_scout(data, grid_resolution = 10)

    expect_s3_class(scout, "dormancy_map")
    expect_true(is.data.frame(scout$scout_results))
})

test_that("hibernate detects dormant patterns", {
    set.seed(42)
    n <- 200
    time <- 1:n
    x <- rnorm(n)
    effect_strength <- exp(-time / 100)
    y <- effect_strength * 0.8 * x + (1 - effect_strength) * rnorm(n)
    data <- data.frame(time = time, x = x, y = y)

    hib <- hibernate(data, time_var = "time", window_size = 0.15)

    expect_s3_class(hib, "hibernation")
    expect_true(is.data.frame(hib$hibernated_patterns))
})

test_that("print methods work", {
    set.seed(42)
    n <- 100
    x <- rnorm(n)
    y <- rnorm(n)
    data <- data.frame(x = x, y = y)

    result <- dormancy_detect(data, threshold = 0.9)

    expect_output(print(result), "Dormancy Detection Results")
    expect_output(summary(result), "Dormancy Analysis Summary")
})

test_that("different detection methods work", {
    set.seed(42)
    n <- 200
    x <- rnorm(n)
    y <- rnorm(n)
    z <- rnorm(n)
    data <- data.frame(x = x, y = y, z = z)

    # Test all methods
    for (method in c("conditional", "threshold", "phase", "cascade")) {
        result <- dormancy_detect(data, method = method, threshold = 0.2)
        expect_s3_class(result, "dormancy")
    }
})

test_that("dormancy_detect handles missing values", {
    set.seed(42)
    n <- 100
    x <- rnorm(n)
    y <- rnorm(n)
    x[sample(n, 10)] <- NA
    y[sample(n, 10)] <- NA
    data <- data.frame(x = x, y = y)

    # expect_warning(result <- dormancy_detect(data, threshold = 0.2))
    result <- dormancy_detect(data, threshold = 0.2)
    expect_s3_class(result, "dormancy")
})

test_that("depth methods produce valid results", {
    set.seed(42)
    n <- 200
    x <- rnorm(n)
    z <- sample(c(0, 1), n, replace = TRUE)
    y <- ifelse(z == 1, 0.8 * x + rnorm(n, 0, 0.3), rnorm(n))
    data <- data.frame(x = x, y = y, z = factor(z))

    result <- dormancy_detect(data, method = "conditional", threshold = 0.2)

    if (nrow(result$patterns) > 0) {
        for (method in c("energy", "stability", "entropy", "combined")) {
            depths <- dormancy_depth(result, method = method)
            expect_s3_class(depths, "dormancy_depth")
        }
    }
})
