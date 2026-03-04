# Identify Patterns That Have Become Dormant Over Time

Analyzes time series or sequential data to identify patterns that were
once active but have become dormant. This is the inverse problem from
[`dormancy_detect`](https://danymukesha.github.io/dormancy/reference/dormancy_detect.md) -
finding patterns that "went to sleep" rather than patterns that are
currently dormant.

## Usage

``` r
hibernate(
  data,
  time_var = NULL,
  window_size = 0.2,
  threshold = 0.3,
  min_observations = 30,
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame with a time column or sequential index.

- time_var:

  Character. Name of the time/sequence variable. If NULL, uses row order
  as sequence. Default is NULL.

- window_size:

  Integer or numeric. Size of the rolling window for detecting changes.
  If integer, uses number of observations. If numeric \< 1, uses
  proportion of data. Default is 0.2 (20% of data).

- threshold:

  Numeric. Minimum change in pattern strength to be considered
  hibernation. Default is 0.3.

- min_observations:

  Integer. Minimum observations required for analysis. Default is 30.

- verbose:

  Logical. Whether to print progress messages. Default is FALSE.

## Value

A list containing:

- `hibernated_patterns` - Patterns that have become dormant

- `timeline` - When patterns transitioned to dormancy

- `hibernation_depth` - How deeply patterns have hibernated

- `revival_potential` - Likelihood patterns could reawaken

## Details

Hibernation detection is important for:

- Understanding system evolution and regime changes

- Identifying lost relationships that might return

- Detecting structural breaks in relationships

- Monitoring degradation of system components

A pattern is considered to have "hibernated" if:

1.  It was strong in an earlier time window

2.  It has weakened significantly in recent windows

3.  The weakening is not due to noise or reduced sample size

## Examples

``` r
set.seed(42)
n <- 500
time <- 1:n
x <- rnorm(n)
# Relationship that fades over time
effect_strength <- exp(-time / 200)
y <- effect_strength * 0.8 * x + (1 - effect_strength) * rnorm(n)
data <- data.frame(time = time, x = x, y = y)

hib <- hibernate(data, time_var = "time", window_size = 0.15)
print(hib)
#> Pattern Hibernation Analysis
#> ============================
#> 
#> Window size: 75 
#> Hibernation threshold: 0.3 
#> 
#> Hibernated Patterns:
#>     variable_1 variable_2 early_correlation late_correlation correlation_change
#> x~y          x          y         0.9574825      -0.05163983          0.9058426
#>     hibernation_time hibernation_depth
#> x~y              258         0.9058426
#> 
#> Revival Potential:
#>   variable_pair revival_potential revival_difficulty
#> 1           x~y        0.09415736          Difficult
```
