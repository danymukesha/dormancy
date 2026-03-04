# Detect Dormant Patterns in Multivariate Data

Identifies dormant patterns in multivariate data - statistical
relationships that exist but are currently inactive. Dormant patterns
only manifest when specific trigger conditions emerge in the data.

## Usage

``` r
dormancy_detect(
  data,
  threshold = 0.3,
  method = "conditional",
  n_bins = 10,
  min_cluster = 0.05,
  parallel = FALSE,
  verbose = FALSE
)
```

## Arguments

- data:

  A numeric matrix or data frame with observations in rows and variables
  in columns.

- threshold:

  Numeric. The minimum dormancy score for a pattern to be considered
  significant. Default is 0.3.

- method:

  Character. The detection method to use. Options are:

  - "conditional" - Detects patterns that are conditionally suppressed
    (active only under specific conditions)

  - "threshold" - Detects patterns that emerge when variables cross
    certain thresholds

  - "phase" - Detects patterns that exist in specific phase regions of
    the data space

  - "cascade" - Detects cascade-ready patterns that could trigger chain
    reactions

  Default is "conditional".

- n_bins:

  Integer. Number of bins for discretizing continuous variables when
  searching for conditional patterns. Default is 10.

- min_cluster:

  Numeric. Minimum proportion of observations that must be in a region
  for it to be considered. Default is 0.05 (5%).

- parallel:

  Logical. Whether to use parallel processing. Default is FALSE.

- verbose:

  Logical. Whether to print progress messages. Default is FALSE.

## Value

A list of class "dormancy" containing:

- `patterns` - A data frame of detected dormant patterns with columns:
  variables, dormancy_score, trigger_variable, trigger_region,
  activation_risk

- `data` - The input data

- `method` - The detection method used

- `threshold` - The threshold used

- `conditional_regions` - List of regions where patterns are dormant

- `metadata` - Additional information about the detection process

## Details

Dormant patterns are fundamentally different from weak or spurious
correlations. A dormant pattern is a genuine relationship that is
currently suppressed by prevailing conditions in the data. The key
insight is that:

1.  The pattern exists (verified through subset analysis)

2.  The pattern is currently inactive (not visible in aggregate
    statistics)

3.  The pattern can "awaken" when conditions change

The detection algorithm works by:

1.  Segmenting the data space into regions

2.  Computing pairwise relationships in each region

3.  Identifying relationships that vary significantly across regions

4.  Flagging relationships that are strong in some regions but weak
    overall

5.  Estimating the conditions under which dormant patterns would
    activate

## See also

[`dormancy_trigger`](dormancy_trigger.md) for identifying activation
triggers, [`dormancy_depth`](dormancy_depth.md) for measuring dormancy
depth, [`awaken`](awaken.md) for simulating pattern activation

## Examples

``` r
# Create data with a dormant pattern
set.seed(42)
n <- 1000
x <- rnorm(n)
z <- sample(c(0, 1), n, replace = TRUE)
# Relationship between x and y only exists when z == 1
y <- ifelse(z == 1, 0.8 * x + rnorm(n, 0, 0.3), rnorm(n))
data <- data.frame(x = x, y = y, z = factor(z))

# Detect dormant patterns
result <- dormancy_detect(data, method = "conditional")
print(result)
#> Dormancy Detection Results
#> ==========================
#> 
#> Method: conditional 
#> Threshold: 0.3 
#> Observations: 1000 
#> Variables: 2 
#> 
#> Patterns detected: 0 
```
