# Assess Risk of Dormant Pattern Activation

Quantifies the risk associated with dormant patterns, including the
probability of activation, potential impact, and uncertainty in risk
estimates. This function provides actionable risk metrics for
decision-making and monitoring priorities.

## Usage

``` r
dormancy_risk(
  dormancy_result,
  depth_result = NULL,
  impact_weights = NULL,
  time_horizon = 1,
  risk_tolerance = 0.3,
  verbose = FALSE
)
```

## Arguments

- dormancy_result:

  An object of class "dormancy" from
  [`dormancy_detect`](dormancy_detect.md).

- depth_result:

  Optional. An object of class "dormancy_depth" from
  [`dormancy_depth`](dormancy_depth.md). If provided, uses depth
  information for more accurate risk assessment.

- impact_weights:

  Optional named vector of weights for different impact types. Default
  considers symmetric positive/negative impacts.

- time_horizon:

  Numeric. The time horizon for risk assessment (in abstract units).
  Longer horizons increase activation probability. Default is 1.

- risk_tolerance:

  Numeric. Risk tolerance threshold for flagging. Default is 0.3.

- verbose:

  Logical. Whether to print progress messages. Default is FALSE.

## Value

A list containing:

- `risk_scores` - Data frame with risk metrics for each pattern

- `risk_matrix` - Matrix of activation probability x impact

- `priorities` - Ordered list of patterns by risk priority

- `recommendations` - Risk management recommendations

- `summary` - Overall risk summary statistics

## Details

Risk assessment for dormant patterns considers multiple dimensions:

- **Activation Probability**: Likelihood that trigger conditions will be
  met in the given time horizon

- **Impact Magnitude**: Expected effect size if the pattern activates

- **Impact Direction**: Whether activation would be beneficial, harmful,
  or neutral

- **Cascade Potential**: Risk of triggering other patterns

- **Uncertainty**: Confidence in risk estimates

The risk score combines these dimensions into an actionable metric:
\$\$Risk = P(activation) \times Impact \times CascadeFactor \times (1 +
Uncertainty)\$\$

## Examples

``` r
set.seed(42)
n <- 500
x <- rnorm(n)
z <- sample(c(0, 1), n, replace = TRUE)
y <- ifelse(z == 1, 0.8 * x + rnorm(sum(z), 0, 0.3), rnorm(n))
#> Warning: longer object length is not a multiple of shorter object length
data <- data.frame(x = x, y = y, z = factor(z))

result <- dormancy_detect(data, method = "conditional")
risk <- dormancy_risk(result, time_horizon = 2)
#> Warning: No dormant patterns to assess risk for.
print(risk)
#> $risk_scores
#> data frame with 0 columns and 0 rows
#> 
#> $risk_matrix
#> NULL
#> 
#> $priorities
#> character(0)
#> 
#> $recommendations
#> [1] "No dormant patterns detected."
#> 
#> $summary
#> NULL
#> 
```
