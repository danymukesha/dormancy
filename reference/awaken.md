# Simulate Awakening of Dormant Patterns

Simulates what would happen if a dormant pattern were to "awaken" -
i.e., become active. This function allows exploration of potential
future states and scenario analysis without waiting for actual pattern
activation.

## Usage

``` r
awaken(
  dormancy_result,
  pattern_id = 1,
  intensity = 1,
  n_sim = 100,
  return_data = FALSE,
  verbose = FALSE
)
```

## Arguments

- dormancy_result:

  An object of class "dormancy" from
  [`dormancy_detect`](https://danymukesha.github.io/dormancy/reference/dormancy_detect.md).

- pattern_id:

  Integer or "all". Which pattern(s) to awaken. Default is 1.

- intensity:

  Numeric. Intensity of awakening, from 0 (dormant) to 1 (fully active).
  Default is 1.

- n_sim:

  Integer. Number of simulation runs. Default is 100.

- return_data:

  Logical. Whether to return simulated data. Default is FALSE.

- verbose:

  Logical. Whether to print progress messages. Default is FALSE.

## Value

A list containing:

- `awakening_effects` - Data frame describing the effects of awakening

- `simulated_stats` - Summary statistics from simulations

- `cascade_effects` - Effects on other patterns (if any)

- `simulated_data` - If `return_data = TRUE`, simulated datasets

## Details

Awakening simulation is valuable for:

- Scenario planning and stress testing

- Understanding potential system behaviors

- Preparing for pattern activation events

- Testing the robustness of current strategies

The simulation works by:

1.  Identifying the dormant pattern's trigger conditions

2.  Simulating data where those conditions are met

3.  Applying the pattern's relationship to the simulated data

4.  Measuring the resulting effects on the system

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
awakening <- awaken(result, pattern_id = 1, n_sim = 50)
#> Warning: No dormant patterns to awaken.
print(awakening)
#> $awakening_effects
#> data frame with 0 columns and 0 rows
#> 
#> $simulated_stats
#> NULL
#> 
#> $cascade_effects
#> NULL
#> 
```
