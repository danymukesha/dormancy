# Measure the Depth of Dormancy in Patterns

Quantifies how "deeply asleep" a dormant pattern is - measuring the
energy required to activate it and the stability of its dormant state.
Deeper dormancy implies greater resistance to activation but potentially
larger effects when awakened.

## Usage

``` r
dormancy_depth(
  dormancy_result,
  method = "combined",
  normalize = TRUE,
  verbose = FALSE
)
```

## Arguments

- dormancy_result:

  An object of class "dormancy" from
  [`dormancy_detect`](dormancy_detect.md).

- method:

  Character. The depth measurement method:

  - "energy" - Measures the statistical "energy barrier" to activation

  - "stability" - Measures how stable the dormant state is

  - "entropy" - Measures information-theoretic dormancy depth

  - "combined" - Weighted combination of all methods

  Default is "combined".

- normalize:

  Logical. Whether to normalize depth scores to \[0, 1\]. Default is
  TRUE.

- verbose:

  Logical. Whether to print progress messages. Default is FALSE.

## Value

A list containing:

- `depths` - Data frame with depth measurements for each pattern

- `depth_distribution` - Summary statistics of depth distribution

- `awakening_effort` - Estimated effort required to activate each
  pattern

- `stability_index` - Stability measure for each pattern's dormant state

## Details

Dormancy depth is a novel concept in statistical analysis, inspired by:

- **Physics**: Potential energy barriers in phase transitions

- **Biology**: Depth of seed dormancy (stratification requirements)

- **Geology**: Locked fault segments and earthquake potential

A deeply dormant pattern:

- Requires significant change in conditions to activate

- Is stable against minor perturbations

- May have a larger effect when finally awakened

- Represents accumulated "potential energy" in the system

The depth measurement helps prioritize which patterns to monitor and
what magnitude of change would be required to awaken them.

## Examples

``` r
set.seed(42)
n <- 500
x <- rnorm(n)
# Create a deeply dormant pattern (only active in extreme conditions)
z <- ifelse(abs(x) > 2, 1, 0)
y <- ifelse(z == 1, 0.9 * x + rnorm(sum(z), 0, 0.1), rnorm(n))
#> Warning: longer object length is not a multiple of shorter object length
data <- data.frame(x = x, y = y)

result <- dormancy_detect(data, method = "threshold")
depths <- dormancy_depth(result)
#> Warning: No dormant patterns to measure depth for.
print(depths)
#> $depths
#> data frame with 0 columns and 0 rows
#> 
#> $depth_distribution
#> NULL
#> 
#> $awakening_effort
#> NULL
#> 
#> $stability_index
#> NULL
#> 
```
