# Scout for Dormant Pattern Regions in Data

Systematically scans the data space to identify regions where dormant
patterns might emerge. Unlike [`dormancy_detect`](dormancy_detect.md)
which identifies specific patterns, `dormancy_scout` maps the "terrain"
of dormancy potential.

## Usage

``` r
dormancy_scout(
  data,
  grid_resolution = 20,
  scout_method = "density",
  return_map = TRUE,
  verbose = FALSE
)
```

## Arguments

- data:

  A numeric matrix or data frame.

- grid_resolution:

  Integer. Resolution of the scanning grid. Higher values give finer
  resolution but slower computation. Default is 20.

- scout_method:

  Character. Scanning method:

  - "density" - Identifies low-density regions where patterns might hide

  - "variance" - Identifies high-variance regions with pattern potential

  - "correlation" - Maps local correlation landscapes

  - "entropy" - Identifies high-entropy regions with dormancy potential

  Default is "density".

- return_map:

  Logical. Whether to return the full dormancy map. Default is TRUE.

- verbose:

  Logical. Whether to print progress messages. Default is FALSE.

## Value

A list containing:

- `scout_results` - Data frame with coordinates and dormancy potential

- `hotspots` - Regions with highest dormancy potential

- `dormancy_map` - If `return_map = TRUE`, a matrix representing the
  dormancy landscape

- `summary` - Summary statistics of the scan

## Details

Scout analysis is useful for:

- Identifying regions to monitor for future pattern emergence

- Understanding the "geography" of your data's pattern space

- Finding data regions that are underexplored or anomalous

- Planning targeted data collection in high-potential regions

The scout creates a map of "dormancy potential" - not actual patterns,
but locations where patterns are more likely to exist or emerge.

## Examples

``` r
set.seed(42)
n <- 500
x <- rnorm(n)
y <- rnorm(n)
# Create a region with hidden pattern
z <- ifelse(x > 1 & y > 1, 0.9 * x + rnorm(sum(x > 1 & y > 1), 0, 0.1), y)
#> Warning: longer object length is not a multiple of shorter object length
data <- data.frame(x = x, y = z)

scout <- dormancy_scout(data, grid_resolution = 15)
print(scout)
#> Dormancy Scout Results
#> ======================
#> 
#> Method: density 
#> Grid resolution: 15 
#> 
#> Variable Pair Analysis:
#>     variable_pair max_potential mean_potential n_hotspots
#> x~y           x~y          0.99      0.7106933         21
#> 
#> Top Dormancy Hotspots:
#>      x_coord    y_coord potential variable_pair
#> 3  -2.141811 -1.4097267      0.99           x~y
#> 4   2.540226 -0.9192236      0.99           x~y
#> 5  -2.567450 -0.4287205      0.99           x~y
#> 9  -2.567450  0.5522857      0.99           x~y
#> 10  2.540226  0.5522857      0.99           x~y
#> 11 -2.567450  1.0427888      0.99           x~y
#> 14 -2.141811  1.5332919      0.99           x~y
#> 17  1.688946  2.0237950      0.99           x~y
#> 18 -0.439252  2.5142981      0.99           x~y
#> 21  0.837667  2.5142981      0.99           x~y
```
