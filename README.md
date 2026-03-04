# dormancy <a href="https://danymukesha.github.io/dormancy/"><img src="man/figures/logo.png" align="right" height="139" alt="dormancy website" /></a>

<!-- badges: start -->
<!--
[![CRAN status](https://www.r-pkg.org/badges/version/dormancy)](https://CRAN.R-project.org/package=dormancy)
[![R-CMD-check](https://github.com/danymukesha/dormancy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/danymukesha/dormancy/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/danymukesha/dormancy/branch/main/graph/badge.svg)](https://app.codecov.io/gh/danymukesha/dormancy?branch=main)
-->
<!-- badges: end -->
<!--
## Overview
-->
**dormancy** is a novel R package for detecting and analyzing *dormant
patterns* in multivariate data. Unlike traditional pattern detection methods
that focus on currently active relationships, dormancy identifies statistical
patterns that exist but remain inactive until specific trigger conditions
emerge.

### What Makes This Package Unique?

This is the **first statistical package** dedicated to dormant pattern 
detection. The concept is inspired by:

- **Biological dormancy**: Seeds remaining dormant until conditions are right
- **Geological phenomena**: Dormant faults that can trigger earthquakes
- **Epidemiology**: Latent infections that activate under stress

In data analysis, dormant patterns are relationships that:

- Are strong in specific data regions but weak overall
- Only emerge when certain thresholds are crossed
- Are masked by confounding variables
- Could trigger cascade effects when activated

## Installation

```r
# Install from CRAN (when available)
install.packages("dormancy")

# Install development version
# devtools::install_github("danymukesha/dormancy")
```

## Quick Example

```r
library(dormancy)

set.seed(42)
n <- 500

# Create data with a dormant pattern
x <- rnorm(n)
condition <- sample(c(0, 1), n, replace = TRUE)

# Relationship only exists when condition == 1
y <- ifelse(condition == 1, 
            0.8 * x + rnorm(n, 0, 0.3), 
            rnorm(n))

data <- data.frame(x = x, y = y, condition = factor(condition))

# Overall correlation is weak
cor(data$x, data$y)  # ~0.35

# Detect the dormant pattern
result <- dormancy_detect(data, method = "conditional")
print(result)
#> Dormant pattern detected: x ~ y
#> Dormancy score: 0.72
#> Trigger: condition == 1
```

## Core Functions

| Function | Description |
|----------|-------------|
| `dormancy_detect()` | Detect dormant patterns using 4 methods |
| `dormancy_trigger()` | Identify activation trigger conditions |
| `dormancy_depth()` | Measure how deeply dormant a pattern is |
| `dormancy_risk()` | Assess activation risk and potential impact |
| `dormancy_scout()` | Map data space for potential dormant regions |
| `awaken()` | Simulate what happens when patterns activate |
| `hibernate()` | Find patterns that have become dormant over time |

## Detection Methods

### 1. Conditional Detection
Finds patterns that are conditionally suppressed - active only under specific
conditions.

### 2. Threshold Detection
Identifies patterns that emerge when variables cross specific thresholds.

### 3. Phase Detection
Detects patterns that exist in specific phase regions of the data space.

### 4. Cascade Detection
Finds patterns that could trigger chain reactions through other variables.

## Why Dormancy Matters

Traditional correlation analysis misses dormant patterns because:

- **Aggregate statistics mask conditional relationships**
- **Weak overall correlations may hide strong local correlations**
- **Threshold effects create piecewise relationships**
- **Phase-dependent patterns vary across the data space**

## Use Cases

### Financial Risk
```r
# Detect dormant correlations that could activate during market stress
result <- dormancy_detect(returns_data, method = "threshold")
risk <- dormancy_risk(result, time_horizon = 30)
```

### Quality Control
```r
# Find patterns that only emerge under certain conditions
result <- dormancy_detect(process_data, method = "conditional")
triggers <- dormancy_trigger(result)
```

### Environmental Monitoring
```r
# Identify dormant patterns signaling ecological shifts
scout <- dormancy_scout(sensor_data)
hib <- hibernate(time_series_data, time_var = "date")
```

### Healthcare Analytics
```r
# Detect latent risk factors
result <- dormancy_detect(patient_data, method = "cascade")
awakening <- awaken(result, intensity = 1)
```

## Key Concepts

### Dormancy Score
Measures how "dormant" a pattern is (0 = active, 1 = fully dormant).

### Trigger Conditions
The specific circumstances under which a dormant pattern would activate.

### Depth of Dormancy
How much change is needed to awaken the pattern:

- **Shallow**: Minor perturbation could activate
- **Deep**: Significant systemic shift required

### Cascade Potential
Risk that activating one pattern triggers others.

## Citation

If you use dormancy in your research, please cite:

```bibtex
@Manual{dormancy,
  title = {dormancy: Detection and Analysis of Dormant Patterns in Data},
  author = {Dany Mukesha},
  year = {2026},
  note = {R package version 0.1.0},
  url = {https://github.com/danymukesha/dormancy}
}
```

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for
guidelines.

## License

MIT License. See [LICENSE](LICENSE) for details.

## Acknowledgments

This package develops a novel statistical framework inspired by concepts from
biology, geology, and epidemiology. The idea of dormant patterns in data
analysis provides a new perspective on hidden relationships and latent risks.

