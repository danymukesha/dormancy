# Changelog

## dormancy 0.1.0

- Initial CRAN release.

### New Features

#### Core Detection

- [`dormancy_detect()`](../reference/dormancy_detect.md): Main function
  for detecting dormant patterns with four methods:
  - Conditional detection: finds patterns suppressed by conditions
  - Threshold detection: finds patterns emerging at thresholds
  - Phase detection: finds patterns in specific phase regions
  - Cascade detection: finds cascade-ready patterns

#### Analysis Functions

- [`dormancy_trigger()`](../reference/dormancy_trigger.md): Identifies
  specific trigger conditions for activation
- [`dormancy_depth()`](../reference/dormancy_depth.md): Measures depth
  of dormancy (energy, stability, entropy methods)
- [`dormancy_risk()`](../reference/dormancy_risk.md): Quantifies
  activation risk and potential impact
- [`dormancy_scout()`](../reference/dormancy_scout.md): Maps data space
  for potential dormant regions
- [`awaken()`](../reference/awaken.md): Simulates pattern activation
  scenarios
- [`hibernate()`](../reference/hibernate.md): Detects patterns that
  became dormant over time

#### Visualization

- Plot methods for dormancy objects
- Support for overview, pattern network, risk, and timeline views

#### Performance

- Rcpp implementations for core algorithms
- Fast rolling correlation computation
- Efficient conditional correlation calculation
- Optimized entropy and mutual information estimation

### Documentation

- Comprehensive vignette introducing the package
- Full roxygen2 documentation
- Examples for all major functions

### Testing

- Complete test suite with testthat
- Coverage for all exported functions
