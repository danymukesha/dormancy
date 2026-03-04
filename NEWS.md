# dormancy 0.1.0

* Initial CRAN release.

## New Features

### Core Detection
- `dormancy_detect()`: Main function for detecting dormant patterns with four methods:
  - Conditional detection: finds patterns suppressed by conditions
  - Threshold detection: finds patterns emerging at thresholds
  - Phase detection: finds patterns in specific phase regions
  - Cascade detection: finds cascade-ready patterns

### Analysis Functions
- `dormancy_trigger()`: Identifies specific trigger conditions for activation
- `dormancy_depth()`: Measures depth of dormancy (energy, stability, entropy methods)
- `dormancy_risk()`: Quantifies activation risk and potential impact
- `dormancy_scout()`: Maps data space for potential dormant regions
- `awaken()`: Simulates pattern activation scenarios
- `hibernate()`: Detects patterns that became dormant over time

### Visualization
- Plot methods for dormancy objects
- Support for overview, pattern network, risk, and timeline views

### Performance
- Rcpp implementations for core algorithms
- Fast rolling correlation computation
- Efficient conditional correlation calculation
- Optimized entropy and mutual information estimation

## Documentation
- Comprehensive vignette introducing the package
- Full roxygen2 documentation
- Examples for all major functions

## Testing
- Complete test suite with testthat
- Coverage for all exported functions
