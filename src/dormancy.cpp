// Fast C++ implementations for dormancy package
// Author: Dany Mukesha

#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <algorithm>

using namespace Rcpp;

// Fast rolling correlation computation
//' @title Fast Rolling Correlation
//' @description Compute rolling correlation between two vectors efficiently.
//' @param x Numeric vector
//' @param y Numeric vector
//' @param window_size Integer window size
//' @return Numeric vector of rolling correlations
//' @keywords internal
// [[Rcpp::export]]
NumericVector fast_rolling_cor(NumericVector x, NumericVector y, int window_size) {
  int n = x.size();
  if (n != y.size()) {
    stop("x and y must have the same length");
  }

  if (window_size > n) {
    stop("window_size cannot exceed vector length");
  }

  int n_windows = n - window_size + 1;
  NumericVector result(n_windows, NA_REAL);

  for (int i = 0; i < n_windows; i++) {
    // Compute means
    double mean_x = 0.0, mean_y = 0.0;
    int count = 0;

    for (int j = i; j < i + window_size; j++) {
      if (!NumericVector::is_na(x[j]) && !NumericVector::is_na(y[j])) {
        mean_x += x[j];
        mean_y += y[j];
        count++;
      }
    }

    if (count < 3) continue;

    mean_x /= count;
    mean_y /= count;

    // Compute covariance and variances
    double cov = 0.0, var_x = 0.0, var_y = 0.0;

    for (int j = i; j < i + window_size; j++) {
      if (!NumericVector::is_na(x[j]) && !NumericVector::is_na(y[j])) {
        double dx = x[j] - mean_x;
        double dy = y[j] - mean_y;
        cov += dx * dy;
        var_x += dx * dx;
        var_y += dy * dy;
      }
    }

    if (var_x > 0 && var_y > 0) {
      result[i] = cov / std::sqrt(var_x * var_y);
    }
  }

  return result;
}

// Fast conditional correlation computation
//' @title Fast Conditional Correlation
//' @description Compute correlation conditioned on a third variable being in a range.
//' @param x Numeric vector
//' @param y Numeric vector
//' @param z Conditioning variable
//' @param z_min Lower bound for z
//' @param z_max Upper bound for z
//' @return Correlation coefficient
//' @keywords internal
// [[Rcpp::export]]
double fast_conditional_cor(NumericVector x, NumericVector y, NumericVector z,
                            double z_min, double z_max) {
  int n = x.size();

  double sum_x = 0.0, sum_y = 0.0;
  int count = 0;

  // First pass: compute means in condition
  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(y[i]) &&
        !NumericVector::is_na(z[i]) && z[i] >= z_min && z[i] <= z_max) {
      sum_x += x[i];
      sum_y += y[i];
      count++;
    }
  }

  if (count < 3) return NA_REAL;

  double mean_x = sum_x / count;
  double mean_y = sum_y / count;

  // Second pass: compute covariance and variances
  double cov = 0.0, var_x = 0.0, var_y = 0.0;

  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(y[i]) &&
        !NumericVector::is_na(z[i]) && z[i] >= z_min && z[i] <= z_max) {
      double dx = x[i] - mean_x;
      double dy = y[i] - mean_y;
      cov += dx * dy;
      var_x += dx * dx;
      var_y += dy * dy;
    }
  }

  if (var_x <= 0 || var_y <= 0) return NA_REAL;

  return cov / std::sqrt(var_x * var_y);
}

// Fast entropy calculation for a 2D distribution
//' @title Fast 2D Entropy
//' @description Compute entropy of a 2D binned distribution.
//' @param x Numeric vector
//' @param y Numeric vector
//' @param n_bins Number of bins per dimension
//' @return Normalized entropy value
//' @keywords internal
// [[Rcpp::export]]
double fast_entropy_2d(NumericVector x, NumericVector y, int n_bins) {
  int n = x.size();

  // Find ranges
  double x_min = R_PosInf, x_max = R_NegInf;
  double y_min = R_PosInf, y_max = R_NegInf;

  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(y[i])) {
      if (x[i] < x_min) x_min = x[i];
      if (x[i] > x_max) x_max = x[i];
      if (y[i] < y_min) y_min = y[i];
      if (y[i] > y_max) y_max = y[i];
    }
  }

  double x_range = x_max - x_min;
  double y_range = y_max - y_min;

  if (x_range <= 0 || y_range <= 0) return 0.0;

  // Create bins
  std::vector<std::vector<int>> bins(n_bins, std::vector<int>(n_bins, 0));
  int total = 0;

  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(y[i])) {
      int bx = std::min((int)((x[i] - x_min) / x_range * n_bins), n_bins - 1);
      int by = std::min((int)((y[i] - y_min) / y_range * n_bins), n_bins - 1);
      bx = std::max(0, bx);
      by = std::max(0, by);
      bins[bx][by]++;
      total++;
    }
  }

  if (total == 0) return 0.0;

  // Compute entropy
  double entropy = 0.0;
  for (int i = 0; i < n_bins; i++) {
    for (int j = 0; j < n_bins; j++) {
      if (bins[i][j] > 0) {
        double p = (double)bins[i][j] / total;
        entropy -= p * std::log2(p);
      }
    }
  }

  // Normalize by max entropy
  double max_entropy = std::log2((double)(n_bins * n_bins));

  return entropy / max_entropy;
}

// Fast mutual information estimation
//' @title Fast Mutual Information
//' @description Estimate mutual information between two variables.
//' @param x Numeric vector
//' @param y Numeric vector
//' @param n_bins Number of bins for discretization
//' @return Mutual information estimate
//' @keywords internal
// [[Rcpp::export]]
double fast_mutual_info(NumericVector x, NumericVector y, int n_bins) {
  int n = x.size();

  // Find ranges
  double x_min = R_PosInf, x_max = R_NegInf;
  double y_min = R_PosInf, y_max = R_NegInf;

  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(y[i])) {
      if (x[i] < x_min) x_min = x[i];
      if (x[i] > x_max) x_max = x[i];
      if (y[i] < y_min) y_min = y[i];
      if (y[i] > y_max) y_max = y[i];
    }
  }

  double x_range = x_max - x_min;
  double y_range = y_max - y_min;

  if (x_range <= 0 || y_range <= 0) return 0.0;

  // Create marginal and joint distributions
  std::vector<int> x_marginal(n_bins, 0);
  std::vector<int> y_marginal(n_bins, 0);
  std::vector<std::vector<int>> joint(n_bins, std::vector<int>(n_bins, 0));
  int total = 0;

  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(y[i])) {
      int bx = std::min((int)((x[i] - x_min) / x_range * n_bins), n_bins - 1);
      int by = std::min((int)((y[i] - y_min) / y_range * n_bins), n_bins - 1);
      bx = std::max(0, bx);
      by = std::max(0, by);
      x_marginal[bx]++;
      y_marginal[by]++;
      joint[bx][by]++;
      total++;
    }
  }

  if (total == 0) return 0.0;

  // Compute mutual information
  double mi = 0.0;
  for (int i = 0; i < n_bins; i++) {
    for (int j = 0; j < n_bins; j++) {
      if (joint[i][j] > 0 && x_marginal[i] > 0 && y_marginal[j] > 0) {
        double p_joint = (double)joint[i][j] / total;
        double p_x = (double)x_marginal[i] / total;
        double p_y = (double)y_marginal[j] / total;
        mi += p_joint * std::log2(p_joint / (p_x * p_y));
      }
    }
  }

  return mi;
}

// Fast threshold detection for dormancy
//' @title Fast Threshold Detection
//' @description Find optimal threshold where correlation changes most.
//' @param x Numeric vector
//' @param y Numeric vector
//' @param n_thresholds Number of candidate thresholds to test
//' @return List with threshold value and correlation difference
//' @keywords internal
// [[Rcpp::export]]
List fast_threshold_detect(NumericVector x, NumericVector y, int n_thresholds) {
  int n = x.size();

  // Sort by x
  std::vector<std::pair<double, double>> data(n);
  for (int i = 0; i < n; i++) {
    data[i] = std::make_pair(x[i], y[i]);
  }
  std::sort(data.begin(), data.end());

  double best_threshold = NA_REAL;
  double best_diff = 0.0;

  // Test thresholds at quantile points
  for (int t = 1; t < n_thresholds; t++) {
    int split = n * t / n_thresholds;
    if (split < 10 || split > n - 10) continue;

    // Compute correlation below threshold
    double mean_x_lo = 0, mean_y_lo = 0;
    for (int i = 0; i < split; i++) {
      mean_x_lo += data[i].first;
      mean_y_lo += data[i].second;
    }
    mean_x_lo /= split;
    mean_y_lo /= split;

    double cov_lo = 0, var_x_lo = 0, var_y_lo = 0;
    for (int i = 0; i < split; i++) {
      double dx = data[i].first - mean_x_lo;
      double dy = data[i].second - mean_y_lo;
      cov_lo += dx * dy;
      var_x_lo += dx * dx;
      var_y_lo += dy * dy;
    }
    double cor_lo = (var_x_lo > 0 && var_y_lo > 0) ? cov_lo / std::sqrt(var_x_lo * var_y_lo) : NA_REAL;

    // Compute correlation above threshold
    double mean_x_hi = 0, mean_y_hi = 0;
    for (int i = split; i < n; i++) {
      mean_x_hi += data[i].first;
      mean_y_hi += data[i].second;
    }
    mean_x_hi /= (n - split);
    mean_y_hi /= (n - split);

    double cov_hi = 0, var_x_hi = 0, var_y_hi = 0;
    for (int i = split; i < n; i++) {
      double dx = data[i].first - mean_x_hi;
      double dy = data[i].second - mean_y_hi;
      cov_hi += dx * dy;
      var_x_hi += dx * dx;
      var_y_hi += dy * dy;
    }
    double cor_hi = (var_x_hi > 0 && var_y_hi > 0) ? cov_hi / std::sqrt(var_x_hi * var_y_hi) : NA_REAL;

    if (!NumericVector::is_na(cor_lo) && !NumericVector::is_na(cor_hi)) {
      double diff = std::abs(cor_hi - cor_lo);
      if (diff > best_diff) {
        best_diff = diff;
        best_threshold = data[split].first;
      }
    }
  }

  return List::create(
    Named("threshold") = best_threshold,
    Named("correlation_difference") = best_diff
  );
}
