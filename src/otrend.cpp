#include <Rcpp.h>
#include <vector>
// [[Rcpp::plugins("cpp11")]]

// Update linear regression coefficients with new data
// [[Rcpp::export]]
void update_otrend_(
  double& x, double& value,
  double& n, double& nn,
  double& x_sum, double& n_sum,
  double& xn_sum, double& nn_sum,
  double& det
) {
  n++;
  nn = pow(n, 2);
  n_sum += n;
  nn_sum += nn;
  x_sum += x;
  xn_sum += x * n;
  det = n * nn_sum - pow(n_sum, 2);
  value = ((nn_sum * x_sum - xn_sum * n_sum) + (xn_sum * n - x_sum * n_sum)) / det;
}

// Run online linear regression on vector
// [[Rcpp::export]]
Rcpp::NumericVector otrend_(std::vector<double> x) {
  double n{};
  double nn{};
  double x_sum{};
  double n_sum{};
  double xn_sum{};
  double nn_sum{};
  double det{};
  std::vector<double> value(x.size());
  auto val{value.begin()};
  for (auto& x_i : x) {
    update_otrend_(x_i, *val, n, nn, x_sum, n_sum, xn_sum, nn_sum, det);
    val++;
  }
  value[0] = x[0];
  Rcpp::NumericVector value_num = Rcpp::wrap(value);
  value_num.attr("n") = n;
  value_num.attr("nn") = nn;
  value_num.attr("x_sum") = x_sum;
  value_num.attr("n_sum") = n_sum;
  value_num.attr("xn_sum") = xn_sum;
  value_num.attr("nn_sum") = nn_sum;
  return value_num;
}
