// [[Rcpp::plugins(cpp11)]]

#include <algorithm>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector calendar_contains(const IntegerVector& calendar, const IntegerVector& date)
{ //  use xxxVector to avoid copy of parameter
  LogicalVector res(date.size());
  for (auto i = 0; i < date.size(); ++i)
  {
    res[i] = std::binary_search(calendar.begin(), calendar.end(), date[i]);
    // if exist, return TRUE, others FALSE
  }
  return res;
}

// [[Rcpp::export]]
IntegerVector calendar_gt(const IntegerVector& calendar, const IntegerVector& date)
{
  IntegerVector out(date.size(), NA_INTEGER);
  for (auto i = 0; i < date.size(); ++i) {
    // return first value pointer that is > given value
    const auto& upper = std::upper_bound(calendar.begin(), calendar.end(), date[i]);
    if (upper != calendar.end()) {
      out[i] = *upper;
    }
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector calendar_gte(const IntegerVector& calendar, const IntegerVector& date, int n)
{
  IntegerVector out(date.size(), NA_INTEGER);
  for (auto i = 0; i < date.size(); ++i) {
    // return value pointer that is >=  given value
    const auto& lower = std::lower_bound(calendar.begin(), calendar.end(), date[i]);
    if (lower + n != calendar.end()) {
      out[i] = *(lower + n);
    }
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector calendar_lte(const IntegerVector& calendar, const IntegerVector& date, int n)
{
  IntegerVector out(date.size(), NA_INTEGER);
  for (auto i = 0; i < date.size(); ++i) {
    const auto& upper = std::upper_bound(calendar.begin(), calendar.end(), date[i]);
    if (upper + n != calendar.begin()) {
      out[i] = *(upper - 1 + n);
    }
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector calendar_lt(const IntegerVector& calendar, const IntegerVector& date)
{
  IntegerVector out(date.size(), NA_INTEGER);
  for (auto i = 0; i < date.size(); ++i) {
    const auto& upper = std::upper_bound(calendar.begin(), calendar.end(), date[i]);
    if (upper != calendar.begin()) { // make sure no out of boundary
      if (*(upper - 1) < date[i]) {
        out[i] = *(upper - 1);
      } else if (upper - 2 != calendar.begin()) {
        out[i] = *(upper - 2);
      }
    }
  }
  return out;
}
