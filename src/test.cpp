#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
int test_some_adding_of_numbers(int a, int b) {
  return a + b;
}