#include <Rcpp.h>
using namespace Rcpp;

// https://stackoverflow.com/questions/54000015/r-package-with-both-c-and-cpp-files-with-rcpp
extern "C" SEXP transpose_impl(SEXP x, SEXP names_template);

// [[Rcpp::export]]
SEXP transpose_cpp(SEXP x, SEXP names_template) {
  return transpose_impl(x, names_template);
}
