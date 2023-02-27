#include <Rcpp.h>
using namespace Rcpp;


// these two functions are modifications of the bbox C++ implementation in
// sf. They've been hard code / changed to handle z and m.
// note that m is actually dropped in creating a polygon bounding box
// thats becasue the M dimension is not actually a dimension but rather an associated
// measurement

// [[Rcpp::export]]
Rcpp::NumericVector get_envelope_z(Rcpp::List sf, int depth = 0) {
  Rcpp::NumericVector bb(6);
  bb[0] = bb[1] = bb[2] = bb[3] = bb[4] = bb[5] = NA_REAL;
  auto n = sf.size();

  switch(depth) {
  case 0: // points:
    for (decltype(n) i = 0; i < n; i++) {
      Rcpp::NumericVector pt = sf[i];
      print(pt);
      if (i == 0) {
        bb[0] = bb[2] = pt[0];
        bb[1] = bb[3] = pt[1];
      } else {
        bb[0] = std::min(pt[0],bb[0]); // xmin
        bb[1] = std::min(pt[1],bb[1]); // y min
        bb[2] = std::max(pt[0],bb[2]); // x max
        bb[3] = std::max(pt[1],bb[3]); // y max
        bb[4] = std::min(pt[2],bb[4]); // z min
        bb[5] = std::max(pt[2],bb[5]); // z max
      }
    }
    break;

  case 1: { // list of matrices:
      bool initialised = false;
      for (decltype(n) i = 0; i < n; i++) {
        Rcpp::NumericMatrix m = sf[i];
        auto rows = m.nrow();

        if (rows > 0) { // non-empty:
          if (! initialised) { // initialize:
            bb[0] = bb[2] = m(0,0);
            bb[1] = bb[3] = m(0,1);
            bb[4] = bb[5] = m(0,2);
            initialised = true;
          }
          for (decltype(rows) j = 0; j < rows; j++) {
            bb[0] = std::min(m(j,0),bb[0]);
            bb[1] = std::min(m(j,1),bb[1]);
            bb[2] = std::max(m(j,0),bb[2]);
            bb[3] = std::max(m(j,1),bb[3]);
            bb[4] = std::min(m(j, 2), bb[4]);
            bb[5] = std::max(m(j, 2), bb[5]);
          }
        }
      }
    }
    break;

  default: // recursive list
    for (decltype(n) i = 0; i < n; i++) {
      Rcpp::NumericVector bbi = get_envelope_z(sf[i], depth - 1); // recurse
      if (! Rcpp::NumericVector::is_na(bbi[0])) {
        if (i == 0) {
          bb[0] = bbi[0];
          bb[1] = bbi[1];
          bb[2] = bbi[2];
          bb[3] = bbi[3];
          bb[4] = bbi[4];
          bb[5] = bbi[5];

        } else {
          bb[0] = std::min(bbi[0],bb[0]);
          bb[1] = std::min(bbi[1],bb[1]);
          bb[2] = std::max(bbi[2],bb[2]);
          bb[3] = std::max(bbi[3],bb[3]);
          bb[4] = std::min(bbi[4], bb[4]);
          bb[5] = std::max(bbi[4], bb[5]);
        }
      }
    }
    break;
  }
  return bb;
}


// [[Rcpp::export]]
Rcpp::NumericVector get_envelope_zm(Rcpp::List sf, int depth = 0) {
  Rcpp::NumericVector bb(8);
  bb[0] = bb[1] = bb[2] = bb[3] = bb[4] = bb[5] = bb[6] = bb[7] = NA_REAL;
  auto n = sf.size();

  switch(depth) {
  case 0: // points:
    for (decltype(n) i = 0; i < n; i++) {
      Rcpp::NumericVector pt = sf[i];
      print(pt);
      if (i == 0) {
        bb[0] = bb[2] = pt[0];
        bb[1] = bb[3] = pt[1];
      } else {
        bb[0] = std::min(pt[0],bb[0]); // xmin
        bb[1] = std::min(pt[1],bb[1]); // y min
        bb[2] = std::max(pt[0],bb[2]); // x max
        bb[3] = std::max(pt[1],bb[3]); // y max
        bb[4] = std::min(pt[2],bb[4]); // z min
        bb[5] = std::max(pt[2],bb[5]); // z max
        bb[6] = std::min(pt[3],bb[6]); // m min
        bb[7] = std::max(pt[3],bb[7]); // m max
      }
    }
    break;

  case 1: { // list of matrices:
      bool initialised = false;
      for (decltype(n) i = 0; i < n; i++) {
        Rcpp::NumericMatrix m = sf[i];
        auto rows = m.nrow();

        if (rows > 0) { // non-empty:
          if (! initialised) { // initialize:
            bb[0] = bb[2] = m(0,0);
            bb[1] = bb[3] = m(0,1);
            bb[4] = bb[5] = m(0,2);
            bb[6] = bb[7] = m(0,3);
            initialised = true;
          }
          for (decltype(rows) j = 0; j < rows; j++) {
            bb[0] = std::min(m(j,0),bb[0]);
            bb[1] = std::min(m(j,1),bb[1]);
            bb[2] = std::max(m(j,0),bb[2]);
            bb[3] = std::max(m(j,1),bb[3]);
            bb[4] = std::min(m(j, 2), bb[4]);
            bb[5] = std::max(m(j, 2), bb[5]);
            bb[6] = std::min(m(j, 3), bb[6]);
            bb[7] = std::max(m(j, 3), bb[7]);
          }
        }
      }
    }
    break;

  default: // recursive list
    for (decltype(n) i = 0; i < n; i++) {
      Rcpp::NumericVector bbi = get_envelope_z(sf[i], depth - 1); // recurse
      if (! Rcpp::NumericVector::is_na(bbi[0])) {
        if (i == 0) {
          bb[0] = bbi[0];
          bb[1] = bbi[1];
          bb[2] = bbi[2];
          bb[3] = bbi[3];
          bb[4] = bbi[4];
          bb[5] = bbi[5];
          bb[6] = bbi[6];
          bb[7] = bbi[7];
        } else {
          bb[0] = std::min(bbi[0],bb[0]);
          bb[1] = std::min(bbi[1],bb[1]);
          bb[2] = std::max(bbi[2],bb[2]);
          bb[3] = std::max(bbi[3],bb[3]);
          bb[4] = std::min(bbi[4], bb[4]);
          bb[5] = std::max(bbi[5], bb[5]);
          bb[6] = std::min(bbi[6], bb[6]);
          bb[7] = std::max(bbi[6], bb[6]);
        }
      }
    }
    break;
  }
  return bb;
}
