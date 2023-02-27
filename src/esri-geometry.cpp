#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// TODO make sfg rcpp functions


// [[Rcpp::export]]
List sfc_point_xy(List points) {

  int n = points.length();

  List res(n);

  for (int i = 0; i < n; i++) {
    NumericVector row = points[i];
    List obs = List::create(Named("x", row[0]), Named("y", row[1]));
    res[i] = obs;
  }

  return res;
}


// [[Rcpp::export]]
List sfc_point_xyz(List points) {
  int n = points.length();

  List res(n);

  for (int i = 0; i < n; i++) {
    NumericVector row = points[i];
    List obs = List::create(
      Named("x", row[0]),
      Named("y", row[1]),
      Named("z", row[2])
    );
    res[i] = obs;
  }

  return res;
}

// [[Rcpp::export]]
List sfc_point_xyzm(List points) {
  int n = points.length();

  List res(n);

  for (int i = 0; i < n; i++) {
    NumericVector row = points[i];
    List obs = List::create(
      Named("x", row[0]),
      Named("y", row[1]),
      Named("z", row[2]),
      Named("m", row[3])
    );
    res[i] = obs;
  }

  return res;
}



// adapted from from https://stackoverflow.com/questions/68397853/how-to-check-if-a-name-exists-in-rcpp-list-object
bool contains(std::string s, std::vector<string> nv) {
  for (int i=0; i < nv.size(); i++) {
    if (std::string(nv[i]) == s) {
      return true;
    }
  }
  return false;
}

// [[Rcpp::export]]
List sfc_point_impl(List points) {

  std::vector<string> attr_names = points.attributeNames();

  bool has_z = contains("z_range", attr_names);
  bool has_m = contains("m_range", attr_names);

  if (has_m) {
    List res = sfc_point_xyzm(points);
    return res;
  } else if (has_z) {
    List res = sfc_point_xyz(points);
    return res;
  } else {
    List res = sfc_point_xy(points);
    return res;
  }

}

// [[Rcpp::export]]
List sfc_multipoint_impl(List mpoints) {
  int n = mpoints.length();

  List res(n);

  for (int i = 0; i < n; i++) {
    NumericMatrix mpoint = mpoints[i];
    // print(mpoint);

    int nn = mpoint.nrow();

    List obs(nn);

    for (int i = 0; i < nn; i++) {
      NumericVector row = mpoint(i, _);
      obs[i] = row;
    }

    res[i] =  List::create(Named("points", obs));
  }

  return res;
}


// [[Rcpp::export]]
List sfc_linestring_impl(List mpoints) {
  int n = mpoints.length();

  List res(n);

  for (int i = 0; i < n; i++) {
    NumericMatrix mpoint = mpoints[i];
    // print(mpoint);

    int nn = mpoint.nrow();

    List obs(nn);

    for (int i = 0; i < nn; i++) {
      NumericVector row = mpoint(i, _);
      obs[i] = row;
    }

    res[i] = List::create(Named("paths", obs));
  }

  return res;
}



// [[Rcpp::export]]
List sfc_multilinestring_inner_impl(List mpoints) {
  int n = mpoints.length();

  List res(n);

  for (int i = 0; i < n; i++) {
    NumericMatrix mpoint = mpoints[i];

    int nn = mpoint.nrow();

    List obs(nn);

    for (int i = 0; i < nn; i++) {
      NumericVector row = mpoint(i, _);
      obs[i] = row;
    }

    res[i] = obs;
  }

  return res;
}


// [[Rcpp::export]]
List sfc_multilinestring_impl(List mlines) {
  int n = mlines.length();

  List res(n);

  // set names
  // CharacterVector pnt = "paths";
  // CharacterVector nms = Rcpp::rep(pnt, n);
  // res.attr("names") = nms;

  for (int i = 0; i < n; i++) {
    List line = mlines[i];
    //int nn = line.length();

    res[i] = List::create(Named("paths", sfc_multilinestring_inner_impl(line)));
  }

  return res;
}


// [[Rcpp::export]]
List sfg_polygon_impl(List mply) {

  int n = mply.length();

  // preallocate result list
  List res(n);

  for (int i = 0; i < n; i++) {

    NumericMatrix mat = mply[i];

    int nn = mat.nrow();

    List obs(nn);

    for (int i = nn -1; i >= 0; i--) {
      obs[i] = mat(i, _);
    };

    res[i] = obs;

  }

  return List::create(Named("rings", res));

}



// [[Rcpp::export]]
List sfc_polygon_impl(List mply) {
  int n = mply.length();

  List res(n);

  for (int i = 0; i < n; i++) {
    List poly = mply[i];

    res[i] = sfg_polygon_impl(poly);
  }
  return res;
}




// multipolgyons need to iterate through each feature
// then through each component polygon
// This iterates through a single multipolygon's component polygon
// each polygon is a numeric matrix which is extracted
// [[Rcpp::export]]
List sfg_multipolygon_inner_impl(List mply) {
  int n = mply.length();

  // preallocate result list
  List res(n);

  for (int i = 0; i < n; i++) {

    NumericMatrix mat = mply[i];

    int nn = mat.nrow();

    List obs(nn);

    for (int i = nn -1; i >= 0; i--) {
      obs[i] = mat(i, _);
    };

    res[i] = obs;

  }

  return res;
}


// [[Rcpp::export]]
List sfg_multipolygon_impl(List mply) {
  int n = mply.length();

  List res(n);
  for (int i = 0; i < n; i++) {
    List poly = mply[i];
    res[i] = sfg_multipolygon_inner_impl(poly)[0];
  }
  return List::create(Named("rings", res));
}


// [[Rcpp::export]]
List sfc_multipolygon_impl(List mply) {

  int n = mply.length();

  // preallocate result list
  // each MULTIPOLYGON feature
  List res(n);

  for (int i = 0; i < n; i++) {
    List mpoly = mply[i];
    res[i] = sfg_multipolygon_impl(mpoly);
  }

  return res;

}


// List sfc_multipolygon_impl(List mply) {
//   int n = mply.length();
//
//   // preallocate result list
//   // each MULTIPOLYGON feature
//   List res(n);
//
//   for (int i = 0; i < n; i++) {
//
//     //
//     List mpoly = mply[i];
//
//     int nn = mpoly.length();
//
//     // POLYGON comonent
//     List res_mply = List(nn);
//
//     for (int j = 0; j < nn; j++) {
//       List poly = mpoly[j];
//       // Rcpp::print(poly);
//       res_mply[j] = sfc_multipolygon_inner_impl(poly);
//     }
//
//     // Rcpp::print(res_mply);
//     res[i] = res_mply;
//   }
//
//   // set names
//   CharacterVector rng = "rings";
//   CharacterVector nms = Rcpp::rep(rng, n);
//   res.attr("names") = nms;
//
//   return res;
//
// }
