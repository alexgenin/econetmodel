// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// select_range
NumericMatrix select_range(NumericMatrix inmat, NumericVector range);
RcppExport SEXP netmodr_select_range(SEXP inmatSEXP, SEXP rangeSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type inmat(inmatSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type range(rangeSEXP);
    __result = Rcpp::wrap(select_range(inmat, range));
    return __result;
END_RCPP
}
// zero_below_cpp
NumericMatrix zero_below_cpp(NumericMatrix old, double eps);
RcppExport SEXP netmodr_zero_below_cpp(SEXP oldSEXP, SEXP epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type old(oldSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    __result = Rcpp::wrap(zero_below_cpp(old, eps));
    return __result;
END_RCPP
}
