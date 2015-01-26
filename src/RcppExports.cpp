// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// frtype2
NumericMatrix frtype2(NumericVector X, NumericMatrix A, NumericMatrix B0);
RcppExport SEXP netmodr_frtype2(SEXP XSEXP, SEXP ASEXP, SEXP B0SEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type X(XSEXP );
        Rcpp::traits::input_parameter< NumericMatrix >::type A(ASEXP );
        Rcpp::traits::input_parameter< NumericMatrix >::type B0(B0SEXP );
        NumericMatrix __result = frtype2(X, A, B0);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// loggrowth
NumericVector loggrowth(NumericVector X, NumericVector R, NumericVector K);
RcppExport SEXP netmodr_loggrowth(SEXP XSEXP, SEXP RSEXP, SEXP KSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type X(XSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type R(RSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type K(KSEXP );
        NumericVector __result = loggrowth(X, R, K);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
