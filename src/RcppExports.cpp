// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// mm
arma::mat mm(arma::mat A, arma::mat B);
RcppExport SEXP _optimalclpm_mm(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(mm(A, B));
    return rcpp_result_gen;
END_RCPP
}
// mmm
arma::mat mmm(arma::mat A, arma::mat B, arma::mat C);
RcppExport SEXP _optimalclpm_mmm(SEXP ASEXP, SEXP BSEXP, SEXP CSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type B(BSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type C(CSEXP);
    rcpp_result_gen = Rcpp::wrap(mmm(A, B, C));
    return rcpp_result_gen;
END_RCPP
}
// mmmm
arma::mat mmmm(arma::mat A, arma::mat B, arma::mat C, arma::mat D);
RcppExport SEXP _optimalclpm_mmmm(SEXP ASEXP, SEXP BSEXP, SEXP CSEXP, SEXP DSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type B(BSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type C(CSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type D(DSEXP);
    rcpp_result_gen = Rcpp::wrap(mmmm(A, B, C, D));
    return rcpp_result_gen;
END_RCPP
}
// minv
arma::mat minv(arma::mat A);
RcppExport SEXP _optimalclpm_minv(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(minv(A));
    return rcpp_result_gen;
END_RCPP
}
