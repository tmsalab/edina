// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/edina.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// edina_Gibbs_Q
Rcpp::List edina_Gibbs_Q(const arma::mat& Y, unsigned int K, unsigned int burnin, unsigned int chain_length);
RcppExport SEXP _edina_edina_Gibbs_Q(SEXP YSEXP, SEXP KSEXP, SEXP burninSEXP, SEXP chain_lengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type K(KSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type burnin(burninSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type chain_length(chain_lengthSEXP);
    rcpp_result_gen = Rcpp::wrap(edina_Gibbs_Q(Y, K, burnin, chain_length));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_edina_edina_Gibbs_Q", (DL_FUNC) &_edina_edina_Gibbs_Q, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_edina(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}