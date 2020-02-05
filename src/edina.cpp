#include <edina.h>

// [[Rcpp::export]]
Rcpp::List edina_Gibbs_Q(const arma::mat &Y, unsigned int K,
                         unsigned int burnin = 1000,
                         unsigned int chain_length = 10000) {

    // Release
    return edina::edina_Gibbs_Q(Y, K, burnin, chain_length);
}
