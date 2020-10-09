// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// compute_pdist2
arma::mat compute_pdist2(arma::mat& X, arma::mat& Y);
RcppExport SEXP _T4transport_compute_pdist2(SEXP XSEXP, SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_pdist2(X, Y));
    return rcpp_result_gen;
END_RCPP
}
// cpp_barysinkhorn14
arma::vec cpp_barysinkhorn14(arma::field<arma::mat>& listdXY, arma::field<arma::vec>& marginals, arma::vec weights, double p, double lambda, int maxiter, double abstol, bool printer, arma::vec initvec);
RcppExport SEXP _T4transport_cpp_barysinkhorn14(SEXP listdXYSEXP, SEXP marginalsSEXP, SEXP weightsSEXP, SEXP pSEXP, SEXP lambdaSEXP, SEXP maxiterSEXP, SEXP abstolSEXP, SEXP printerSEXP, SEXP initvecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::field<arma::mat>& >::type listdXY(listdXYSEXP);
    Rcpp::traits::input_parameter< arma::field<arma::vec>& >::type marginals(marginalsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< double >::type abstol(abstolSEXP);
    Rcpp::traits::input_parameter< bool >::type printer(printerSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type initvec(initvecSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_barysinkhorn14(listdXY, marginals, weights, p, lambda, maxiter, abstol, printer, initvec));
    return rcpp_result_gen;
END_RCPP
}
// cpp_sinkhorn13
Rcpp::List cpp_sinkhorn13(arma::vec a, arma::vec b, arma::mat dab, double lambda, double p, int maxiter, double abstol);
RcppExport SEXP _T4transport_cpp_sinkhorn13(SEXP aSEXP, SEXP bSEXP, SEXP dabSEXP, SEXP lambdaSEXP, SEXP pSEXP, SEXP maxiterSEXP, SEXP abstolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type a(aSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type b(bSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type dab(dabSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< double >::type abstol(abstolSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_sinkhorn13(a, b, dab, lambda, p, maxiter, abstol));
    return rcpp_result_gen;
END_RCPP
}
// image_barysinkhorn14
arma::vec image_barysinkhorn14(arma::mat& dxy, arma::field<arma::vec>& marginals, arma::vec weights, double p, double lambda, int maxiter, double abstol, bool printer, arma::vec initvec, int nthread);
RcppExport SEXP _T4transport_image_barysinkhorn14(SEXP dxySEXP, SEXP marginalsSEXP, SEXP weightsSEXP, SEXP pSEXP, SEXP lambdaSEXP, SEXP maxiterSEXP, SEXP abstolSEXP, SEXP printerSEXP, SEXP initvecSEXP, SEXP nthreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type dxy(dxySEXP);
    Rcpp::traits::input_parameter< arma::field<arma::vec>& >::type marginals(marginalsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< double >::type abstol(abstolSEXP);
    Rcpp::traits::input_parameter< bool >::type printer(printerSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type initvec(initvecSEXP);
    Rcpp::traits::input_parameter< int >::type nthread(nthreadSEXP);
    rcpp_result_gen = Rcpp::wrap(image_barysinkhorn14(dxy, marginals, weights, p, lambda, maxiter, abstol, printer, initvec, nthread));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_T4transport_compute_pdist2", (DL_FUNC) &_T4transport_compute_pdist2, 2},
    {"_T4transport_cpp_barysinkhorn14", (DL_FUNC) &_T4transport_cpp_barysinkhorn14, 9},
    {"_T4transport_cpp_sinkhorn13", (DL_FUNC) &_T4transport_cpp_sinkhorn13, 7},
    {"_T4transport_image_barysinkhorn14", (DL_FUNC) &_T4transport_image_barysinkhorn14, 10},
    {NULL, NULL, 0}
};

RcppExport void R_init_T4transport(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}