// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// simple example of creating two matrices and
// returning the result of an operatioon on them
//
// via the exports attribute we tell Rcpp to make this function
// available from R
//
// [[Rcpp::export]]
arma::mat mm(arma::mat A, arma::mat B) { return A * B; }

// [[Rcpp::export]]
arma::mat mmm(arma::mat A, arma::mat B, arma::mat C) { return A * B * C; }

// [[Rcpp::export]]
arma::mat mmmm(arma::mat A, arma::mat B, arma::mat C, arma::mat D) {
	return A * B * C * D; }

// [[Rcpp::export]]
arma::mat minv(arma::mat A) { return inv(A); }
