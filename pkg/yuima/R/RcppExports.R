# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

evalKernelCpp <- function(Integrand2, Integrand2expr, myenvd1, myenvd2, ExistdN, ExistdX, gridTime, dimCol, NameCol, JumpTimeName) {
    .Call('_yuima_evalKernelCpp', PACKAGE = 'yuima', Integrand2, Integrand2expr, myenvd1, myenvd2, ExistdN, ExistdX, gridTime, dimCol, NameCol, JumpTimeName)
}

evalKernelCpp2 <- function(Integrand2, Integrand2expr, myenvd1, myenvd2, CondIntensity, NameCountingVar, Namecovariates, ExistdN, ExistdX, gridTime, dimCol, NameCol, JumpTimeName) {
    .Call('_yuima_evalKernelCpp2', PACKAGE = 'yuima', Integrand2, Integrand2expr, myenvd1, myenvd2, CondIntensity, NameCountingVar, Namecovariates, ExistdN, ExistdX, gridTime, dimCol, NameCol, JumpTimeName)
}

sqnorm <- function(x) {
    .Call('_yuima_sqnorm', PACKAGE = 'yuima', x)
}

makeprop <- function(mu, sample, low, up) {
    .Call('_yuima_makeprop', PACKAGE = 'yuima', mu, sample, low, up)
}

is_zero <- function(x) {
    .Call('_yuima_is_zero', PACKAGE = 'yuima', x)
}

cpp_split <- function(str, sep) {
    .Call('_yuima_cpp_split', PACKAGE = 'yuima', str, sep)
}

cpp_paste <- function(x, y, sep) {
    .Call('_yuima_cpp_paste', PACKAGE = 'yuima', x, y, sep)
}

cpp_collapse <- function(str, sep) {
    .Call('_yuima_cpp_collapse', PACKAGE = 'yuima', str, sep)
}

cpp_outer <- function(x, y) {
    .Call('_yuima_cpp_outer', PACKAGE = 'yuima', x, y)
}

cpp_ito_outer <- function(x, y) {
    .Call('_yuima_cpp_ito_outer', PACKAGE = 'yuima', x, y)
}

cpp_to_str <- function(i) {
    .Call('_yuima_cpp_to_str', PACKAGE = 'yuima', i)
}

cpp_label <- function(I) {
    .Call('_yuima_cpp_label', PACKAGE = 'yuima', I)
}

cpp_ito_product <- function(idx, dZ, Z_K, K, d, a, p, q = 0L) {
    .Call('_yuima_cpp_ito_product', PACKAGE = 'yuima', idx, dZ, Z_K, K, d, a, p, q)
}

cpp_E <- function(str) {
    .Call('_yuima_cpp_E', PACKAGE = 'yuima', str)
}

cpp_ito <- function(K_set, dZ, Z_K, d, r) {
    .Call('_yuima_cpp_ito', PACKAGE = 'yuima', K_set, dZ, Z_K, d, r)
}

W1 <- function(crossdx, b, A, h) {
    .Call('_yuima_W1', PACKAGE = 'yuima', crossdx, b, A, h)
}

W2 <- function(dx, b, h) {
    .Call('_yuima_W2', PACKAGE = 'yuima', dx, b, h)
}

Irregular_PseudoLoglik_COG <- function(lengthObs, B, Btilde, InvBtilde, a0, bq, a1, V, PseudologLik, ta, state, stateMean, e, DeltaG2, Deltat) {
    .Call('_yuima_Irregular_PseudoLoglik_COG', PACKAGE = 'yuima', lengthObs, B, Btilde, InvBtilde, a0, bq, a1, V, PseudologLik, ta, state, stateMean, e, DeltaG2, Deltat)
}

detcpp <- function(A) {
    .Call('_yuima_detcpp', PACKAGE = 'yuima', A)
}

Smake <- function(b, d) {
    .Call('_yuima_Smake', PACKAGE = 'yuima', b, d)
}

solvecpp <- function(A) {
    .Call('_yuima_solvecpp', PACKAGE = 'yuima', A)
}

sub_f <- function(S, b) {
    .Call('_yuima_sub_f', PACKAGE = 'yuima', S, b)
}

likndim <- function(dx, b, A, h) {
    .Call('_yuima_likndim', PACKAGE = 'yuima', dx, b, A, h)
}

residualCpp <- function(dx, a, b, w, h) {
    .Call('_yuima_residualCpp', PACKAGE = 'yuima', dx, a, b, w, h)
}

