generate_data = function(n, p) {
  list(covariates = matrix(rnorm(n*p), n, p), responses = rnorm(n))
}