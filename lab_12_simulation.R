generate_data = function(n, p) {
  list(covariates = matrix(rnorm(n*p), n, p), responses = rnorm(n))
}

model_select = function(covariates, responses, cutoff) {
  p.values = summary(lm(responses ~ covariates))$coefficients[,4][-1] # remove intercept
  if (all(p.values > cutoff)) return(c()) # none of the p-values meet the cutoff
  summary(lm(responses ~ covariates[,p.values <= cutoff]))$coefficients[,4] # alt regression
}

