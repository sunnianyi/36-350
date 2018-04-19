generate_data = function(n, p) {
  list(covariates = matrix(rnorm(n*p), n, p), responses = rnorm(n))
}

model_select = function(covariates, responses, cutoff) {
  p.values = head(summary(lm(responses ~ covariates))$coefficients[,4], -1)
  if (all(p.values > cutoff)) return(c()) # none of the p-values meet the cutoff
  summary(lm(responses ~ covariates[,p.values <= cutoff]))$coefficients[,4] # alt regression
}

run_simulation = function(n_trials, n, p, cutoff) {
  p.values = c()
  for (i in 1:n_trials) {
    data = generate_data(n, p)
    p.values = c(p.values, as.vector(model_select(data$covariates, data$responses, cutoff)))
  }
  hist(p.values, xlab="p-values", main=paste("n = ", n, ", p = ", p, sep=''))
}

par(mfrow=c(3,3), mar=c(4,4,4,2))
for (n in c(100, 1000, 10000)) {
  for (p in c(10, 20, 50)) {
    run_simulation(100, n, p, 0.05)
  }
}
