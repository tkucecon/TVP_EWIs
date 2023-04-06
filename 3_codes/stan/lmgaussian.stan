
// -----------------------------------------------------------------------------
// About this code
// Estimate a linear model with time-varying coefficients
// -----------------------------------------------------------------------------

// input data
data {
  int                      I;      // sample size
  int                      p;      // number of covariates
  int                      Tmax;   // number of time variables
  matrix[I, p]             X;      // explanatory variable matrix
  real                     Y[I];   // bank equity return
  int<lower=1, upper=Tmax> Tid[I]; // time index of each sample
}

// parameters accepted by the model
parameters {
  // main
  matrix[p, Tmax] beta;     // matrix of coefficients including intercept
  vector<lower=0>[p] theta; // vector of sd of coefficients (controls smoothness)
  real<lower=0> sigma;      // sigma of the likelihood part 
  // latent parameters for horseshoe prior: for theta
  vector<lower=0>[p] lambda_theta;
  real<lower=0> tau_theta;
  // latent parameters for horseshoe prior: for the initial values of beta
  vector<lower=0>[p] lambda_beta1;
  real<lower=0> tau_beta1;
}

// model part
model {
  // priors for theta: horseshoe prior
  lambda_theta ~ cauchy(0, 1);
  tau_theta    ~ cauchy(0, 1);
  theta        ~ normal(0, tau_theta * lambda_theta);
  // priors for the initial beta: horseshoe prior
  lambda_beta1 ~ cauchy(0, 1);
  tau_beta1    ~ cauchy(0, 1);
  beta[, 1]    ~ normal(0, tau_beta1 * lambda_beta1);
  // state equation: let coefficients evolve over time
  for (t in 2:Tmax) {
    beta[, t] ~ normal(beta[, t-1], theta);
  }
  // main likelihood equation of linear regression
  for (i in 1:I) {
    Y[i] ~ normal(X[i] * beta[, Tid[i]], sigma);
  }
}
