
// -----------------------------------------------------------------------------
// About this code
// Estimate a logit model without time-varying coefficients
// Allow for a discontinuous structural change of parameters
// -----------------------------------------------------------------------------

// input data
data {
  int                      I;      // sample size
  int                      p;      // number of covariates
  int                      Tmax;   // number of time variables
  matrix[I, p]             X;      // explanatory variable matrix
  int<lower=0, upper=1>    Y[I];   // crisis dummy
  int<lower=1, upper=Tmax> Tid[I]; // time index of each sample
}

// parameters accepted by the model
parameters {
  // main
  matrix[p, Tmax]          beta;  // matrix of coefficients including intercept
  matrix<lower=0>[p, Tmax] theta; // matrix of sd of coefficients (controls smoothness)
  // latent parameters for horseshoe prior: for theta
  matrix<lower=0>[p, Tmax] lambda_theta;
  vector<lower=0>[Tmax]    tau_theta;
  // latent parameters for horseshoe prior: for the initial values of beta
  vector<lower=0>[p]       lambda_beta1;
  real<lower=0>            tau_beta1;
}

// transformed parameters
transformed parameters {
  vector[I] q;    // main equation of logistic regression
  for (i in 1:I) {
    q[i] = inv_logit(X[i] * beta[, Tid[i]]);
  }
}

// model part
model {
  // priors for theta: horseshoe prior
  for (t in 2:Tmax){
    lambda_theta[, t] ~ cauchy(0, 1);
    tau_theta[t]      ~ cauchy(0, 1);
    theta[, t]        ~ normal(0, tau_theta[t] * lambda_theta[, t]);
  }
  // priors for the initial beta: horseshoe prior
  lambda_beta1 ~ cauchy(0, 1);
  tau_beta1    ~ cauchy(0, 1);
  beta[, 1]    ~ normal(0, tau_beta1 * lambda_beta1);
  // state equation: let coefficients evolve over time
  for (t in 2:Tmax) {
    beta[, t] ~ normal(beta[, t-1], theta[, t]);
  }
  // logistic regression likelihood
  Y ~ bernoulli(q); // logistic regression
}
