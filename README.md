# Time-varying and Stable Early Warning Indicators: A Sparse Bayesian Logistic Regression Approach

GitHub page for the paper: XXXX (name of the paper to be updated)

## 1_createDB.r
This code downloads the JST data from Macrohistory database (http://data.macrohistory.net/JST/JSTdatasetR5.dta) and creates the master data under 4_data folder.

## 2_dynamic.r
This is the main code to estimate a sparse dynamic logistic regression model. The model assumes that 
$$q_{i,t} = X_{i,t}\beta_t + \varepsilon_{i,t}$$
where $q_{i,t}$ is the logit transformed probability of financial crisis ($Pr[crisis_{i,t}]$). This logistic regression model is common in the robustness check too. 

As the baseline model, here I assume that the coefficients are identical across countries contemporally, but evolves over time following a random walk with Gaussian errors (state equation).
$$\beta_t \sim N(\beta_{t-1}, \sigma)$$
which implicitly assumes that the coefficients smoothly and slowly evolves over time.

## 2_dynamic.stan
Stan code to run MCMC estimation for the basic model.

## util/plot_heat.r
This code contains the function to plot a heat map of the time-varying coefficients. 

## util/plot_dynamic.r
This code contains the function to draw line plots of significant coefficients with 90% posterior credible region.
