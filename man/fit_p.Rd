% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step3_fit_p.R
\name{fit_p}
\alias{fit_p}
\title{Step 3: Optimizing parameters to fit real data}
\usage{
fit_p(
  data,
  id = NULL,
  n_trials = NULL,
  fit_model = list(TD, RSTD, Utility),
  funcs = NULL,
  model_name = c("TD", "RSTD", "Utility"),
  lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
  upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
  initial_params = NA,
  initial_size = 50,
  iteration = 10,
  seed = 123,
  nc = 1,
  algorithm
)
}
\arguments{
\item{data}{[data.frame] raw data. 
This data should include the following mandatory columns: 
 \itemize{
   \item "sub"
   \item "time_line" (e.g., "Block", "Trial")
   \item "L_choice"
   \item "R_choice"
   \item "L_reward"
   \item "R_reward"
   \item "sub_choose"
 }}

\item{id}{[vector] which subject is going to be analyzed.
is being analyzed. The value should correspond to an entry in the "sub" 
column, which must contain the subject IDs. 
e.g., `id = unique(data$Subject)`}

\item{n_trials}{[integer] number of total trials}

\item{fit_model}{[list] A collection of functions applied to fit models to the data.}

\item{funcs}{[vector] A character vector containing the names of all 
user-defined functions required for the computation.}

\item{model_name}{[list] the name of fit modals}

\item{lower}{[list] The lower bounds for model fit models}

\item{upper}{[list] The upper bounds for model fit models}

\item{initial_params}{[vector] Initial values for the free parameters. 
These need to be set only when using L-BFGS-B. Other algorithms 
automatically generate initial values.
for `L-BFGS-B`, `GenSA`, set `initial = c(0, 0, ...)`}

\item{initial_size}{[integer] Initial values for the free parameters. 
These need to be set only when using L-BFGS-B. Other algorithms 
automatically generate initial values.
for `Bayesian`, `GA`, set `initial = 50`}

\item{iteration}{[integer] the number of iteration}

\item{seed}{[integer] random seed. This ensures that the results are 
reproducible and remain the same each time the function is run. 
default: `seed = 123`}

\item{nc}{[integer] Number of CPU cores to use for parallel computation.}

\item{algorithm}{[character] Choose an algorithm package from
`L-BFGS-B`, `GenSA`, `GA`, `DEoptim`, `PSO`, `Bayesian`, `CMA-ES`.
In addition, any algorithm from the `nloptr` package is also
supported. If your chosen `nloptr` algorithm requires a local search,
you need to input a character vector. The first element represents
the algorithm used for global search, and the second element represents
the algorithm used for local search.}
}
\value{
The optimal parameters found by the algorithm for each subject,
 along with the model fit calculated using these parameters.
 This is returned as an object of class \code{binaryRL} containing results
 for all subjects with all models.
}
\description{
This function optimizes free parameters of reinforcement learning 
 models built with the `run_m` function. After constructing a 
 reinforcement learning model (a function with only ONE argument, 
 `params`), the `fit_p` function searches for the optimal values of 
 these free parameters.

 The function provides several optimization algorithms:
  \itemize{
    \item 1. L-BFGS-B (from `stats::optim`);
    \item 2. Simulated Annealing (`GenSA`);
    \item 3. Genetic Algorithm (`GA`);
    \item 4. Differential Evolution (`DEoptim`);
    \item 5. Particle Swarm Optimization (`pso`);
    \item 6. Bayesian Optimization (`mlrMBO`);
    \item 7. Covariance Matrix Adapting Evolutionary Strategy (`cmaes`);
    \item 8. Nonlinear Optimization (`nloptr`)
  }

 For more information, please refer to the GitHub repository:
 https://github.com/yuki-961004/binaryRL
}
