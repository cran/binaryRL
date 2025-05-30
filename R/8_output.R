#' Summary the Results
#'
#' @param data [data.frame] A data frame resulting from the 'step7' process of the `digits` function. 
#' 
#' @param n_params [integer] The number of free parameters in your model. 
#' 
#' @param n_trials [integer] The total number of trials in your experiment.
#' 
#' @param lambda [vector] Extra parameters that may be used in functions. 
#'  e.g., `lambda = c(0.4, 0.7, 20, 60)`
#' 
#' @param gamma [vector] Parameters used in the `util_func` (Utility Function), 
#'  often referred to as the discount rate. For example, 
#'  `utility = gamma * reward`, if gamma < 1, it indicates that people 
#'  tend to discount the objective reward. Provide the value as a vector 
#'  e.g., `gamma = c(0.7)`
#' 
#' @param eta [vector] Parameters used in the `rate_func` (Learning Rate Function), 
#'  representing the rate at which the subject updates the 
#'  difference (prediction error) between the reward and the expected value 
#'  in the subject's mind. In the TD model, there is a single learning rate 
#'  throughout the experiment. In the RSTD model, two different learning rates 
#'  are used when the reward is higher or lower than the expected value.
#'  e.g., `eta = c(0.3, 0.7)`
#' 
#' @param epsilon [vector] Parameters used in the `expl_func` (Exploration Function), 
#'  determining whether the subject makes decisions based on the relative values 
#'  of the left and right options, or chooses completely randomly. For example, 
#'  when epsilon = 0.1, it means the subject has a 10% chance of making a 
#'  completely random choice and a 90% chance of choosing based on the values 
#'  of the options.
#'  e.g., `epsilon = c(0.1)`
#' 
#' @param tau [vector] Parameters used in the `prob_func` (Soft-Max Function), 
#'  representing the sensitivity of the subject to the value difference when 
#'  making decisions. It determines the probability of selecting the left option 
#'  versus the right option based on their values. A larger value of tau 
#'  indicates greater sensitivity to the value difference between the options. 
#'  In other words, even a small difference in value will make the subject more 
#'  likely to choose the higher-value option. 
#'  e.g., `tau = c(0.5)`
#'
#' @returns binaryRL[list]:
#'   \itemize{
#'     \item{\code{data}: output data frame with all information}
#'     \item{\code{params}: all parameters value}
#'     \item{\code{numeric}: ACC}
#'     \item{\code{numeric}: LogL}
#'     \item{\code{numeric}: AIC}
#'     \item{\code{numeric}: BIC}
#'   }
#'   
#' @noRd
#' 
output <- function(
    data, 
    n_params, n_trials, 
    gamma, eta, epsilon, tau, lambda
){
  params <- list(
    lambda = c(lambda),
    gamma = c(gamma),
    eta = c(eta), 
    epsilon = c(epsilon),
    tau = c(tau)
  )
  
  # 因为第一行用来填写初始值了, 所以需要重新把第二行初始化成第一行
  rownames(data) <- NULL 
  mean_ACC <- round(mean(data$ACC), 4) * 100
  sum_LL <- round(sum(data$L_logl) + sum(data$R_logl), digits = 2)
  AIC <- round(2 * n_params - 2 * sum_LL, digits = 2)
  BIC <- round(n_params * log(n_trials) - 2 * sum_LL, digits = 2)
  
  res <- list(
    data = data,
    params = params,
    acc = mean_ACC,
    ll = sum_LL,
    aic = AIC,
    bic = BIC
  )
  
  class(res) <- c("binaryRL")
  
  return(res)
}
