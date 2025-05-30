% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_tau.R
\name{func_tau}
\alias{func_tau}
\title{Function: Soft-Max Function}
\usage{
func_tau(LR, try, L_value, R_value, var1 = NA, var2 = NA, tau = 1, lambda)
}
\arguments{
\item{LR}{Are you calculating the probability for the left option or the 
right option?}

\item{try}{If the choice was random, the value is 1; if the choice was 
based on value, the value is 0.}

\item{L_value}{The value of the left option}

\item{R_value}{The value of the right option}

\item{var1}{[character] column name of extra variable 1. If your model uses 
more than just reward and expected value, and you need other information, 
such as whether the choice frame is Gain or Loss, then you can input the 
'Frame' column as var1 into the model.
e.g., `var1 = "Extra_Var1"`}

\item{var2}{[character] column name of extra variable 2. If one additional 
variable, var1, does not meet your needs, you can add another additional 
variable, var2, into your model.
e.g., `var2 = "Extra_Var2"`}

\item{tau}{[vector] Parameters used in the Soft-Max Function 
`prob_func` representing the sensitivity of the subject to the value 
 difference when making decisions. It determines the probability of selecting 
 the left option versus the right option based on their values. A larger 
 value of tau indicates greater sensitivity to the value difference between 
 the options. In other words, even a small difference in value will make the 
 subject more likely to choose the higher-value option. 
 e.g., `tau = c(0.5)`}

\item{lambda}{[vector] Extra parameters that may be used in functions. 
e.g., `lambda = c(0.4, 0.7, 20, 60)`}
}
\value{
The probability of choosing this option
}
\description{
Function: Soft-Max Function
}
\note{
When customizing these functions, please ensure that you do not modify 
the arguments. Instead, only modify the `if-else` statements or the internal 
logic to adapt the function to your needs.
}
