library(nloptr)

#' Generlised beta distribution
#'
#' @param alpha 
#'
#' @return
#' @export
#'
#' @examples
beta2 <- function(alpha) {
  prod(gamma(alpha)) / gamma(sum(alpha))
}



# Objective Function
eval_f <- function(x, shares) {
  # from:  https://en.wikipedia.org/wiki/Dirichlet_distribution#Entropy
  # calculates the negative entropy of a dirichlet distribution given `shares` (sum to 1) and a concentration parameter `x`, so that `alpha = shares * x`
  alpha <- x * shares
  K <- length(alpha)
  psi <- digamma(alpha)
  alpha0 <- sum(alpha)
  (-(log(beta2(alpha)) + (alpha0 - K) * digamma(alpha0) - sum((alpha - 1) * psi)))
}

#' Finds the Gamma value which maximises the entropy of a Dirichlet distribution
#' with given alphas (=sector shares), using the nloptr optimazation package. 
#'
#' @param shares 
#' @param eval_f 
#' @param x0 
#' @param bounds 
#' @param local_opts 
#' @param opts 
#'
#' @return
#' @export
#'
#' @examples
find_gamma_maxent <- function(shares,
                              eval_f, 
                              x0 = 1, # initial value of mu
                              bounds = c(0.001, 300),
                              local_opts = list( "algorithm" = "NLOPT_LD_MMA", # optim options
                                                 "xtol_rel" = 1.0e-4 ),
                              opts = list( "algorithm"= "NLOPT_GN_ISRES",
                                           "xtol_rel"= 1.0e-4,
                                           "maxeval"= 3E5,
                                           "local_opts" = local_opts,
                                           "print_level" = 0 )
                              
) {
  # lower and upper bounds
  lb <- bounds[1]
  ub <- bounds[2]
  
  # Run the optimizer
  res <- nloptr ( x0 = x0,
                  eval_f = eval_f,
                  #   eval_g_eq = eval_g,
                  lb = lb, 
                  ub = ub,
                  opts = opts, 
                  shares = shares
  )
  
  if (res$status == 5) {
    warning("Optimization stopped because maxeval was reached. Re-run optimizer and capture individual runs.")
    # Optimization stopped because maxeval was reached.
    # Run optim again: But capture print output this time to find the lowest value the optimizer has ever reached
    opts$print_level = 3
    output = capture.output(nloptr ( x0 = x0,
                                     eval_f = eval_f,
                                     #   eval_g_eq = eval_g,
                                     lb = lb, 
                                     ub = ub,
                                     opts = opts, 
                                     shares = shares
    ))
    runs = extract_nloptr_runs(output)
    
    res_orig = res
    res = list()
    res$nloptr = res_orig
    res$solution = runs[which.min(fx)]$x
    res$objective = runs[which.min(fx)]$fx
    
  }
  return(res)
}

#' from: https://stackoverflow.com/questions/68819019/nloptr-not-returning-lowest-solution-so-far-after-reaching-maxeval
#'
#' @param nloptr_output 
#'
#' @return
#' @export
#'
#' @examples
extract_nloptr_runs <- function(nloptr_output) {
  x <- nloptr_output[grepl('\tx', nloptr_output)]
  fx <- nloptr_output[grepl('\tf', nloptr_output)]
  runs <- data.table(
    x = as.numeric(str_extract_all(x, "[0-9]+\\.[0-9]+", simplify = TRUE)), 
    fx = as.numeric(str_extract_all(fx, "[0-9]+\\.[0-9]+", simplify = TRUE))  
  )
  runs[, id := 1:.N]
  return(runs)
}


plot_gamma_maxent <- function(res,  from = 0.001, to = 100, n = 1E3, ylim = c(0,50)) {
  fun1 <- function(x) {
    sapply(x, function(y) {
      res$eval_f(x=y)
    })}
  
  curve(fun1,  from = from, to = to, n = n, ylim = ylim)
  abline(v = res$solution, col = 2)
  abline(h = res$objective, col = 2, lty = 2)
}


plot_dirichlet <- function(x, mu = NULL, u = NULL, type, ...) {
  
  # type line plot ====================================== 
  if (type == 1) {
    plot(x[1, ],
         type = 'lines',
         ylim = c(0, 1),
         col = 'grey80', ...)
    
    for (i in 2:nrow(x)) {
      lines(x[i, ], col = 'grey80')
    }
    
    if (!is.null(mu)) lines(mu, col = 'red', lwd = 3)
    if (!is.null(u)) {
      lines(mu + u,
            col = 'red',
            lwd = 3,
            lty = 2)
      lines(mu - u,
            col = 'red',
            lwd = 3,
            lty = 2)  
    }  
  }
  
  # type 95 CI =========================================
  if (type == 2) {
    percent <- x %>%
      apply(2, quantile, probs = c(0.025, 0.5, 0.975))
    
    plot(
      percent[1, ],
      type = 'point',
      ylim = c(0, 1),
      col = 'grey30',
      pch = 25, ...
    )
    points(percent[2, ], col = 'grey30', pch = 8)
    points(percent[3, ], col = 'grey30', pch = 24)
    
    if (!is.null(mu)) points(mu, col = 'red', pch = 8)
    
    if (!is.null(u)) {
      points(mu + u,
             col = 'red',
             pch = 24)
      points(mu - u,
             col = 'red',
             pch = 25)  
    }
  }
  
  # type boxplot =====================================
  if (type == 3) {
    boxplot(x, ...)
    
    if (!is.null(mu)) points(mu, col = 'red', lwd = 3)
    
    if (!is.null(u)) {
      points(mu + u,
             col = 'red',
             lwd = 3,
             pch = 4)
      points(mu - u,
             col = 'red',
             lwd = 3,
             pch = 4)  
    } 
  }
}




sample_dirichlet_maxent <- function(N, shares, ...) {
  out <- find_gamma_maxent(shares, eval_f = eval_f, ...)
  sample <- gtools::rdirichlet(N, shares * out$solution)
  attr(sample, 'nloptr') <- out
  return(sample)
}



#' RNG from dirichlet distribution. 
#' Like gtools rdirichlet. But returns a list of numeric vectors instead
#'
#' @param n 
#' @param alpha 
#'
#' @return
#' @export
#'
#' @examples
rdirichlet_list <- function(n, alpha) {
  rn <- gtools::rdirichlet(n, alpha)
  rn <- as.data.table(rn)
  return(rn[, list(sample = lapply(.SD, c))]$sample)
  
}


#' Derivative of the Dirichlet entropy function.
#' Derivated using the `Deriv` package and `autodiffr`. 
#' For derivation see script: 
#' 
#' @param x 
#' @param shares 
#'
#' @return
#' @export
#'
#' @examples
dirichlet_entropy_grad <- function (x, shares) 
{
  #
  .e1 <- shares * x
  .e2 <- sum(.e1)
  -(((1 - prod(gamma(.e1))/(beta2(.e1) * gamma(.e2))) * digamma(.e2) + 
       (.e2 - length(.e1)) * trigamma(.e2)) * sum(shares) - 
      sum(shares * ((.e1 - 1) * trigamma(.e1) + digamma(.e1))))
}


#' Like `find_gamma_maxent`: Finds the Gamma value which maximises the entropy of a Dirichlet distribution
#' with given alphas (=sector shares), using the nloptr optimazation package. 
#'
#' BUT: including the first derivative of the Dir Entropy function `dirichlet_entropy_grad`, 
#' and thus is **much** faster than `find_gamma_maxent`. 
#' 
#' @param shares 
#' @param eval_f 
#' @param eval_grad_f 
#' @param x0 
#' @param bounds 
#' @param shares_lb lower bound of shares. Only values LARGER than that will be considered (set this to e.g. 1E-4) to increase chance of convergence
#' @param local_opts 
#' @param opts 
#'
#' @return
#' @export
#'
#' @examples
find_gamma_maxent2 <- function(shares,
                               eval_f = eval_f, 
                               eval_grad_f = dirichlet_entropy_grad,
                               x0 = 1, # initial value of gamma
                               x0_n_tries = 100,
                               bounds = c(0.001, 300),
                               shares_lb = 0,
                               local_opts = list( "algorithm" = "NLOPT_LD_MMA", # optim options
                                                  "xtol_rel" = 1.0e-4 ),
                               opts = list( "algorithm"= "NLOPT_GD_STOGO",
                                            "xtol_rel"= 1.0e-4,
                                            "maxeval"= 1E3,
                                            "local_opts" = local_opts,
                                            "print_level" = 0 )
                               
) {
  
  # remove shares of zero
  shares <- shares[which(shares > shares_lb)]
  
  # lower and upper bounds
  lb <- bounds[1]
  ub <- bounds[2]
  
  count <- 0
  while(!is.finite(eval_f(x = x0, shares = shares)) 
        | !is.finite(eval_grad_f(x = x0, shares = shares))) {
    if (count > x0_n_tries) {
      #stop('Error: Could not find an initial value x0 which is defined by eval_f and/or eval_grad_f. Either increase x0_n_tries (defaul: 100), or increase the parameter space with the bounds argument')
      warning('Warning: Could not find an initial value x0 which is defined by eval_f and/or eval_grad_f. Either increase x0_n_tries (defaul: 100), or increase the parameter space with the bounds argument. Q&D solution: x0 is set to 1')
      x0 <- 1
      break
    } else {
      x0 <- runif(1, min = lb, max = ub) 
      count <- count + 1  
    }
  }
  
  # Run the optimizer
  res <- nloptr ( x0 = x0,
                  eval_f = eval_f,
                  eval_grad_f = eval_grad_f,
                  lb = lb, 
                  ub = ub,
                  opts = opts, 
                  shares = shares
  )
  
  return(res)
}



