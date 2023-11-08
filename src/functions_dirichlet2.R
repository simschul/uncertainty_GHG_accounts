library(Deriv)

beta3 <- function(alpha, prod_fun = prod) {
  prod_fun(gamma(alpha)) / gamma(sum(alpha))
}

make_prod_fun <- function(length, envir = environment()) {
  paste('function(x)', paste0('x[', 1:length, ']', collapse = '*')) %>% 
    parse(text = .) %>% 
    eval(envir = envir)#(envir = .GlobalEnv) # carefull here when multiproc
}

# Objective Function
eval_f2 <- function(x, prod_fun = prod) {
  # from:  https://en.wikipedia.org/wiki/Dirichlet_distribution#Entropy
  # calculates the negative entropy of a dirichlet distribution given `shares` (sum to 1) and a concentration parameter `x`, so that `alpha = shares * x`
  alpha <- x * shares
  K <- length(alpha)
  psi <- digamma(alpha)
  alpha0 <- sum(alpha)
  (-(log(beta3(alpha, prod_fun = prod_fun)) + (alpha0 - K) * digamma(alpha0) - sum((alpha - 1) * psi)))
}
# alpha <- c(0.01, 0.89, 0.1)
# 
# shares <- alpha


fun <- function() print(environment())
fun()
shares <- testshares


find_gamma_maxent2 <- function(shares, bounds = c(0.1, 1000)) {
  my_prod <- make_prod_fun(length(shares), envir = environment())
  fun_deriv <- Deriv(function(x) eval_f2(x, prod_fun = my_prod), 
                     cache.exp = FALSE)
  fun_deriv2 <- function(x) sapply(x, fun_deriv)
  # f.lower <- fun_deriv2(bounds[1])
  # f.upper <- fun_deriv2(bounds[2])
  # 
  # while(!is.finite(f.lower)) {
  #   bounds[1] <- mean(bounds)
  #   f.lower <- fun_deriv2(bounds[1])
  # }
  # while(!is.finite(f.upper)) {
  #   bounds[2] <- mean(bounds)
  #   f.upper <- fun_deriv2(bounds[2])
   <- <- <- <- <- <- <- <- <- <- <- 