## *** NFFD  ***

nffd_param <- read.csv(file = "../optimizationTables/param_NFFD.csv", sep=',', header = TRUE)

# nffd: number of frost free days
# m: month of the year
# tm: min temperature for that month
nffd <- function(m, tm) {
  
  optimized_params <- nffd_param[nffd_param$Month == m,]

  a <- optimized_params$a
  b <- optimized_params$b
  t0 <- optimized_params$T0

  return( a/(1 + exp(-(tm - t0)/b)))
}
