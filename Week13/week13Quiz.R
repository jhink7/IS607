library(Rcpp)
library(microbenchmark)

sumanddoubleR <- function(x) {
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  return <- total * 2.0
}

cppFunction('double sumanddoubleC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return 2.0*total;
}')


inVect <- runif(100000, 0, 10000)

microbenchmark(
  sumanddoubleC(inVect),
  sumanddoubleR(inVect)
)