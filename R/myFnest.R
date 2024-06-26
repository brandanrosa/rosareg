myFnest <- function(objc, objr, alpha = 0.05) {

  # Model Data
  comp <- myLAreg(objc)
  redd <- myLAreg(objr)

  # n-k-g
  n <- dim(comp$dat)[1]
  k <- dim(comp$dat)[2]
  g <- dim(redd$dat)[2]

  # RSS (both models) and s-squared for complete model
  RSSc <- comp$RSS
  RSSr <- redd$RSS
  ssqc <- comp$s.squared

  # df1 & df2
  df1 <- k-g
  df2 <- n - (k+1)

  # F_nest Test Statistic
  F_nest <- ((RSSr - RSSc) / df1) / ssqc

  # RAR (F_alpha)
  F_alpha <- stats::qf(1 - alpha, df1 = df1, df2 = df2)

  # Test
  test <- ifelse(F_nest > F_alpha, "REJECT H0", "FAIL TO REJECT H0")

  # List
  list(F_nest=F_nest, F_alpha=F_alpha, Test=test, n=n, k=k, g=g, RSSc=RSSc, RSSr=RSSr, ssqc=ssqc)
}
