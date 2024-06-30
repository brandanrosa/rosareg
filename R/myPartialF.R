#' myPartialF
#'
#' A function which runs a Partial F-Test using the `anova()` function.
#'
#' @param objr an `lm()` object of the reduced model
#' @param objc an `lm()` object of the complete model
#' @param alpha alpha level, default is 0.05
#'
#' @return a list containing the ANOVA summary, F_Statistic, p-value, alpha level, and test conclusion.
#' @export
#'
#' @examples \dontrun{myPartialF(lm1, lm2, alpha=0.05)}
myPartialF <- function(objr, objc, alpha=0.05) { #objr=reduced, objc=complete

  # anova
  ani <- stats::anova(objr, objc)
  print(ani)

  # anova stuff
  F <- ani$F[2]
  RSS <- ani$RSS
  p_value <- ani$`Pr(>F)`[2]

  # Test
  test <- as.vector(ifelse(p_value < alpha, "Reject H0", "Fail to Reject H0"))

  # List
  list(anova=ani, F=F, RSS=RSS, p_value=p_value, alpha=alpha, Test=test)
}
