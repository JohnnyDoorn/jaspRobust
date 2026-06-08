context("Robust Correlation -- Verification project")

# Reference outputs reproduce the WRS2 package vignette's winall() worked example
# (Mair & Wilcox, 2020, doi:10.3758/s13428-019-01246-w). The vignette computes
# pairwise Winsorized correlations on the alcoholic-group symptom data in wide
# format. Running CorrelationRobust with the Winsorized method on the same
# Time1/Time2/Time3 columns reproduces the published correlation matrix and
# p-values cell-for-cell.

.initVerifiedCorrelationRobustOptions <- function() {
  options <- list()
  options$variables           <- list()
  options$correlationMethod   <- "percentageBend"
  options$trimProportion      <- 0.2
  options$alternative         <- "twoSided"
  options$sampleSize          <- FALSE
  options$significanceFlagged <- FALSE
  options$descriptivesTable   <- FALSE
  options$scatterPlot         <- FALSE
  return(options)
}

# WRS2 vignette: winall(hangwide) on the alcoholic-group wide-format symptoms.
# Published correlation matrix: r(T1,T2)=0.2651, r(T1,T3)=0.4875, r(T2,T3)=0.6791.
# Published p-values:           p(T1,T2)=0.2705, p(T1,T3)=0.0393, p(T2,T3)=0.0028.
test_that("wincor matches WRS2 winall vignette (hangover alcoholic Time1-3)", {
  options <- .initVerifiedCorrelationRobustOptions()
  options$variables <- c("Time1", "Time2", "Time3")
  options$correlationMethod <- "winsorized"

  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationRobust",
                                    "hangover_wide_alcoholic.csv", options)

  resultTable <- results[["results"]][["correlationTable"]][["data"]]
  jaspTools::expect_equal_tables(
    "test" = resultTable,
    "ref"  = list(0.265117416144604, 0.270461662732368, 1.16654129231958,
                  "Time1", "Time2", 0.487514658260004, 0.0393489034506629,
                  2.36893191968689, "Time1", "Time3", 0.679058863551443,
                  0.00284493293739208, 3.92462368937991, "Time2", "Time3"))
})
