context("Robust Linear Regression -- Verification project")

# Reference outputs reproduce the canonical MASS::rlm worked example on
# Brownlee's (1965) stack-loss data, as published in Venables & Ripley (2002)
# "Modern Applied Statistics with S" (4th ed., Springer) and in the MASS
# package's own rlm() help page. JASP's RegressionLinearRobust calls
# MASS::rlm() directly with Huber psi, so the coefficient table, residual
# scale, and degrees of freedom match the rlm output cell-for-cell.

.initVerifiedRegressionRobustOptions <- function() {
  options <- list()
  options$dependent  <- ""
  options$covariates <- list()
  options$factors    <- list()
  options$estimationMethod    <- "huber"
  options$coefficientEstimate <- TRUE
  options$coefficientCi       <- FALSE
  options$coefficientCiLevel  <- 0.95
  options$residualVsFittedPlot <- FALSE
  options$residualQqPlot       <- FALSE
  options$weightsPlot          <- FALSE
  options$descriptivesTable    <- FALSE
  options$descriptivesTrimProportion <- 0.2
  return(options)
}

# MASS::rlm(stack.loss ~ ., data = stackloss) with default Huber psi.
# Published coefficients (Venables & Ripley 2002, Table 6.5; ?MASS::rlm):
#   (Intercept) -41.0265, Air.Flow 0.8294, Water.Temp 0.9261, Acid.Conc. -0.1278
# Published residual scale = 2.4407 on 17 df (n - p = 21 - 4).
test_that("rlm Huber matches MASS reference (Brownlee's stackloss data)", {
  options <- .initVerifiedRegressionRobustOptions()
  options$dependent <- "stack.loss"
  options$covariates <- c("Air.Flow", "Water.Temp", "Acid.Conc.")

  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinearRobust", "stackloss.csv", options)

  coefTable <- results[["results"]][["coefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(
    "test" = coefTable,
    "ref"  = list(-41.0265310522329, 2.87378052554289e-05, 9.80734719640061,
                  "(Intercept)", -4.18324448300251, 0.829373868887716,
                  8.67088008529427e-14, 0.111180346015732, "Air.Flow",
                  7.45971656510551, 0.926108184089072, 0.00227055973640233,
                  0.303408095306798, "Water.Temp", 3.05235159646158,
                  -0.127849160311598, 0.32109382049443, 0.128852585127027,
                  "Acid.Conc.", -0.992212614015932))

  summaryTable <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(
    "test" = summaryTable,
    "ref"  = list("<unicode>", 3, 21, 2.44071379481485))
})
