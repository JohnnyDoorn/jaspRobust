context("Robust ANCOVA -- Verification project")

# Reference outputs reproduce the WRS2 ANCOVA worked example from the package
# vignette (Mair & Wilcox, 2020, doi:10.3758/s13428-019-01246-w). The vignette
# uses fr1 = fr2 = 0.3 with six user-specified evaluation points; JASP exposes
# only WRS2's defaults (fr1 = fr2 = 1, evaluation points chosen by WRS2). The
# locked expected values therefore match JASP's default-options run, not the
# bespoke 6-point vignette table.

.initVerifiedAncovaRobustOptions <- function() {
  options <- list()
  options$dependent       <- ""
  options$fixedFactors    <- ""
  options$covariates      <- ""
  options$robustMethod    <- "trimmedMeans"
  options$trimProportion  <- 0.2
  options$bootstrapSamples <- 599
  options$descriptivesTable <- FALSE
  options$descriptivePlotHorizontalAxis <- ""
  options$descriptivePlotSeparateLines  <- ""
  options$descriptivePlotSeparatePlot   <- ""
  options$descriptivePlotErrorBar       <- FALSE
  options$descriptivePlotErrorBarType   <- "ci"
  options$descriptivePlotCiLevel        <- 0.95
  options$rainCloudHorizontalAxis    <- ""
  options$rainCloudSeparatePlots     <- ""
  options$rainCloudHorizontalDisplay <- FALSE
  options$rainCloudYAxisLabel        <- ""
  return(options)
}

# WRS2 vignette: ancova(Posttest ~ Pretest + Group, ...) on the Electric Company
# data. Verified table at JASP defaults (no fr1/fr2 override, default eval pts).
test_that("ancova matches WRS2 vignette (Electric Company data, JASP defaults)", {
  options <- .initVerifiedAncovaRobustOptions()
  options$dependent <- "Posttest"
  options$fixedFactors <- "Group"
  options$covariates <- "Pretest"

  set.seed(1)
  results <- jaspTools::runAnalysis("AncovaRobust", "electric.csv", options)

  resultTable <- results[["results"]][["ancovaRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(
    "test" = resultTable,
    "ref"  = list(-23.0435158196117, 3.76769164378747, 11.5, 22, 21,
                  0.0579448813817118, 4.84080866342892, 1.99097150042803,
                  -9.6379120879121, -14.3629838198003, -0.0860161801996835,
                  50.3, 41, 24, 0.00959023697497385, 2.64270409343806,
                  2.73375290784115, -7.22450000000001, -7.92010479255551,
                  1.65288257033329, 78.8, 73, 66, 0.0902238554039705,
                  1.82800458750654, 1.71422497105736, -3.13361111111111,
                  -8.5998407757692, -0.597190263367111, 99.6, 65, 62,
                  0.00346036736118238, 1.5216129815006, 3.02213215546646,
                  -4.59851551956815, -6.83901775510173, -0.47727854119456,
                  113.9, 48, 43, 0.00354242333666832, 1.18813702128762,
                  3.07889417012163, -3.65814814814814))
})
