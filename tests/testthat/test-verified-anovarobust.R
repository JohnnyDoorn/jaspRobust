context("Robust ANOVA -- Verification project")

# Reference outputs reproduce worked examples from the WRS2 package vignette
# (Mair & Wilcox, 2020, doi:10.3758/s13428-019-01246-w). Each test runs the JASP
# analysis on the same dataset used in the vignette and locks the published
# table values via jaspTools::expect_equal_tables.

.initVerifiedAnovaRobustOptions <- function() {
  options <- list()
  options$dependent       <- ""
  options$fixedFactors    <- list()
  options$covariates      <- list()
  options$robustMethod    <- "trimmedMeans"
  options$trimProportion  <- 0.2
  options$bootstrapSamples <- 599
  options$descriptivesTable <- FALSE
  options$effectSizeTable   <- FALSE
  options$postHocTerms <- list()
  options$postHocCorrectionHochberg   <- TRUE
  options$postHocCorrectionBonferroni <- FALSE
  options$postHocCorrectionHolm       <- FALSE
  options$postHocCi <- FALSE
  options$postHocCiLevel <- 0.95
  options$postHocSignificanceFlag <- FALSE
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

# WRS2 vignette: t1way(Wdiff ~ Treat, data = anorexia)
# Published: F = 5.6286, df1 = 2, df2 = 24.89, p = 0.00962, xi = 0.55
test_that("t1way matches WRS2 vignette (anorexia weight change)", {
  options <- .initVerifiedAnovaRobustOptions()
  options$dependent <- "Wdiff"
  options$fixedFactors <- "Treat"

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRobust", "anorexia.csv", options)

  resultTable <- results[["results"]][["anovaRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(
    "test" = resultTable,
    "ref"  = list(2, 24.8896887474554, 0.549121012084309, 0.0096225417585577,
                  5.6286168389342))
})

# WRS2 vignette: lincon(Wdiff ~ Treat, data = anorexia)
# Published: CBT-Cont psihat=2.96 p=0.222; CBT-FT psihat=-6.11 p=0.039;
# Cont-FT psihat=-9.07 p=0.009.
test_that("lincon post hoc matches WRS2 vignette (anorexia)", {
  options <- .initVerifiedAnovaRobustOptions()
  options$dependent <- "Wdiff"
  options$fixedFactors <- "Treat"
  options$postHocTerms <- list(list(components = "Treat"))
  options$postHocCi <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRobust", "anorexia.csv", options)

  resultTable <- results[["results"]][["postHocContainer"]][["collection"]][["postHocContainer_Treat"]][["data"]]
  jaspTools::expect_equal_tables(
    "test" = resultTable,
    "ref"  = list(-3.03708523796843, 8.96208523796843, "CBT", "Cont", 2.9625,
                  0.22200647804533, -12.3349029562612, 0.116721138079384, "CBT",
                  "FT", -6.10909090909091, 0.0388536779956632, -16.0825459665156,
                  -2.06063585166621, "Cont", "FT", -9.0715909090909,
                  0.00879636885842627))
})

# WRS2 vignette: med1way(Wdiff ~ Treat, data = anorexia) with set.seed(123)
# Published: F = 4.5708, critical value = 2.8398, p = 0.008
test_that("med1way matches WRS2 vignette (anorexia, seed=123)", {
  options <- .initVerifiedAnovaRobustOptions()
  options$dependent <- "Wdiff"
  options$fixedFactors <- "Treat"
  options$robustMethod <- "medians"

  set.seed(123)
  results <- jaspTools::runAnalysis("AnovaRobust", "anorexia.csv", options)

  resultTable <- results[["results"]][["anovaRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(
    "test" = resultTable,
    "ref"  = list(2.83981653079319, 0.008, 4.57084062661611))
})

# WRS2 vignette: t2way(attractiveness ~ gender*alcohol, data = goggles)
# Published: gender Q=1.67 p=.209; alcohol Q=48.28 p=.001; interaction Q=26.26 p=.001
test_that("t2way matches WRS2 vignette (beer goggles)", {
  options <- .initVerifiedAnovaRobustOptions()
  options$dependent <- "attractiveness"
  options$fixedFactors <- c("gender", "alcohol")

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRobust", "goggles.csv", options)

  resultTable <- results[["results"]][["anovaRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(
    "test" = resultTable,
    "ref"  = list("gender", 0.209, 1.66666666666667, "alcohol", 0.001,
                  48.2845014074818, "gender <unicode> alcohol", 0.001,
                  26.2571844932613))
})
