context("Robust Repeated Measures ANOVA -- Verification project")

# Reference outputs reproduce worked examples from the WRS2 package vignette
# (Mair & Wilcox, 2020, doi:10.3758/s13428-019-01246-w). The vignette uses
# hangover and essays datasets in long format; we widen them once and ship the
# wide CSVs alongside the tests so the JASP analysis can consume them directly.

.initVerifiedRMRobustOptions <- function() {
  options <- list()
  options$repeatedMeasuresFactors <- list()
  options$repeatedMeasuresCells   <- list()
  options$betweenSubjectFactors   <- list()
  options$robustMethod    <- "trimmedMeans"
  options$trimProportion  <- 0.2
  options$bootstrapSamples <- 599
  options$descriptivesTable <- FALSE
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
  options$normalizeErrorBarsDescriptives <- TRUE
  options$rainCloudHorizontalAxis    <- ""
  options$rainCloudSeparatePlots     <- ""
  options$rainCloudHorizontalDisplay <- FALSE
  options$rainCloudYAxisLabel        <- ""
  return(options)
}

# WRS2 vignette: rmanova(symptoms ~ time, blocks=id) on hangover controls.
# Published: F = 2.6883, df1 = 2, df2 = 22, p = 0.09026.
test_that("rmanova matches WRS2 vignette (hangover control)", {
  options <- .initVerifiedRMRobustOptions()
  options$repeatedMeasuresFactors <- list(
    list(name = "time", levels = c("Time1", "Time2", "Time3"))
  )
  options$repeatedMeasuresCells <- c("Time1", "Time2", "Time3")

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresRobust",
                                    "hangover_wide_control.csv", options)

  resultTable <- results[["results"]][["rmRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(
    "test" = resultTable,
    "ref"  = list(2, 22, "time", 0.0902553622330652, 2.68830532855875))
})

# WRS2 vignette: rmmcp(symptoms ~ time, blocks=id) on hangover controls.
# Published pairwise contrasts: 1-2 psihat=-2.67 p=.146; 1-3 psihat=-1.00 p=.221;
# 2-3 psihat=0.50 p=.656.
test_that("rmmcp post hoc matches WRS2 vignette (hangover control)", {
  options <- .initVerifiedRMRobustOptions()
  options$repeatedMeasuresFactors <- list(
    list(name = "time", levels = c("Time1", "Time2", "Time3"))
  )
  options$repeatedMeasuresCells <- c("Time1", "Time2", "Time3")
  options$postHocTerms <- list(list(components = "time"))

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresRobust",
                                    "hangover_wide_control.csv", options)

  resultTable <- results[["results"]][["rmRobustPostHocContainer"]][["collection"]][["rmRobustPostHocContainer_time"]][["data"]]
  jaspTools::expect_equal_tables(
    "test" = resultTable,
    "ref"  = list(-7.47191536213064, 2.13858202879731, "Time1", "Time2",
                  0.145884171322724, 0.437652513968171, -2.66666666666667,
                  -3.17264747010019, 1.17264747010019, "Time1", "Time3",
                  0.220853410046966, 0.441706820093933, -1, -2.57825824772231,
                  3.57825824772231, "Time2", "Time3", 0.655827851330351,
                  0.655827851330351, 0.5))
})

# WRS2 vignette: bwtrim(symptoms ~ group*time, id=id, data=hangover).
# Published: group Q=6.61 df=(1, 14.48) p=.0218; time Q=4.49 df=(2, 15.42) p=.0290;
# interaction Q=0.57 df=(2, 15.42) p=.5790.
test_that("bwtrim matches WRS2 vignette (hangover mixed design)", {
  options <- .initVerifiedRMRobustOptions()
  options$repeatedMeasuresFactors <- list(
    list(name = "time", levels = c("Time1", "Time2", "Time3"))
  )
  options$repeatedMeasuresCells <- c("Time1", "Time2", "Time3")
  options$betweenSubjectFactors <- "group"

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresRobust",
                                    "hangover_wide.csv", options)

  resultTable <- results[["results"]][["rmRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(
    "test" = resultTable,
    "ref"  = list(1, 14.4847059170794, "group", 0.0217511542976931,
                  6.60867348372618, 2, 15.417298401099, "time",
                  0.0290102658679592, 4.49312155031112, 2, 15.417298401099,
                  "group <unicode> time", 0.57899539851255, 0.566295774040394))
})
