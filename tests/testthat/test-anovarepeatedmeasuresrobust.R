context("Robust Repeated Measures ANOVA")

# Reference tables captured via jaspTools::makeTestTable on debug.csv with
# set.seed(1).

# debug.csv has no native wide-format RM columns, so we treat three correlated
# scale variables (contNormal, contcor1, contcor2) as 3 levels of a single RM
# factor. This is enough to exercise the analysis pipeline.

.initRMRobustOptions <- function() {
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

# ---- Pure within (rmanova) -----------------------------------------------

test_that("Robust RM ANOVA (within only, trimmed means) main table matches", {
  options <- .initRMRobustOptions()
  options$repeatedMeasuresFactors <- list(
    list(name = "RMfactor", levels = c("L1", "L2", "L3"))
  )
  options$repeatedMeasuresCells <- c("contNormal", "contcor1", "contcor2")
  options$robustMethod   <- "trimmedMeans"
  options$trimProportion <- 0.2
  options$descriptivesTable <- TRUE
  options$postHocTerms <- list(list(components = "RMfactor"))
  options$postHocCorrectionHochberg   <- TRUE
  options$postHocCorrectionBonferroni <- TRUE
  options$postHocCorrectionHolm       <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresRobust", "debug.csv", options)

  table <- results[["results"]][["rmRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.65952865830488, 97.9121908399879, "RMfactor", 0.0464308286212937,
         3.38602565670305))

  table <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("L1", 0.9382150060752, -0.249904344716667, -0.38335354, 100, 0.564965989561465,
         "L2", 1.2122742046674, 0.0105420493, 0.003635444, 100, 0.708177014442337,
         "L3", 1.0434667689831, 0.0535305609833333, 0.0299231485, 100,
         0.627231548181343))

  table <- results[["results"]][["rmRobustPostHocContainer"]][["collection"]][["rmRobustPostHocContainer_RMfactor"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.640637946824405, 0.0880449218577387, "L1", "L2", 0.0666344198454363,
         0.199903259536309, 0.145437895333171, 0.199903259536309, -0.276296512483333,
         -0.671484218830783, 0.0996552005307826, "L1", "L3", 0.0727189476665855,
         0.218156842999756, 0.145437895333171, 0.199903259536309, -0.28591450915,
         -0.230605613644365, 0.185559700844365, "L2", "L3", 0.790613900985873,
         1, 0.790613900985873, 0.790613900985873, -0.0225229564))
})

# ---- Pure within, bootstrap (rmanovab) -----------------------------------

test_that("Robust RM ANOVA (within bootstrap) main table matches", {
  options <- .initRMRobustOptions()
  options$repeatedMeasuresFactors <- list(
    list(name = "RMfactor", levels = c("L1", "L2", "L3"))
  )
  options$repeatedMeasuresCells <- c("contNormal", "contcor1", "contcor2")
  options$robustMethod    <- "trimmedMeansBootstrap"
  options$trimProportion  <- 0.2
  options$bootstrapSamples <- 199

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresRobust", "debug.csv", options)

  table <- results[["results"]][["rmRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("RMfactor", "", 3.38602565670305))
})

# ---- Mixed design (bwtrim) ------------------------------------------------

test_that("Robust RM ANOVA (mixed, trimmed means) main table matches", {
  options <- .initRMRobustOptions()
  options$repeatedMeasuresFactors <- list(
    list(name = "RMfactor", levels = c("L1", "L2", "L3"))
  )
  options$repeatedMeasuresCells   <- c("contNormal", "contcor1", "contcor2")
  options$betweenSubjectFactors   <- "facGender"
  options$robustMethod   <- "trimmedMeans"
  options$trimProportion <- 0.2
  options$descriptivesTable <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresRobust", "debug.csv", options)

  table <- results[["results"]][["rmRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 57.9336483503635, "facGender", 0.89459940359633, 0.0177075445379754,
         2, 51.2047538834003, "RMfactor", 0.111628846221902, 2.28919985854055,
         2, 51.2047538834003, "facGender <unicode> RMfactor", 0.0484029045714631,
         3.21455414651095))

  table <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("L1", "f", 0.5828973992247, -0.439290827233333, -0.4570769135,
         50, 0.436792042981782, "L2", "f", 1.1298734560743, 0.0357877384000001,
         0.211364676, 50, 0.716897399551089, "L3", "f", 0.9618835008258,
         0.204071441366667, 0.1343194775, 50, 0.681341600303714, "L1",
         "m", 1.1318691105456, -0.0352707709333333, 0.1378493075, 50,
         0.624793853145177, "L2", "m", 1.1517189525366, -0.0160352303666666,
         -0.059104015, 50, 0.685360746411084, "L3", "m", 0.8765182475721,
         -0.0862303857666667, -0.1175669025, 50, 0.58954825793456))
})

# ---- Mixed design bootstrap (sppba/sppbb/sppbi) --------------------------

test_that("Robust RM ANOVA (mixed bootstrap) main table matches", {
  options <- .initRMRobustOptions()
  options$repeatedMeasuresFactors <- list(
    list(name = "RMfactor", levels = c("L1", "L2", "L3"))
  )
  options$repeatedMeasuresCells <- c("contNormal", "contcor1", "contcor2")
  options$betweenSubjectFactors <- "facGender"
  options$robustMethod    <- "trimmedMeansBootstrap"
  options$trimProportion  <- 0.2
  options$bootstrapSamples <- 199

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasuresRobust", "debug.csv", options)

  table <- results[["results"]][["rmRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("facGender", 0.939698492462312, 0.00997274507466552, "RMfactor",
         0.306532663316583, -0.267677592474227, "facGender <unicode> RMfactor",
         0.0904522613065326, -0.591308697087234))
})
