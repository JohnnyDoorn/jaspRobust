context("Robust ANCOVA")

# Reference tables captured via jaspTools::makeTestTable on debug.csv with
# set.seed(1).

.initAncovaRobustOptions <- function() {
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

# ---- Robust ANCOVA, trimmed means -----------------------------------------

test_that("Robust ANCOVA (trimmed means) main table matches", {
  options <- .initAncovaRobustOptions()
  options$dependent      <- "contNormal"
  options$fixedFactors   <- "contBinom"
  options$covariates     <- "contGamma"
  options$robustMethod   <- "trimmedMeans"
  options$trimProportion <- 0.2
  options$descriptivesTable <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("AncovaRobust", "debug.csv", options)

  table <- results[["results"]][["ancovaRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(-0.429027548643857, 1.19160659701138, 0.197333889, 28, 19, 0.207377004136249,
                                             0.295323904506083, 1.29108926966631, 0.381289524183761, -0.334023763648355,
                                             1.07654360943883, 0.678039819, 35, 25, 0.160881045013004, 0.258158427865385,
                                             1.43810886193043, 0.371259922895238, -0.313353019183238, 0.88873727563623,
                                             1.281209304, 42, 30, 0.205171788939185, 0.223339563750743, 1.28813777279324,
                                             0.287692128226496, -0.591772256204538, 0.69583135571223, 1.800714959,
                                             42, 32, 0.828932797134438, 0.239210500177242, 0.217505292264742,
                                             0.0520295497538462, -1.04568493748888, 0.754565858188882, 3.491491238,
                                             20, 14, 0.650968217010081, 0.316483173488053, 0.459928210545117,
                                             -0.14555953965))

  table <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(0, 0.8649064354161, -0.18943073975, -0.2223981035, 58, "contNormal",
                                             0.568333486814063, 0, 1.4728222018503, 1.79220918169444, 1.6961274775,
                                             58, "contGamma", 1.22469792658697, 1, 0.7927275718572, -0.332608387307692,
                                             -0.405769511, 42, "contNormal", 0.58496035543894, 1, 1.2583327140888,
                                             1.72278395003846, 1.641466777, 42, "contGamma", 0.794984474988436))
})

# ---- Robust ANCOVA, bootstrap --------------------------------------------

test_that("Robust ANCOVA (bootstrap) main table matches", {
  options <- .initAncovaRobustOptions()
  options$dependent       <- "contNormal"
  options$fixedFactors    <- "contBinom"
  options$covariates      <- "contGamma"
  options$robustMethod    <- "trimmedMeansBootstrap"
  options$trimProportion  <- 0.2
  options$bootstrapSamples <- 199

  set.seed(1)
  results <- jaspTools::runAnalysis("AncovaRobust", "debug.csv", options)

  table <- results[["results"]][["ancovaRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(-0.521452546614296, 1.28403159498182, 0.197333889, 28, 19, 0.190954773869347,
                                             1.29108926966631, 0.381289524183761, -0.41787522738979, 1.16039507318027,
                                             0.678039819, 35, 25, 0.120603015075377, 1.43810886193043, 0.371259922895238,
                                             -0.395009194609794, 0.970393451062785, 1.281209304, 42, 30,
                                             0.165829145728643, 1.28813777279324, 0.287692128226496, -0.679185834411559,
                                             0.783244933919251, 1.800714959, 42, 32, 0.829145728643216, 0.217505292264742,
                                             0.0520295497538462, -1.11298097438002, 0.82186189508002, 3.491491238,
                                             20, 14, 0.678391959798995, -0.459928210545117, -0.14555953965
  ))
})

# ---- Robust ANCOVA descriptives plot --------------------------------------

test_that("Robust ANCOVA descriptives plot matches", {
  options <- .initAncovaRobustOptions()
  options$dependent    <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$covariates   <- "contGamma"
  options$descriptivePlotHorizontalAxis <- "contGamma"
  options$descriptivePlotErrorBar       <- TRUE
  options$descriptivePlotErrorBarType   <- "ci"
  options$descriptivePlotCiLevel        <- 0.95

  set.seed(1)
  results <- jaspTools::runAnalysis("AncovaRobust", "debug.csv", options)

  plotName <- results[["results"]][["descriptivesPlotContainer"]][["collection"]][[1]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "ancovarobust-descriptives")
})
