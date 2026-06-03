context("Robust Correlation")

# Reference tables captured via jaspTools::makeTestTable on debug.csv with
# set.seed(1). Plot snapshots are created on first run -- inspect manually
# before accepting.

.initCorrelationRobustOptions <- function() {
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

# ---- Percentage bend, two-sided ------------------------------------------

test_that("Robust correlation (percentage bend, two-sided) main table matches", {
  options <- .initCorrelationRobustOptions()
  options$variables           <- c("contNormal", "contGamma", "contcor1", "contcor2")
  options$correlationMethod   <- "percentageBend"
  options$trimProportion      <- 0.2
  options$alternative         <- "twoSided"
  options$sampleSize          <- TRUE
  options$significanceFlagged <- TRUE
  options$descriptivesTable   <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationRobust", "debug.csv", options)

  table <- results[["results"]][["correlationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.055, 100, 0.587572794673231, -0.544152025855925, "contNormal",
         "contGamma", 0.133, 100, 0.187549845177757, 1.32712858993291,
         "contNormal", "contcor1", -0.012, 100, 0.906583892884649, -0.117652386187062,
         "contNormal", "contcor2", -0.165, 100, 0.101909841468354, -1.65114620070499,
         "contGamma", "contcor1", -0.081, 100, 0.421184365209572, -0.807761272298114,
         "contGamma", "contcor2", "0.674 ***", 100, 1.59872115546023e-14,
         9.02270219279272, "contcor1", "contcor2"))

  table <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.9382150060752, -0.249904344716667, -0.38335354, 100, "contNormal",
         0.564965989561465, 1.3904477575464, 1.72587557315, 1.6961274775,
         100, "contGamma", 0.838930241377002, 1.2122742046674, 0.0105420493,
         0.003635444, 100, "contcor1", 0.708177014442337, 1.0434667689831,
         0.0535305609833333, 0.0299231485, 100, "contcor2", 0.627231548181343))
})

# ---- Winsorized, one-sided greater ---------------------------------------

test_that("Robust correlation (Winsorized, greater) main table matches", {
  options <- .initCorrelationRobustOptions()
  options$variables           <- c("contNormal", "contcor1", "contcor2")
  options$correlationMethod   <- "winsorized"
  options$trimProportion      <- 0.2
  options$alternative         <- "greater"
  options$sampleSize          <- TRUE
  options$significanceFlagged <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationRobust", "debug.csv", options)

  table <- results[["results"]][["correlationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.153, 100, 0.0655863585077646, 1.53113427567498, "contNormal",
         "contcor1", 0.005, 100, 0.479942786571129, 0.0505149873620996,
         "contNormal", "contcor2", "0.669 ***", 100, 9.49684775264359e-13,
         8.90552574221034, "contcor1", "contcor2"))
})

# ---- Winsorized, one-sided less, custom trim -----------------------------

test_that("Robust correlation (Winsorized, less, trim 0.1) main table matches", {
  options <- .initCorrelationRobustOptions()
  options$variables         <- c("contNormal", "contcor1")
  options$correlationMethod <- "winsorized"
  options$trimProportion    <- 0.1
  options$alternative       <- "less"

  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationRobust", "debug.csv", options)

  table <- results[["results"]][["correlationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.137918403022772, 0.914004385584259, 1.37849599898171, "contNormal",
         "contcor1"))
})

# ---- Scatter plots --------------------------------------------------------

test_that("Robust correlation scatter plot matches", {
  options <- .initCorrelationRobustOptions()
  options$variables         <- c("contNormal", "contcor1")
  options$correlationMethod <- "percentageBend"
  options$trimProportion    <- 0.2
  options$alternative       <- "twoSided"
  options$scatterPlot       <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("CorrelationRobust", "debug.csv", options)

  plotName <- results[["results"]][["scatterPlotContainer"]][["collection"]][["scatterPlotContainer_contNormal-contcor1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "corrobust-scatter-contnormal-contcor1")
})
