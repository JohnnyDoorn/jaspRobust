context("Robust Linear Regression")

# Reference tables captured via jaspTools::makeTestTable on debug.csv with
# set.seed(1). Plot snapshots are created on first run -- inspect manually
# before accepting.

.initRegressionRobustOptions <- function() {
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

# ---- Huber M-estimation --------------------------------------------------

test_that("Robust regression (Huber) coefficient table matches", {
  options <- .initRegressionRobustOptions()
  options$dependent  <- "contNormal"
  options$covariates <- c("contGamma", "contcor1")
  options$factors    <- "facGender"
  options$estimationMethod   <- "huber"
  options$coefficientEstimate <- TRUE
  options$coefficientCi       <- TRUE
  options$coefficientCiLevel  <- 0.95
  options$descriptivesTable   <- TRUE
  options$descriptivesTrimProportion <- 0.2

  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinearRobust", "debug.csv", options)

  table <- results[["results"]][["coefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.723887124682077, -0.0325173412113011, -0.378202232946689, 0.0320063917769555,
         0.176373083618937, "(Intercept)", -2.14433078555123, -0.15384683188197,
         0.0925798558144423, -0.0306334880337638, 0.626053391473507,
         0.0628651061040393, "contGamma", -0.487289212286806, -0.062367447468289,
         0.307539468321495, 0.122586010426603, 0.193926073025473, 0.0943657431227214,
         "contcor1", 1.29905203276131, 0.03972197621644, 0.782438142542405,
         0.411080059379422, 0.0300364235306221, 0.189471891367498, "facGenderm",
         2.16960973162027))

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("<unicode>", 23, 100, 0.796940853777074))

  table <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.9382150060752, -0.249904344716667, -0.38335354, 100, "contNormal",
         0.564965989561465, 1.3904477575464, 1.72587557315, 1.6961274775,
         100, "contGamma", 0.838930241377002, 1.2122742046674, 0.0105420493,
         0.003635444, 100, "contcor1", 0.708177014442337))
})

# ---- Bisquare M-estimation -----------------------------------------------

test_that("Robust regression (Bisquare) coefficient table matches", {
  options <- .initRegressionRobustOptions()
  options$dependent  <- "contNormal"
  options$covariates <- "contGamma"
  options$estimationMethod    <- "bisquare"
  options$coefficientEstimate <- TRUE
  options$coefficientCi       <- TRUE
  options$coefficientCiLevel  <- 0.95

  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinearRobust", "debug.csv", options)

  table <- results[["results"]][["coefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.556108874470742, 0.0748958419550429, -0.24060651625785, 0.134993804079147,
         0.16097354885168, "(Intercept)", -1.49469597939685, -0.136914023743508,
         0.111395080365067, -0.0127594716892206, 0.840364541059729, 0.0633453231965499,
         "contGamma", -0.201427209545212))
})

# ---- MM-estimation -------------------------------------------------------

test_that("Robust regression (MM) coefficient table matches", {
  options <- .initRegressionRobustOptions()
  options$dependent  <- "contNormal"
  options$covariates <- "contGamma"
  options$estimationMethod    <- "mm"
  options$coefficientEstimate <- TRUE
  options$coefficientCi       <- TRUE
  options$coefficientCiLevel  <- 0.99

  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinearRobust", "debug.csv", options)

  table <- results[["results"]][["coefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.645631148067342, 0.184613472564534, -0.230508837751404, 0.152629567455205,
         0.161160644357914, "(Intercept)", -1.43030476621499, -0.178363301782976,
         0.148349466875563, -0.0150069174537061, 0.812942721143063, 0.0634189478721288,
         "contGamma", -0.236631447812166))

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("<unicode>", 100, 100, 0.925895589723393))
})

# ---- Diagnostic plots ----------------------------------------------------

test_that("Robust regression diagnostic plots match", {
  options <- .initRegressionRobustOptions()
  options$dependent  <- "contNormal"
  options$covariates <- "contGamma"
  options$estimationMethod     <- "huber"
  options$residualVsFittedPlot <- TRUE
  options$residualQqPlot       <- TRUE
  options$weightsPlot          <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("RegressionLinearRobust", "debug.csv", options)

  plotName <- results[["results"]][["residualVsFittedPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "regrobust-resvfitted")

  plotName <- results[["results"]][["residualQqPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "regrobust-qq")

  plotName <- results[["results"]][["weightsPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "regrobust-weights")
})
