context("Robust ANOVA")

# Reference tables captured via jaspTools::makeTestTable on debug.csv with
# set.seed(1). Plot snapshots are created on first run -- inspect manually
# before accepting.

.initAnovaRobustOptions <- function() {
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

# ---- One-way, trimmed means ----------------------------------------------

test_that("Robust ANOVA (trimmed means, one-way) main table matches", {
  options <- .initAnovaRobustOptions()
  options$dependent       <- "contNormal"
  options$fixedFactors    <- "facFive"
  options$robustMethod    <- "trimmedMeans"
  options$trimProportion  <- 0.2
  options$descriptivesTable <- TRUE
  options$effectSizeTable   <- TRUE
  options$postHocTerms <- list(list(components = "facFive"))
  options$postHocCorrectionHochberg   <- TRUE
  options$postHocCorrectionBonferroni <- TRUE
  options$postHocCorrectionHolm       <- TRUE
  options$postHocCi <- TRUE
  options$postHocCiLevel <- 0.95
  options$postHocSignificanceFlag <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRobust", "debug.csv", options)

  # Main ANOVA table
  table <- results[["results"]][["anovaRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4, 27.3953740127667, 0.246967268889078, 0.42761027285612, 0.993766203263345))

  # Descriptives
  table <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.9396690222885, -0.404761084, -0.6449013935, 20, 0.601119310135569,
         2, 0.7720385997639, -0.400448821333333, -0.4523285395, 20, 0.438172452131876,
         3, 0.8905697647602, 0.07770341025, 0.100563719, 20, 0.58841384144579,
         4, 0.7166765840871, -0.392882583083333, -0.405936197, 20, 0.498691006837332,
         5, 0.8534667412593, -0.12925203875, -0.115966126, 20, 0.55005081732598))

  # Effect sizes (pairwise akp.effect)
  table <- results[["results"]][["effectSizeTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-1.32149443071732, 0.6274121659023, 1, 2, -0.0046050976190931,
         -1.86582955108108, 0.344089918436359, 1, 3, -0.515227449139839,
         -1.26235067429917, 0.571579118105199, 1, 4, -0.0126851401498742,
         -1.68271214899105, 0.473716773157129, 1, 5, -0.294218174167974,
         -2.24997562385921, 0.126554761806835, 2, 3, -0.700511760135876,
         -0.988097237079746, 0.67071177017647, 2, 4, -0.0110848355900461,
         -1.65382110994035, 0.283467919647542, 2, 5, -0.397313915866411,
         -0.130102098777841, 1.68384825898989, 3, 4, 0.513393575772664,
         -0.374121160828434, 1.34436568387164, 3, 5, 0.225781471384523,
         -1.49568677459518, 0.474886369689868, 4, 5, -0.339358321824593))

  # Post hoc
  table <- results[["results"]][["postHocContainer"]][["collection"]][["postHocContainer_facFive"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.881524711001107, 0.872900185667774, 1, 2, -0.00431226266666668,
         1, 0.98795944123868, 1, "", -1.46631427367234, 0.501385285172341,
         1, 3, -0.48246449425, 1, 0.98795944123868, 1, "", -0.928122865869736,
         0.904365864036403, 1, 4, -0.0118785009166667, 1, 0.98795944123868,
         1, "", -1.22913386774226, 0.678115777242264, 1, 5, -0.27550904525,
         1, 0.98795944123868, 1, "", -1.34242531699602, 0.386120853829355,
         2, 3, -0.478152231583333, 1, 0.98795944123868, 1, "", -0.78511203830219,
         0.76997956180219, 2, 4, -0.00756623825000002, 1, 0.98795944123868,
         1, "", -1.0973231801822, 0.554929615015536, 2, 5, -0.271196782583333,
         1, 0.98795944123868, 1, "", -0.433660997855105, 1.37483298452177,
         3, 4, 0.470585993333333, 1, 0.98795944123868, 1, "", -0.735476513223632,
         1.14938741122363, 3, 5, 0.206955449, 1, 0.98795944123868, 1,
         "", -1.1327217886924, 0.605460700025734, 4, 5, -0.263630544333333,
         1, 0.98795944123868, 1, ""))
})

# ---- One-way, bootstrap trimmed means ------------------------------------

test_that("Robust ANOVA (bootstrap one-way) main table matches", {
  options <- .initAnovaRobustOptions()
  options$dependent       <- "contNormal"
  options$fixedFactors    <- "facFive"
  options$robustMethod    <- "trimmedMeansBootstrap"
  options$trimProportion  <- 0.2
  options$bootstrapSamples <- 199
  options$postHocTerms <- list(list(components = "facFive"))

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRobust", "debug.csv", options)

  table <- results[["results"]][["anovaRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.246967268889078, 0.396984924623116, 0.993766203263345, 0.06099283190253))

  table <- results[["results"]][["postHocContainer"]][["collection"]][["postHocContainer_facFive"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 2, -0.00431226266666668, 0.904522613065327, 1, 3, -0.48246449425,
         0.150753768844221, 1, 4, -0.0118785009166667, 0.984924623115578,
         1, 5, -0.27550904525, 0.422110552763819, 2, 3, -0.478152231583333,
         0.0804020100502513, 2, 4, -0.00756623825000002, 0.994974874371859,
         2, 5, -0.271196782583333, 0.361809045226131, 3, 4, 0.470585993333333,
         0.110552763819096, 3, 5, 0.206955449, 0.482412060301508, 4,
         5, -0.263630544333333, 0.311557788944724))
})

# ---- One-way, medians ----------------------------------------------------

test_that("Robust ANOVA (medians) main table matches", {
  options <- .initAnovaRobustOptions()
  options$dependent      <- "contNormal"
  options$fixedFactors   <- "facFive"
  options$robustMethod   <- "medians"

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRobust", "debug.csv", options)

  table <- results[["results"]][["anovaRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.53922446058501, 0.29, 1.19427168447303))
})

# ---- Two-way, trimmed means ----------------------------------------------

test_that("Robust ANOVA (two-way trimmed means) main table matches", {
  options <- .initAnovaRobustOptions()
  options$dependent      <- "contNormal"
  options$fixedFactors   <- c("facGender", "facExperim")
  options$robustMethod   <- "trimmedMeans"
  options$trimProportion <- 0.2
  options$descriptivesTable <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRobust", "debug.csv", options)

  table <- results[["results"]][["anovaRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("facGender", 0.063, 3.71084847310373, "facExperim", 0.624, 0.245671317146405,
         "facGender <unicode> facExperim", 0.711, 0.14081991222944))

  table <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("control", "f", 0.546858107124, -0.500962208736842, -0.611664173,
         29, 0.452514724186531, "control", "m", 1.4220581965338, -0.0355878467692308,
         0.108920494, 21, 0.86398130474699, "experimental", "f", 0.6283146997134,
         -0.324868454076923, -0.412828649, 21, 0.447663326619839, "experimental",
         "m", 0.707616469602, -0.0112451077368421, 0.166778121, 29, 0.556695979475909))
})

# ---- Plots (raincloud + descriptives plot, one-way) ----------------------

test_that("Robust ANOVA descriptives and raincloud plots match", {
  options <- .initAnovaRobustOptions()
  options$dependent    <- "contNormal"
  options$fixedFactors <- "facFive"
  options$robustMethod <- "trimmedMeans"
  options$descriptivePlotHorizontalAxis <- "facFive"
  options$descriptivePlotErrorBar       <- TRUE
  options$descriptivePlotErrorBarType   <- "ci"
  options$descriptivePlotCiLevel        <- 0.95
  options$rainCloudHorizontalAxis <- "facFive"

  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRobust", "debug.csv", options)

  descPlotName <- results[["results"]][["descriptivesPlotContainer"]][["collection"]][["descriptivesPlotContainer_plot_"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[descPlotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "anovarobust-descriptives")

  rainPlotName <- results[["results"]][["rainCloudContainer"]][["collection"]][["rainCloudContainer_rainCloud_"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[rainPlotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "anovarobust-raincloud")
})
