#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Shared implementation for both ANOVA (no covariates) and ANCOVA (with covariates).
# AnovaInternal delegates here directly.

.linregRobustIsRobust <- function(options) {
  !is.null(options$estimationMethod) && options$estimationMethod != "ols"
}

RegressionLinearInternal <- function(jaspResults, dataset, options) {

  nModels <- length(options[["modelTerms"]])
  ready <- options$dependent != "" && (length(options[["modelTerms"]][[nModels]][["components"]]) > 0 || options$interceptTerm)

  if (ready) {
    numericVariables <- c(options$dependent, unlist(options$covariates), options$weights)
    numericVariables <- numericVariables[numericVariables != ""]
    factors <- unlist(options$factors)
    dataset <- excludeNaListwise(dataset, columns = c(factors, numericVariables))
    .linregRobustCheckErrors(dataset, options)
  }

  modelContainer  <- .linregRobustGetModelContainer(jaspResults, position = 1)
  model           <- .linregRobustCalcModel(modelContainer, dataset, options, ready)

  # these output elements show statistics of all the lm fits
  if (is.null(modelContainer[["summaryTable"]]))
    .linregRobustCreateSummaryTable(modelContainer, model, options, position = 1)

  if (options$modelFit && is.null(modelContainer[["anovaTable"]]))
    .linregRobustCreateAnovaTable(modelContainer, model, options, position = 2)

  if (options$coefficientEstimate && is.null(modelContainer[["coeffTable"]]))
    .linregRobustCreateCoefficientsTable(modelContainer, model, dataset, options, position = 3)

  if (options$coefficientBootstrap && is.null(modelContainer[["bootstrapCoeffTable"]]))
    .linregRobustCreateBootstrapCoefficientsTable(modelContainer, model, dataset, options, position = 4)

  if (options$equationTable && is.null(modelContainer[["equationTable"]]))
    .linregRobustCreateEquationTable(modelContainer, model, dataset, options, position = 6)

  if (options$partAndPartialCorrelation && is.null(modelContainer[["partialCorTable"]]))
    .linregRobustCreatePartialCorrelationsTable(modelContainer, model, dataset, options, position = 6)

  if (options$covarianceMatrix && is.null(modelContainer[["coeffCovMatrixTable"]]))
    .linregRobustCreateCoefficientsCovarianceMatrixTable(modelContainer, model, options, position = 7)

  if (options$collinearityDiagnostic && is.null(modelContainer[["collinearityTable"]]))
    .linregRobustCreateCollinearityDiagnosticsTable(modelContainer, model, options, position = 8)

  # these output elements show statistics of the "final model" (lm fit with all predictors in enter method and last lm fit in stepping methods)
  finalModel <- model[[length(model)]]

  if (options$residualCasewiseDiagnostic && is.null(modelContainer[["influenceTable"]]))
    .linregRobustInfluenceTable(modelContainer, finalModel[["fit"]], dataset, options, ready = ready, position = 9)

  if (options$residualStatistic && is.null(modelContainer[["residualsTable"]]))
    .linregRobustCreateResidualsTable(modelContainer, finalModel, options, position = 10)

  if (options$residualVsDependentPlot && is.null(modelContainer[["residualsVsDepPlot"]]))
     .linregRobustCreateResidualsVsDependentPlot(modelContainer, finalModel, options, position = 11)

  if (options$residualVsCovariatePlot && is.null(modelContainer[["residualsVsCovContainer"]]))
    .linregRobustCreateResidualsVsCovariatesPlots(modelContainer, finalModel, dataset, options, position = 12)

  if (options$residualVsFittedPlot && is.null(modelContainer[["residualsVsPredPlot"]]))
    .linregRobustCreateResidualsVsPredictedPlot(modelContainer, finalModel, options, position = 13)

  if (options$residualHistogramPlot && is.null(modelContainer[["residualsVsHistPlot"]]))
    .linregRobustCreateResidualsVsHistogramPlot(modelContainer, finalModel, options, position = 14)

  if (options$residualQqPlot && is.null(modelContainer[["residualsQQPlot"]]))
    .linregRobustCreateResidualsQQPlot(modelContainer, finalModel, options, position = 15)

  if (options$marginalPlot && is.null(modelContainer[["marginalPlotsContainer"]]))
    .linregRobustCreateMarginalPlots(modelContainer, finalModel, dataset, options, position = 17)

  # these output elements do not use statistics of a pre-calculated lm fit
  if (options$partialResidualPlot && is.null(modelContainer[["partialPlotContainer"]]))
    .linregRobustCreatePartialPlots(modelContainer, dataset, options, position = 16)
  if (options$descriptives && is.null(modelContainer[["descriptivesTable"]]))
    .linregRobustCreateDescriptivesTable(modelContainer, dataset, options, position = 5)
}

.linregRobustCheckErrors <- function(dataset, options) {
  defaultTarget <- c(options$dependent, unlist(options$covariates))
  errorTypes <- c("infinity", "variance", "observations")
  # varCovData check requires 2+ covariates to compute covariance matrix
  if (length(options$covariates) >= 2)
    errorTypes <- c(errorTypes, "varCovData")

  .hasErrors(dataset, type = errorTypes,
             observations.amount = "< 2",
             observations.target = defaultTarget,

             varCovData.target = unlist(options$covariates),
             varCovData.corFun = stats::cov,

             exitAnalysisIfErrors = TRUE)

  if (options$weights != "") {
    .hasErrors(dataset, type = c("infinity", "limits", "observations"),
               all.target = options$weights, limits.min = 0, observations.amount = "< 2",
               exitAnalysisIfErrors = TRUE)
    if (length(options$factors) != 0)
      .hasErrors(dataset,
                 type = "factorLevels",
                 factorLevels.target  = options$factors,
                 factorLevels.amount  = '< 2',
                 exitAnalysisIfErrors = TRUE)
    # varCovData check requires 2+ covariates/factors to compute covariance matrix
    if (length(c(options$covariates, options$factors)) >= 2) {
      covwt <- function(...) return(stats::cov.wt(..., wt = dataset[[options[["weights"]]]])$cov)
      .hasErrors(dataset[, -which(colnames(dataset) %in% c(options$weights))],  type = "varCovData", varCovData.corFun = covwt,
                 exitAnalysisIfErrors = TRUE)
    }
  }
}

.linregRobustGetModelContainer <- function(jaspResults, position) {
  if (is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c("dependent", "covariates", "factors", "weights", "modelTerms",
                              "interceptTerm", "quadraticTerms", "estimationMethod"))
    modelContainer$position <- position
    jaspResults[["modelContainer"]] <- modelContainer
  }
  return(jaspResults[["modelContainer"]])
}

.linregRobustCreateSummaryTable <- function(modelContainer, model, options, position) {
  if(options[['dependent']] == "")
    summaryTable <- createJaspTable(gettext("Model Summary"))
  else
    summaryTable <- createJaspTable(gettextf("Model Summary - %s", options[['dependent']]))

  summaryTable$dependOn(c("residualDurbinWatson", "rSquaredChange", "fChange", "modelAICBIC"))
  summaryTable$position <- position
  summaryTable$showSpecifiedColumnsOnly <- TRUE

  summaryTable$addColumnInfo(name = "model",  title = gettext("Model"),                    type = "string")
  summaryTable$addColumnInfo(name = "R",      title = gettext("R"),                        type = "number", format = "dp:3")
  summaryTable$addColumnInfo(name = "R2",     title = gettextf("R%s", "\u00B2"),           type = "number", format = "dp:3")
  summaryTable$addColumnInfo(name = "adjR2",  title = gettextf("Adjusted R%s", "\u00B2"),  type = "number", format = "dp:3")
  summaryTable$addColumnInfo(name = "RMSE",   title = gettext("RMSE"),                     type = "number")

  if (options$modelAICBIC) {
    summaryTable$addColumnInfo(name = "AIC",      title = gettext("AIC"),                  type = "number", format = "dp:3")
    summaryTable$addColumnInfo(name = "BIC",      title = gettext("BIC"),                  type = "number", format = "dp:3")
  }

  if (options$rSquaredChange || options$fChange) {
    if (options$rSquaredChange)
        summaryTable$addColumnInfo(name = "R2c",  title = gettextf("R%s Change", "\u00B2"), type = "number", format = "dp:3")
    if (options$fChange)
        summaryTable$addColumnInfo(name = "Fc",   title = gettext("F Change"),              type = "number")
    summaryTable$addColumnInfo(name = "df1",  title = gettext("df1"),                   type = "integer")
    summaryTable$addColumnInfo(name = "df2",  title = gettext("df2"),                   type = "integer")
    summaryTable$addColumnInfo(name = "p",    title = gettext("p"),                     type = "pvalue")
  }

  if (options$residualDurbinWatson) {
    summaryTable$addColumnInfo(name = "DW_ac",  title = gettext("Autocorrelation"),  type = "number", overtitle = gettext("Durbin-Watson"))
    summaryTable$addColumnInfo(name = "DW",     title = gettext("Statistic"),        type = "number", overtitle = gettext("Durbin-Watson"))

    if (options$weights == "")
      summaryTable$addColumnInfo(name = "DW_p", title = gettext("p"), type = "pvalue", overtitle = "Durbin-Watson")
    else
      summaryTable$addFootnote(message = gettext("p-value for Durbin-Watson test is unavailable for weighted regression."))
  }

  for (i in seq_along(options$modelTerms))
    .linregRobustAddPredictorsInModelFootnote(summaryTable, options[["modelTerms"]][[i]][["components"]], i, options[["covariates"]], options[["quadraticTerms"]])

  if (!is.null(model)) {
    if (length(model) == 1 && length(model[[1]]$predictors) == 0 && !options$interceptTerm)
      summaryTable$addFootnote(gettext("No covariate could be entered in the model"))
    else
      .linregRobustFillSummaryTable(summaryTable, model)

    if (.linregRobustIsRobust(options)) {
      methodLabel <- if (options$estimationMethod == "huber") "Huber" else "bisquare"
      summaryTable$addFootnote(gettextf("M-estimation with %s weight function. R\u00B2 is a pseudo-R\u00B2 based on unweighted residuals.", methodLabel))
    }
  }

  modelContainer[["summaryTable"]] <- summaryTable
}

.linregRobustFillSummaryTable <- function(summaryTable, model) {
  for (i in seq_along(model)) {
    lmSummary     <- model[[i]][["summary"]]
    rSquareChange <- model[[i]][["rSquareChange"]]
    durbinWatson  <- model[[i]][["durbinWatson"]]

    aic <- tryCatch(as.numeric(AIC(model[[i]][["fit"]])), error = function(e) NaN)
    bic <- tryCatch(as.numeric(BIC(model[[i]][["fit"]])), error = function(e) NaN)

    summaryTable$addRows(list(
      model = model[[i]]$title,
      R     = as.numeric(sqrt(lmSummary$r.squared)),
      R2    = as.numeric(lmSummary$r.squared),
      adjR2 = as.numeric(lmSummary$adj.r.squared),
      AIC   = aic,
      BIC   = bic,
      RMSE  = as.numeric(lmSummary$sigma),
      R2c   = rSquareChange$R2c,
      Fc    = rSquareChange$Fc,
      df1   = rSquareChange$df1,
      df2   = rSquareChange$df2,
      p     = rSquareChange$p,
      DW_ac = durbinWatson$r,
      DW    = durbinWatson$dw,
      DW_p  = durbinWatson$p
    ))
  }
}

.linregRobustCreateAnovaTable <- function(modelContainer, model, options, position) {
  anovaTable <- createJaspTable(gettext("ANOVA"))
  anovaTable$dependOn(c("modelFit", "vovkSellke"))
  anovaTable$position <- position
  anovaTable$showSpecifiedColumnsOnly <- TRUE

  anovaTable$addColumnInfo(name = "model", title = gettext("Model"),          type = "string", combine = TRUE)
  anovaTable$addColumnInfo(name = "cases", title = "",                        type = "string")
  anovaTable$addColumnInfo(name = "SS",    title = gettext("Sum of Squares"), type = "number")
  anovaTable$addColumnInfo(name = "df",    title = gettext("df"),             type = "integer")
  anovaTable$addColumnInfo(name = "MS",    title = gettext("Mean Square"),    type = "number")
  anovaTable$addColumnInfo(name = "F",     title = gettext("F"),              type = "number")
  anovaTable$addColumnInfo(name = "p",     title = gettext("p"),              type = "pvalue")

  for (i in seq_along(options$modelTerms))
    .linregRobustAddPredictorsInModelFootnote(anovaTable, options[["modelTerms"]][[i]][["components"]], i, options[["covariates"]], options[["quadraticTerms"]])

  .linregRobustAddVovkSellke(anovaTable, options$vovkSellke)

  if (!is.null(model)) {
    .linregRobustAddInterceptNotShownFootnote(anovaTable, model, options)
    .linregRobustFillAnovaTable(anovaTable, model, options)
  }

  modelContainer[["anovaTable"]] <- anovaTable
}

.linregRobustFillAnovaTable <- function(anovaTable, model, options) {
  rowTypes <- list(Regression = gettext("Regression"), Residual = gettext("Residual"), Total = gettext("Total"))

  indicesOfModelsWithPredictors <- .linregRobustGetIndicesOfModelsWithPredictors(model, options)
  for (i in indicesOfModelsWithPredictors) {
    isNewGroup  <- i > 1
    anovaRes    <- .linregRobustGetAnova(model[[i]]$fit, model[[i]]$predictors)

    for (rowType in names(rowTypes)) {
      anovaTable$addRows(c(anovaRes[[rowType]], list(.isNewGroup = isNewGroup, model = model[[i]]$title, cases = rowTypes[[rowType]])))
      isNewGroup <- FALSE
    }
  }
}

.linregRobustCreateCoefficientsTable <- function(modelContainer, model, dataset, options, position) {

  coeffTable <- createJaspTable(gettext("Coefficients"))
  coeffTable$dependOn(c("coefficientEstimate", "coefficientCi", "coefficientCiLevel",
                        "collinearityStatistic", "vovkSellke"))
  coeffTable$position <- position
  coeffTable$showSpecifiedColumnsOnly <- TRUE

  coeffTable$addColumnInfo(name = "model",        title = gettext("Model"),          type = "string", combine = TRUE)
  coeffTable$addColumnInfo(name = "name",         title = "",                        type = "string")
  coeffTable$addColumnInfo(name = "unstandCoeff", title = gettext("Unstandardized"), type = "number")
  coeffTable$addColumnInfo(name = "SE",           title = gettext("Standard Error"), type = "number")
  coeffTable$addColumnInfo(name = "standCoeff",   title = gettext("Standardized"),   type = "number")
  coeffTable$addColumnInfo(name = "t",            title = gettext("t"),              type = "number")
  coeffTable$addColumnInfo(name = "p",            title = gettext("p"),              type = "pvalue")

  .linregRobustAddVovkSellke(coeffTable, options$vovkSellke)

  if (options$coefficientCi) {
    overtitle <- gettextf("%.0f%% CI", 100 * options$coefficientCiLevel)
    coeffTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    coeffTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  if (options$collinearityStatistic) {
    overtitle <- gettext("Collinearity Statistics")
    coeffTable$addColumnInfo(name = "tolerance",  title = gettext("Tolerance"),  type = "number", format = "dp:3", overtitle = overtitle)
    coeffTable$addColumnInfo(name = "VIF",        title = gettext("VIF"),        type = "number",                  overtitle = overtitle)

  }

  if (!is.null(model)) {
    .linregRobustAddFootnoteFactors(coeffTable, options[["factors"]], options[["collinearityDiagnostic"]], options[["quadraticTerms"]])
    .linregRobustFillCoefficientsTable(coeffTable, model, dataset, options)

    if (.linregRobustIsRobust(options))
      coeffTable$addFootnote(gettext("p-values are approximate, based on the t-distribution with residual degrees of freedom."))
  }

  modelContainer[["coeffTable"]] <- coeffTable
}

.linregRobustAddFootnoteFactors <- function(table, factors, collinearityDiagnostics = FALSE, quadraticTerms = FALSE) {
  if (length(factors) > 0L || quadraticTerms) {
    colNames <- "standCoeff"
    if (quadraticTerms)
      message <- gettext("Standardized coefficients can only be computed for continuous, linear predictors.")
    else
      message <- gettext("Standardized coefficients can only be computed for continuous predictors.")
    table$addFootnote(colNames = colNames, message = message)
  }
}

.linregRobustAddFootnoteAliasedCoefficients <- function(table, rows) {
  table$addFootnote(gettext("Missing coefficients are undefined because of singularities. Check the data for anything out of order!"))
}

.linregRobustFillCoefficientsTable <- function(coeffTable, model, dataset, options) {
  for (i in seq_along(model)) {
    isNewGroup <- i > 1

    temp <- .linregRobustGetCoefficients(model[[i]]$fit, model[[i]]$predictors, dataset, options)
    coefficients <- temp[["coefficients"]]
    footnoteRows <- temp[["footnote"]]

    for (j in seq_along(coefficients)) {
      coeffTable$addRows(c(coefficients[[j]], list(.isNewGroup = isNewGroup, model = model[[i]]$title)))
      isNewGroup <- FALSE
    }

    # TODO: should this be done only after joining all obtained footnotes?
    if (!is.null(footnoteRows))
      .linregRobustAddFootnoteAliasedCoefficients(coeffTable, footnoteRows)

  }
}

.linregRobustCreateBootstrapCoefficientsTable <- function(modelContainer, model, dataset, options, position) {
  bootstrapCoeffTable <- createJaspTable(gettext("Bootstrap Coefficients"))
  bootstrapCoeffTable$dependOn(c("coefficientEstimate", "coefficientCi", "coefficientCiLevel",
                                 "coefficientBootstrap", "coefficientBootstrapSamples"))
  bootstrapCoeffTable$position <- position
  bootstrapCoeffTable$showSpecifiedColumnsOnly <- TRUE

  bootstrapCoeffTable$addColumnInfo(name = "model",        title = gettext("Model"),          type = "string", combine = TRUE)
  bootstrapCoeffTable$addColumnInfo(name = "name",         title = "",                        type = "string")
  bootstrapCoeffTable$addColumnInfo(name = "unstandCoeff", title = gettext("Unstandardized"), type = "number")
  bootstrapCoeffTable$addColumnInfo(name = "bias",         title = gettext("Bias"),           type = "number")
  bootstrapCoeffTable$addColumnInfo(name = "SE",           title = gettext("Standard Error"), type = "number")
  bootstrapCoeffTable$addColumnInfo(name = "pvalue",       title = gettext("p"),              type = "pvalue")

  if (options$coefficientCi) {
    overtitle <- gettextf("%s%% CI\u002A", 100 * options$coefficientCiLevel)
    bootstrapCoeffTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    bootstrapCoeffTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  bootstrapCoeffTable$addFootnote(gettextf("Bootstrapping based on %i replicates.", options[['coefficientBootstrapSamples']]))
  bootstrapCoeffTable$addFootnote(gettext("Coefficient estimate is based on the median of the bootstrap distribution."))
  bootstrapCoeffTable$addFootnote(gettext("Bias corrected accelerated."), colNames = "pvalue", symbol = "\u002A")

  bootstrapCoeffTable$addCitation(
    c("Efron, B., & Tibshirani, R. J. (1994). An introduction to the bootstrap. CRC press.",
      "Hall, P. (1992). The bootstrap and Edgeworth expansion. Springer Science & Business Media.")
  )

  modelContainer[["bootstrapCoeffTable"]] <- bootstrapCoeffTable
  if (!is.null(model))
    .linregRobustFillBootstrapCoefficientsTable(bootstrapCoeffTable, modelContainer, model, dataset, options)

}

.linregRobustFillBootstrapCoefficientsTable <- function(bootstrapCoeffTable, modelContainer, model, dataset, options) {

  metaCols <- .linregRobustGetTitlesAndIsNewGroups(model)

  if (is.null(modelContainer[["bootstrapCoefficients"]])) {
    startProgressbar(options$coefficientBootstrapSamples * length(model))
    bootstrapCoeffTable$setData(metaCols)

    anyMissingValues <- FALSE
    coefficients <- NULL

    for (i in seq_along(model)) {
      data <- .linregRobustGetBootstrapCoefficients(model[[i]]$fit, dataset, options)
      anyMissingValues <- anyNA(data[-1L])
      coefficients <- rbind(coefficients, data)
      bootstrapCoeffTable$setData(.linregRobustCombineMetaWithData(metaCols, coefficients))
    }

    if (anyMissingValues)
      bootstrapCoeffTable$addFootnote(gettext("Some bootstrap results could not be computed."))

    modelContainer[["bootstrapCoefficients"]] <- createJaspState(coefficients)
    modelContainer[["bootstrapCoefficients"]]$dependOn(c("coefficientBootstrapSamples", "coefficientCiLevel"))
  } else {
    bootstrapCoeffTable$setData(.linregRobustCombineMetaWithData(metaCols, modelContainer[["bootstrapCoefficients"]]$object))
  }
}

.linregRobustGetTitlesAndIsNewGroups <- function(model, includeConstant) {
  isNewGroup  <- logical(0)
  titles      <- character(0)
  for (i in seq_along(model)) {
    if (is.null(model[[i]]$fit))
      next

    numPredictors <- length(stats::coef(model[[i]]$fit))

    isNewGroupCurrent <- i > 1
    if (numPredictors > 1)
      isNewGroupCurrent <- c(isNewGroupCurrent, logical(numPredictors - 1))

    isNewGroup  <- c(isNewGroup, isNewGroupCurrent)
    titles      <- c(titles, rep(model[[i]]$title, numPredictors))
  }

  return(data.frame(.isNewGroup = isNewGroup, model = titles))
}

.linregRobustCombineMetaWithData <- function(meta, data) { # this can go once we can add cells to a jaspTable
  if (!is.data.frame(meta) || !is.data.frame(data))
    stop(gettext("expecting both arguments to be data.frames"))

  filler            <- matrix(NA, ncol(data), nrow = nrow(meta)-nrow(data))
  colnames(filler)  <- names(data)
  data              <- rbind(data, filler)

  return(cbind(meta, data))
}

.linregRobustCreateEquationTable <- function(modelContainer, model, dataset, options, position = 6) {

  equationTable <- createJaspTable(gettext("Regression Equations"))
  equationTable$dependOn("equationTable")
  equationTable$position <- position

  equationTable$addColumnInfo(name = "model",   title = gettext("Model"),   type = "string", combine = TRUE)
  equationTable$addColumnInfo(name = "formula",   title = gettext("Equation"),   type = "string")

  if (!is.null(model)) {

    for (i in seq_along(model)) {

      coefs <- coef(model[[i]][["fit"]])
      names(coefs) <- .linregRobustMakePrettyNames(model[[i]][["fit"]])

      coefFormula <- paste(ifelse(sign(coefs[-1])==1, " +", " \u2013"),
                           round(abs(coefs[-1]), .numDecimals),
                           names(coefs)[-1],
                           collapse = "", sep = " ")
      .linregRobustGetParametersAndLevels(model[[i]][["fit"]])
      # Now add dependent name and intercept
      filledFormula <- paste0(options[["dependent"]], " = ",
                              round(coefs[1], .numDecimals),
                              coefFormula)

      equationTable$addRows(list(
        model = model[[i]]$title,
        formula = filledFormula
      ))
    }
  }

  modelContainer[["equationTable"]] <- equationTable

}


.linregRobustCreatePartialCorrelationsTable <- function(modelContainer, model, dataset, options, position) {
  partPartialTable <- createJaspTable(gettext("Part And Partial Correlations"))
  partPartialTable$dependOn("partAndPartialCorrelation")
  partPartialTable$position <- position

  partPartialTable$addColumnInfo(name = "model",   title = gettext("Model"),   type = "string", combine = TRUE)
  partPartialTable$addColumnInfo(name = "name",    title = "",                 type = "string")
  partPartialTable$addColumnInfo(name = "partial", title = gettext("Partial"), type = "number", format = "dp:3")
  partPartialTable$addColumnInfo(name = "part",    title = gettext("Part"),    type = "number", format = "dp:3")

  if (!is.null(model)) {
    .linregRobustAddInterceptNotShownFootnote(partPartialTable, model, options)
    .linregRobustFillPartialCorrelationsTable(partPartialTable, model, dataset, options)
  }

  modelContainer[["partialCorTable"]] <- partPartialTable
}

.linregRobustFillPartialCorrelationsTable <- function(partPartialTable, model, dataset, options) {
  indicesOfModelsWithPredictors <- .linregRobustGetIndicesOfModelsWithPredictors(model, options)
  for (i in indicesOfModelsWithPredictors) {
    isNewGroup <- i > 1

    cors <- .linregRobustGetPartAndPartialCorrelation(model[[i]]$fit, model[[i]]$predictors, dataset, options)

    for (j in seq_along(cors)) {
      partPartialTable$addRows(c(cors[[j]], list(.isNewGroup = isNewGroup, model = model[[i]]$title)))
      isNewGroup <- FALSE
    }
  }
}

.linregRobustCreateCoefficientsCovarianceMatrixTable <- function(modelContainer, model, options, position) {
  covMatTable <- createJaspTable(gettext("Coefficients Covariance Matrix"))
  covMatTable$dependOn("covarianceMatrix")
  covMatTable$position <- position

  covMatTable$addColumnInfo(name = "model", title = gettext("Model"), type = "string", combine = TRUE)
  covMatTable$addColumnInfo(name = "name",  title = "",               type = "string")

  if (!is.null(model)) {
    .linregRobustAddPredictorsAsColumns(covMatTable, model, includeIntercept = FALSE)
    .linregRobustAddInterceptNotShownFootnote(covMatTable, model, options)
    .linregRobustFillCoefficientsCovarianceMatrixTable(covMatTable, model, options)
  }

  modelContainer[["coeffCovMatrixTable"]] <- covMatTable
}

.linregRobustFillCoefficientsCovarianceMatrixTable <- function(covMatTable, model, options) {
  rawNames    <- setdiff(.linregRobustGetParameterNames(model), "(Intercept)")          # names used by R
  prettyNames <- setdiff(.linregRobustMakePrettyNames(model),   gettext("(Intercept)")) # names shown in JASP

  indicesOfModelsWithPredictors <- .linregRobustGetIndicesOfModelsWithPredictors(model, options)
  for (i in indicesOfModelsWithPredictors) {
    isNewGroup <- i > 1

    covData <- .linregRobustGetCovarianceMatrix(model[[i]]$fit, rawNames, prettyNames)
    if (nrow(covData) > 0) {
      covData <- cbind(covData, .isNewGroup = c(isNewGroup, rep(F, nrow(covData) - 1)), model = model[[i]]$title)
      covMatTable$addRows(covData)
    }
  }
}

.linregRobustCreateCollinearityDiagnosticsTable <- function(modelContainer, model, options, position) {
  collDiagTable <- createJaspTable(gettext("Collinearity Diagnostics"))
  collDiagTable$dependOn("collinearityDiagnostic")
  collDiagTable$position <- position

  collDiagTable$addColumnInfo(name = "model",      title = gettext("Model"),           type = "string", combine = TRUE)
  collDiagTable$addColumnInfo(name = "dimension",  title = gettext("Dimension"),       type = "integer")
  collDiagTable$addColumnInfo(name = "eigenvalue", title = gettext("Eigenvalue"),      type = "number")
  collDiagTable$addColumnInfo(name = "condIndex",  title = gettext("Condition Index"), type = "number")

  if (!is.null(model)) {
    .linregRobustAddPredictorsAsColumns(collDiagTable, model, options[["interceptTerm"]], overtitle = gettext("Variance Proportions"), format = "number")
    .linregRobustAddInterceptNotShownFootnote(collDiagTable, model, options)
    .linregRobustFillCollinearityDiagnosticsTable(collDiagTable, model, options)
  }

  modelContainer[["collinearityTable"]] <- collDiagTable
}

.linregRobustFillCollinearityDiagnosticsTable <- function(collDiagTable, model, options) {
  columns <- .linregRobustGetPredictorColumnNames(model, options$modelTerms)
  indicesOfModelsWithPredictors <- .linregRobustGetIndicesOfModelsWithPredictors(model, options)
  for (i in indicesOfModelsWithPredictors) {
    isNewGroup <- i > 1

    collDiagData <- try(.linregRobustGetCollinearityDiagnostics(model[[i]]$fit, columns, options$interceptTerm))
    if (jaspBase::isTryError(collDiagData)) {

      collDiagData <- cbind(.isNewGroup = isNewGroup, model = model[[i]]$title)
      collDiagTable$addFootnote(gettext("Some collinearity diagnostics could not be computed. This is expected if the model contains singularities."))

    } else {
      collDiagData <- cbind(collDiagData, .isNewGroup = c(isNewGroup, rep(F, nrow(collDiagData) - 1)), model = model[[i]]$title)
    }
    collDiagTable$addRows(collDiagData)
  }
}

.linregRobustCreateResidualsTable <- function(modelContainer, finalModel, options, position) {
  residualsTable <- createJaspTable(gettext("Residuals Statistics"))
  residualsTable$dependOn("residualStatistic")
  residualsTable$position <- position

  residualsTable$addColumnInfo(name = "type", title = "",                 type = "string")
  residualsTable$addColumnInfo(name = "min",  title = gettext("Minimum"), type = "number")
  residualsTable$addColumnInfo(name = "max",  title = gettext("Maximum"), type = "number")
  residualsTable$addColumnInfo(name = "mean", title = gettext("Mean"),    type = "number")
  residualsTable$addColumnInfo(name = "SD",   title = gettext("SD"),      type = "number")
  residualsTable$addColumnInfo(name = "N",    title = gettext("N"),       type = "integer")

  if (!is.null(finalModel))
    residualsTable$addRows(.linregRobustGetResidualsStatistics(finalModel$fit, finalModel$predictors))

  modelContainer[["residualsTable"]] <- residualsTable
}

.linregRobustCreateResidualsVsDependentPlot <- function(modelContainer, finalModel, options, position) {
  residVsDepPlot <- createJaspPlot(title = gettext("Residuals vs. Dependent"), width = 530, height = 400)
  residVsDepPlot$dependOn("residualVsDependentPlot")
  residVsDepPlot$position <- position

  modelContainer[["residualsVsDepPlot"]] <- residVsDepPlot

  if (!is.null(finalModel) && !is.null(finalModel$fit)) {
    fit <- finalModel$fit
    .linregRobustInsertPlot(residVsDepPlot, .linregRobustPlotResiduals, xVar = fit$model[ , 1], xlab = options$dependent, res = residuals(fit), ylab = gettext("Residuals"))
  }
}

.linregRobustCreateResidualsVsCovariatesPlots <- function(modelContainer, finalModel, dataset, options, position) {
  residualsVsCovContainer <- createJaspContainer(gettext("Residuals vs. Covariates"))
  residualsVsCovContainer$dependOn("residualVsCovariatePlot")
  residualsVsCovContainer$position <- position
  modelContainer[["residualsVsCovContainer"]] <- residualsVsCovContainer

  if (!is.null(finalModel)) {
    predictors <- finalModel$predictors

    for (predictor in predictors)
      .linregRobustCreatePlotPlaceholder(residualsVsCovContainer, index = .unvf(predictor), title = gettextf("Residuals vs. %s", .unvf(predictor)))

    for (predictor in predictors) {
      if (.linregRobustIsInteraction(predictor) && .linregRobustContainsFactor(finalModel$fit, predictor)) {
        # TODO: this is maybe possible when an interaction consists of only factors, but the plot won't be very pretty
        residualsVsCovContainer[[.unvf(predictor)]]$setError(gettext("Cannot plot residuals versus an interaction with a factor."))
      } else {
        .linregRobustFillResidualsVsCovariatesPlot(residualsVsCovContainer[[.unvf(predictor)]], predictor, finalModel$fit, dataset)
      }
    }
  }
}

.linregRobustFillResidualsVsCovariatesPlot <- function(residVsCovPlot, predictor, fit, dataset) {
  if (.linregRobustIsInteraction(predictor))
    xVar <- .linregRobustMakeCombinedVariableFromInteraction(predictor, dataset)
  else
    xVar <- dataset[[predictor]]

  .linregRobustInsertPlot(residVsCovPlot, .linregRobustPlotResiduals, xVar = xVar, xlab = .unvf(predictor), res = residuals(fit), ylab = gettext("Residuals"))
}

.linregRobustCreateResidualsVsPredictedPlot <- function(modelContainer, finalModel, options, position) {
  residVsPredPlot <- createJaspPlot(title = gettext("Residuals vs. Predicted"), width = 530, height = 400)
  residVsPredPlot$dependOn("residualVsFittedPlot")
  residVsPredPlot$position <- position

  modelContainer[["residualsVsPredPlot"]] <- residVsPredPlot

  if (!is.null(finalModel) && !is.null(finalModel$fit)) {
    fit <- finalModel$fit
    .linregRobustInsertPlot(residVsPredPlot, .linregRobustPlotResiduals, xVar = predict(fit), xlab = gettext("Predicted Values"), res = residuals(fit), ylab = gettext("Residuals"))
  }
}

.linregRobustCreateResidualsVsHistogramPlot <- function(modelContainer, finalModel, options, position) {
  title <- gettext("Residuals Histogram")
  if (options$residualHistogramStandardizedPlot)
    title <- gettextf("Standardized %s", title)

  residVsHistPlot <- createJaspPlot(title = title, width = 530, height = 400)
  residVsHistPlot$dependOn(c("residualHistogramPlot", "residualHistogramStandardizedPlot"))
  residVsHistPlot$position <- position

  modelContainer[["residualsVsHistPlot"]] <- residVsHistPlot

  if (!is.null(finalModel))
    .linregRobustFillResidualsVsHistogramPlot(residVsHistPlot, finalModel$fit, options)
}

.linregRobustFillResidualsVsHistogramPlot <- function(residVsHistPlot, fit, options) {
  if (!is.null(fit)) {

    residName <- gettext("Residuals")
    resid     <- residuals(fit)
    if (options$residualHistogramStandardizedPlot) {
      residName <- gettextf("Standardized %s", residName)
      resid     <- resid / sd(resid)
    }

    .linregRobustInsertPlot(residVsHistPlot, .linregRobustPlotResidualsHistogram, res = resid, resName = residName)
  }
}

.linregRobustCreateResidualsQQPlot <- function(modelContainer, finalModel, options, position) {
  residQQPlot <- createJaspPlot(title = gettext("Q-Q Plot Standardized Residuals"), width = 400, height = 400)
  residQQPlot$dependOn(c("residualQqPlot", "qqPlotCi, qqPlotCiLevel"))
  residQQPlot$position <- position

  modelContainer[["residualsQQPlot"]] <- residQQPlot

  if (!is.null(finalModel) && !is.null(finalModel$fit)) {
    fit <- finalModel$fit

    .linregRobustInsertPlot(residQQPlot, .linregRobustFillPlotResQQ, model = fit,
                      residType = "deviance", options = options)
  }
}

# Helper function to add pre- and suffix to a string. Identical function from correlation.R
.linregRobustAppendLabel <- function(condition = FALSE, label, prefix = "", suffix = ""){
  if(!condition) return(label)
  if(length(prefix)>1 || length(suffix)>1) return()
  if (length(label)>1){
    # Recursive vectorization if list of variable names so it labels each variable.
    return(lapply(label, .linregRobustAppendLabel, condition = condition, prefix = prefix, suffix = suffix))
  }
  return(paste0(prefix, label, suffix))
}

.linregRobustCreatePartialPlots <- function(modelContainer, dataset, options, position) {
  predictors <- .linregRobustGetPredictors(options$modelTerms)

  title <- ngettext(length(options$covariates) + length(options$factors), "Regression Plot", "Partial Regression Plot")

  partialPlotContainer <- createJaspContainer(title)
  partialPlotContainer$dependOn(c("partialResidualPlot", "partialResidualPlotCi", "partialResidualPlotCiLevel",
                                  "partialResidualPlotPredictionInterval", "partialResidualPlotPredictionIntervalLevel"))
  partialPlotContainer$position <- position
  modelContainer[["partialPlotContainer"]] <- partialPlotContainer

  predictors <- .linregRobustGetPredictors(options$modelTerms[[length(options$modelTerms)]][["components"]])
  if (any(.linregRobustIsInteraction(predictors))) {
    .linregRobustCreatePlotPlaceholder(partialPlotContainer, index = "placeholder", title = "")
    partialPlotContainer$setError(gettext("Partial plots are not supported for models containing interaction terms"))
    return()
  }

  # Title adjustment depending on single or multiple predictors
  if (options$dependent != "" && length(predictors) > 0) {
    for (predictor in predictors) {

      # Base case of single predictor partial plot title
      titleString <- gettextf("%1$s vs. %2$s", options$dependent, .unvf(predictor))

      # Adjust title if variables need to be controlled for partial plot
      if (length(predictors) > 1) {
        controlled <- predictors[predictors != predictor]
        titleSuffix <-  paste(" (", paste(controlled, collapse = ", "), " <i>partialed out</i> )" )
        titleString <- .linregRobustAppendLabel(
          condition = TRUE,
          label = titleString,
          prefix = "",
          suffix = titleSuffix
        )
      }

      # Create plot placeholder with appropriate title
      .linregRobustCreatePlotPlaceholder(partialPlotContainer, index = .unvf(predictor), title = titleString)
    }

    # Error message for factor predictors
    for (predictor in predictors) {
      if (.linregRobustContainsFactor(dataset, predictor)) {
        partialPlotContainer[[.unvf(predictor)]]$setError(gettext("Partial plots are not supported for factors"))
      } else {
        .linregRobustFillPartialPlot(partialPlotContainer[[.unvf(predictor)]], predictor, predictors, dataset, options)
      }
    }
  }
}

.linregRobustFillPartialPlot <- function(partialPlot, predictor, predictors, dataset, options) {

  plotData  <- .linregRobustGetPartialPlotData(predictor, predictors, dataset, options)
  xVar      <- plotData[["residualsPred"]]
  resid     <- plotData[["residualsDep"]]
  dfResid   <- length(resid) - length(predictors) - 1

  xlab      <- gettextf("Residuals %s", .unvf(predictor))
  ylab      <- gettextf("Residuals %s", options$dependent)

  # Compute regresion lines
  weights <- dataset[[options$weights]]
  line <- as.list(setNames(lm(residualsDep~residualsPred, data = plotData, weights = weights)$coeff,
                           c("intercept", "slope"))
                  )

  .linregRobustInsertPlot(partialPlot, .linregRobustPlotResiduals, xVar = xVar, res = resid, dfRes = dfResid, xlab = xlab, ylab = ylab,
                    regressionLine = TRUE, confidenceIntervals = options$partialResidualPlotCi,
                    confidenceIntervalsInterval = options$partialResidualPlotCiLevel,
                    predictionIntervals = options$partialResidualPlotPredictionInterval,
                    predictionIntervalsInterval = options$partialResidualPlotPredictionIntervalLevel,
                    standardizedResiduals = FALSE, intercept = line[['intercept']], slope = line[['slope']])
}

.linregRobustCreateDescriptivesTable <- function(modelContainer, dataset, options, position) {
  descriptivesTable <- createJaspTable(gettext("Descriptives"))
  descriptivesTable$dependOn("descriptives")
  descriptivesTable$position <- position

  descriptivesTable$addColumnInfo(name = "var",  title = "",              type = "string")
  descriptivesTable$addColumnInfo(name = "N",    title = gettext("N"),    type = "integer")
  descriptivesTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
  descriptivesTable$addColumnInfo(name = "SD",   title = gettext("SD"),   type = "number")
  descriptivesTable$addColumnInfo(name = "SE",   title = gettext("SE"),   type = "number")

  variables <- c(options$dependent, unlist(options$covariates))
  variables <- variables[variables != ""]
  if (length(variables) > 0)
    descriptivesTable$addRows(.linregRobustGetDescriptives(variables, dataset))

  modelContainer[["descriptivesTable"]] <- descriptivesTable
}

.linregRobustCalcModel <- function(modelContainer, dataset, options, ready) {
  if (!ready)
    return()

  if (!is.null(modelContainer[["model"]])) {
    model <- modelContainer[["model"]]$object
    model <- .linregRobustCalcDurBinWatsonTestResults(modelContainer, model, options)
    return(model)
  }
  nModels           <- length(options$modelTerms)
  dependent         <- options$dependent

  if (options$weights != "")
    weights <- dataset[[options$weights]]
  else
    weights <- rep(1, length(dataset[[dependent]]))

  model <- .linregRobustGetModelEnterMethod(dependent, modelTerms = options[["modelTerms"]], dataset, options, weights)

  for (i in seq_along(model)) {
    singleModel <- model[[i]]
    modNum <- singleModel[["number"]]
    model[[i]][["title"]]         <-  gettextf("M%s", intToUtf8(0x2080 + modNum - 1, multiple = FALSE)) # singleModel[["title"]]
    model[[i]][["summary"]]       <- .linregRobustGetSummary(singleModel$fit)
    model[[i]][["rSquareChange"]] <- .linregRobustGetrSquaredChange(singleModel$fit, i, model[1:i], options)
  }

  modelContainer[["model"]] <- createJaspState(model)

  model <- .linregRobustCalcDurBinWatsonTestResults(modelContainer, model, options)

  return(model)
}

.linregRobustCalcDurBinWatsonTestResults <- function(modelContainer, model, options) {

  if (!options[["residualDurbinWatson"]])
    return(model)

  durbinWatsonResults <- modelContainer[["durbinWatsonResults"]] %setOrRetrieve% (
    lapply(model, function(singleModel) {
      .linregRobustGetDurBinWatsonTestResults(singleModel$fit, options$weights)
    }) |> createJaspState()
  )

  for (i in seq_along(model))
    model[[i]][["durbinWatson"]] <- durbinWatsonResults[[i]]

  return(model)
}

.linregRobustGetModelEnterMethod <- function(dependent, modelTerms, dataset, options, weights) {
  model <- list()
  isRobust <- .linregRobustIsRobust(options)

  for (i in seq_along(modelTerms)) {
    thisModelTerms <- .linregRobustGetPredictors(modelTerms[[i]][["components"]])
    formula <- .linregRobustGetFormula(dependent, thisModelTerms, options$interceptTerm, options$covariates, options$quadraticTerms)
    if (!is.null(formula)) {
      if (isRobust) {
        psiFunc <- if (options$estimationMethod == "huber") MASS::psi.huber else MASS::psi.bisquare
        fit <- MASS::rlm(formula, data = dataset, weights = weights, psi = psiFunc, maxit = 200)
        fit$x <- model.matrix(fit)
      } else {
        fit <- stats::lm(formula, data = dataset, weights = weights, x = TRUE)
      }
      model[[length(model) + 1]] <- list(fit = fit, predictors = thisModelTerms, number = i)
    }
  }

  return(model)
  }

.linregRobustGetSummary <- function(fit) {
  summary <- list(r.squared = NaN, r.squared = NaN, adj.r.squared = NaN, sigma = NaN)

  if (!is.null(fit)) {
    summary <- summary(fit)

    if (inherits(fit, "rlm")) {
      # rlm summary has r.squared = NA; compute pseudo-R^2
      y <- fitted(fit) + residuals(fit)
      ss_resid <- sum(residuals(fit)^2)
      ss_total <- sum((y - mean(y))^2)
      n <- length(y)
      p <- length(coef(fit))
      r2 <- if (ss_total > 0) 1 - ss_resid / ss_total else NaN
      summary$r.squared     <- r2
      summary$adj.r.squared <- 1 - (1 - r2) * (n - 1) / (n - p)
    }
  }

  return(summary)
}

.linregRobustGetDurBinWatsonTestResults <- function(fit, weights) {
  durbinWatson <- list(r = NaN, dw = NaN, p = NaN)

  if (!is.null(fit)) {
    # TODO: Make some nicer error messsage/footnote when durbin watson computation fails
    durbinWatson <- try(.durbinWatsonTest.lm(fit, alternative = c("two.sided")))

    if (jaspBase::isTryError(durbinWatson)) {
      return(list(r = NaN, dw = NaN, p = NaN))
    }

    if (weights == "") # if regression is not weighted, calculate p-value with lmtest (car method is unstable)
      # TODO: this can fail when there are many interactions between factors. Do we want to show a footnote about that?
      durbinWatson[["p"]] <- tryCatch(
        lmtest::dwtest(fit, alternative = c("two.sided"))$p.value,
        error = function(e) NaN
      )
  }

  return(durbinWatson)
}

.linregRobustGetrSquaredChange <- function(fit, currentIndex, processedModels, options) {
  #R^2_change in Field (2013), Eqn. 8.15:
  #F.change = (n-p_new - 1)R^2_change / p_change ( 1- R^2_new)
  #df1 = p_change = abs( p_new - p_old )
  #df2 = n-p_new
  # the above works only for continuous predictors, for categorical, we have (k-1) coefficients
  # where k=number of the levels in the categorical variable

  if (currentIndex == 1) {
    # if we include the intercept, the number of coefficients to compare the null model to is 1
    # otherwise there are no coefficients
    prevCoefs    <- if(options[["interceptTerm"]]) numeric(1) else numeric(0)
    prevRSquared <- 0
  } else {
    prevCoefs    <- stats::coefficients(processedModels[[currentIndex - 1]]$fit)
    prevRSquared <- .linregRobustGetSummary(processedModels[[currentIndex - 1]]$fit)$r.squared
  }

  rSquaredChange <- fChange <- df1 <- df2 <- p <- NaN
  if (!is.null(fit)) {
    rSquared        <- .linregRobustGetSummary(fit)$r.squared
    rSquaredChange  <- rSquared - prevRSquared

    coefs <- stats::coefficients(fit)

    df1 <- abs(length(coefs) - length(prevCoefs)) # df1 = p_change = abs( p_new - p_old )
    df2 <- stats::df.residual(fit) # df2 = n-p_new but should take factors into account

    if (df1 > 0L) {
      fChange <- (df2 * rSquaredChange) / (df1 * (1 - rSquared))
      p       <- pf(q = fChange, df1 = df1, df2 = df2, lower.tail = FALSE)
    } else {
      fChange <- p <- NA
    }
  }

  rSquareChange <- list(
    R2c = rSquaredChange,
    Fc  = fChange,
    df1 = df1,
    df2 = df2,
    p   = p
  )

  return(rSquareChange)
}

.linregRobustGetAnova <- function(fit, predictors) {
  Fvalue <- mssResidual <- mssModel <- dfResidual <- dfModel <- dfTotal <- ssResidual <- ssModel <- ssTotal <- p <- vovksellke <- NaN

  if (!is.null(fit)) {
    if (length(predictors) > 0) {
      summary     <- summary(fit)

      if (inherits(fit, "rlm")) {
        # For rlm: compute Wald-type F test
        y <- fitted(fit) + residuals(fit)
        ssResidual <- sum(residuals(fit)^2)
        ssTotal    <- sum((y - mean(y))^2)
        ssModel    <- ssTotal - ssResidual

        dfModel    <- length(coef(fit)) - 1L
        dfResidual <- length(y) - length(coef(fit))
        dfTotal    <- dfResidual + dfModel

        mssModel    <- if (dfModel > 0) ssModel / dfModel else NaN
        mssResidual <- if (dfResidual > 0) ssResidual / dfResidual else NaN
        Fvalue      <- if (mssResidual > 0) mssModel / mssResidual else NaN
        p           <- pf(q = Fvalue, df1 = dfModel, df2 = dfResidual, lower.tail = FALSE)
        vovksellke  <- VovkSellkeMPR(p)
      } else {
        Fvalue			<- summary$fstatistic[1]
        mssResidual	<- summary$sigma^2
        mssModel	  <- Fvalue * mssResidual
        dfResidual	<- summary$fstatistic[3]
        dfModel		  <- summary$fstatistic[2]
        dfTotal		  <- dfResidual + dfModel
        ssResidual  <- mssResidual * dfResidual
        ssModel		  <- mssModel * dfModel
        ssTotal		  <- ssResidual + ssModel

        p           <- pf(q = Fvalue, df1 = dfModel, df2 = dfResidual, lower.tail = FALSE)
        vovksellke  <- VovkSellkeMPR(p)
      }
    } else {
      Fvalue <- mssResidual <- mssModel <- dfResidual <- dfModel <- dfTotal <- ssResidual <- ssModel <- ssTotal <- p <- vovksellke <- "."
    }
  }

  anova <- list(
    Regression  = list(F = Fvalue,  SS = ssModel,     df = dfModel,     MS = mssModel,  p = p, vovksellke = vovksellke),
    Residual    = list(             SS = ssResidual,  df = dfResidual,  MS = mssResidual),
    Total       = list(             SS = ssTotal,     df = dfTotal)
  )

  return(anova)
}

.linregRobustGetCoefficients <- function(fit, predictors, dataset, options) {
  rows <- list()
  footnote <- NULL

  if (!is.null(fit)) {
    if (options$interceptTerm)
      predictors <- c("(Intercept)", predictors)

    factors <- options[["factors"]]
    hasFactors <- length(factors) > 0L

    summ          <- summary(fit)
    missingCoeffs <- if (!is.null(summ[["aliased"]])) summ[["aliased"]] else setNames(rep(FALSE, length(coef(fit))), names(coef(fit)))

    # Normalize coefficient matrix to always have 4 columns: Estimate, Std. Error, t value, Pr(>|t|)
    rawCoefs <- coef(summ)
    if (inherits(fit, "rlm")) {
      # rlm summary has columns: Value, Std. Error, t value (no p-value)
      # Compute p-values using t-distribution with residual df
      dfResid <- df.residual(fit)
      pvals <- 2 * pt(-abs(rawCoefs[, "t value"]), df = dfResid)
      rawCoefs <- cbind(rawCoefs, "Pr(>|t|)" = pvals)
      colnames(rawCoefs)[1] <- "Estimate"
    }

    # adapted from stats:::print.summary.lm -- automatically handles missing values
    estimates <- matrix(NaN, length(missingCoeffs), 4, dimnames = list(names(missingCoeffs), colnames(rawCoefs)))
    estimates[!missingCoeffs, ] <- rawCoefs
    confInterval  <- confint(fit, level = options$coefficientCiLevel)
    confInterval[is.na(confInterval)] <- NaN

    info <- .linregRobustGetParametersAndLevels(fit)
    names <- .linregRobustMakePrettyNames(info)
    rawNames <- info[["paramsRaw"]]
    names[c(FALSE, names[-1L] == names[-length(names)])] <- ""

    # show footnote for missing coefficients
    footnote <- NULL
    if (any(missingCoeffs))
      footnote <- list(rows = names[missingCoeffs])


    rows <- vector("list", nrow(estimates))

    collinearityDiagnostics <- .linregRobustGetVIFAndTolerance(fit)
    dataClasses <- attr(terms(fit), "dataClasses")

    # counts if we're showing a second level of a factor/ interaction
    # for example for collinearity diagnostics
    factorsSeen <- hashtab()
    rowIndex <- 1L

    for (i in seq_along(names)) {

      hasFactors <- any(dataClasses[rawNames[[i]]] == "factor")
      unstandCoeff <- estimates[i, "Estimate"]
      predictor <- paste(rawNames[[i]], collapse = ":")
      # these don't exist for the intercept or categorical predictors.
      standCoeff <- NULL
      if (!identical(rawNames[[i]], "(Intercept)") && !hasFactors)
        standCoeff <- .linregRobustGetStandardizedCoefficient(dataset, options[["dependent"]], predictor, unstandCoeff)

      # tolerance/ VIF is only shown for the first level of categorical variables/ interactions
      tolerance <- VIF <- NULL
      if (!is.null(collinearityDiagnostics) && !identical(predictor, "(Intercept)") && (!hasFactors || !factorsSeen[[rawNames[[i]], nomatch = FALSE]])) {

        tolerance  <- collinearityDiagnostics[["tolerance"]][[predictor]]
        VIF        <- collinearityDiagnostics[["VIF"]][[predictor]]

        if (hasFactors)
          factorsSeen[[rawNames[[i]]]] <- TRUE

      }

      # get translation for (Intercept)
      name <- names[i]
      if (identical(name, "(Intercept)"))
        name <- gettext("(Intercept)")

      row <- list(
        name         = name,
        unstandCoeff = unstandCoeff,
        SE           = estimates[i, "Std. Error"],
        t            = estimates[i, "t value"],
        p            = estimates[i, "Pr(>|t|)"],
        lower        = confInterval[i, 1],
        upper        = confInterval[i, 2],
        standCoeff   = standCoeff,
        tolerance    = tolerance,
        VIF          = VIF,
        vovksellke   = VovkSellkeMPR(estimates[i, "Pr(>|t|)"])
      )

      rows[[i]] <- row

    }

  }

  return(list(coefficients = rows, footnote = footnote))
}

.linregRobustGetBootstrapCoefficients <- function(fit, dataset, options) {
  isRobust <- inherits(fit, "rlm")
  psiFunc  <- if (isRobust) fit$psi else NULL

  .bootstrapping <- function(data, indices, formula, wlsWeights, useRlm, psi) {
    progressbarTick()

    d <- data[indices, , drop = FALSE] # allows boot to select sample
    if (useRlm) {
      if (wlsWeights == "") {
        bfit <- MASS::rlm(formula = formula, data = d, psi = psi, maxit = 200)
      } else {
        weights <- d[[wlsWeights]]
        bfit <- MASS::rlm(formula = formula, data = d, weights = weights, psi = psi, maxit = 200)
      }
    } else {
      if (wlsWeights == "") {
        bfit <- lm(formula = formula, data = d)
      } else {
        weights <- d[[wlsWeights]]
        bfit <- lm(formula = formula, data = d, weights = weights)
      }
    }

    return(coef(bfit))
  }

  data <- data.frame(unstandCoeff = numeric(0), bias = numeric(0), SE = numeric(0), pvalue = numeric(0), lower = numeric(0), upper = numeric(0))

  if (!is.null(fit)) {

    #weights <- if (options$weights == "") NULL else options$weights
    missingCoeffs <- NULL
    coefNames <- names(coef(fit))
    if (anyNA(fit$coefficients))
      missingCoeffs <- coefNames[which(is.na(coef(fit)))]

    summary <- boot::boot(data = dataset, statistic = .bootstrapping,
                          R = options$coefficientBootstrapSamples,
                          formula = formula(fit),
                          wlsWeights = options$weights,
                          useRlm = isRobust,
                          psi = psiFunc)

    coefficients  <- matrixStats::colMedians(summary$t, na.rm = TRUE)
    bias          <- colMeans(summary$t, na.rm = TRUE) - summary$t0
    stdErrors     <- matrixStats::colSds(summary$t, na.rm = TRUE)

    for (i in seq_along(coefNames)) {
      coefName <- coefNames[[i]]

      if (coefName %in% missingCoeffs) {
        data[i, ] <- rep(NaN, ncol(data))
        next
      }

      ci <- try(boot::boot.ci(summary, type = "bca", conf = options$coefficientCiLevel, index = i))
      if (jaspBase::isTryError(ci))
        ci <- list(bca = rep(NaN, 5L))

      p <- try(.boot.pval(summary, type = "bca", index = i))
      if (isTryError(p))
        p <- NaN

      data[i, ] <- c(coefficients[i], bias[i], stdErrors[i], p, ci$bca[4], ci$bca[5])
    }

    data[["name"]] <- .linregRobustMakePrettyNames(fit)
  }

  return(data)
}

.linregRobustGetVIFAndTolerance <- function(fit) {

  # also used inside car:::vif.default
  noTerms <- length(labels(stats::terms(fit)))
  if (noTerms == 0L) # nothing can be computed
    return(NULL)
  if (noTerms == 1L) {# trivial case, always 1
    name  <- labels(stats::terms(fit))
    value <- setNames(1, name)
    return(list(VIF = value, tolerance = value))
  }

  # we can actually compute things
  result <- try(car::vif(fit))

  if (jaspBase::isTryError(result)) {
    nas <- rep(NA, noTerms)
    names(nas) <- labels(terms(fit))
    result <- list(VIF = nas, tolerance = nas)
    return(result)
  }

  VIF <- if (is.matrix(result)) {
    result[, 3L]
  } else {
    result
  }
  tolerance <- 1 / VIF

  result <- list(VIF       = VIF,
                 tolerance = tolerance)

  return(result)
}

.linregRobustGetStandardizedCoefficient <- function(dataset, dependent, predictor, unstandCoeff) {
  sdDependent <- sd(dataset[[dependent]])

  if (.linregRobustIsInteraction(predictor))
    sdIndependent <- sd(.linregRobustMakeCombinedVariableFromInteraction(predictor, dataset))
  # else if (grepl(pattern = "I\\(([^)]+)\\^2\\)", predictor))
  #   # if quadratic term, calculate sd of squared variable
  #   # this is probably not conventional so for now we omit this calculation and don't present std b's
  #   sdIndependent <- sd(dataset[[gsub("\u00B2", "", .linregRobustPrettyQuadraticName(predictor))]]^2)
  else
    sdIndependent <- sd(dataset[[predictor]])

  return(unstandCoeff * sdIndependent / sdDependent)
}

.linregRobustGetPartAndPartialCorrelation <- function(fit, predictors, dataset, options) {
  formula <- formula(fit)
  R2      <- .linregRobustGetSummary(fit)[["r.squared"]]

  cors <- vector("list", length(predictors))
  names(cors) <- predictors

  for(predictor in predictors) {

    # drop the term from the formula and refit the model
    newFormula <- update(formula, as.formula(sprintf(". ~ . - %s", predictor)))
    data <- dataset
    newFit     <- update(fit, formula = newFormula)
    newR2      <- .linregRobustGetSummary(newFit)[["r.squared"]]

    sr2 <- R2 - newR2      # squared semi-partial (part) correlation
    pr2 <- sr2 / (1-newR2) # squared partial correlation

    # determine the sign of the coefficient
    sign <- sign(coefficients(fit)[predictor])
    if(is.na(sign)) sign <- 1 # for categorical predictors

    cors[[predictor]] <- list(
      name    = jaspBase::gsubInteractionSymbol(predictor),
      part    = sign * sqrt(sr2),
      partial = sign * sqrt(pr2)
    )
  }

  return(cors)
}

.linregRobustGetCovarianceMatrix <- function(fit, rawNames, prettyNames) {
  data <- data.frame()

  if (!is.null(fit)) {

    namesInModel <- setdiff(names(coef(fit)), "(Intercept)")
    covmatrix <- matrix(NA_real_, length(namesInModel), length(rawNames), dimnames = list(namesInModel, rawNames))
    covmatrix[namesInModel, namesInModel] <- vcov(fit)[namesInModel, namesInModel]
    covmatrix[lower.tri(covmatrix)] <- NA

    if (nrow(covmatrix) > 0L) {
      colnames(covmatrix) <- prettyNames
      names <- prettyNames[rawNames %in% namesInModel]
      data <- cbind(data.frame(name = names), covmatrix)
    }
  }

  return(data)
}

.linregRobustGetCollinearityDiagnostics <- function(fit, columns, includeConstant) {
  data <- data.frame()

  if (!is.null(fit)) {
    eigenvalues         <- .linregRobustGetEigenValues(fit)
    conditionIndices    <- .linregRobustGetConditionIndices(fit)
    varianceProportions <- .linregRobustGetVarianceProportions(fit)

    data <- data.frame(dimension = seq_along(names(fit$coefficients)), eigenvalue = eigenvalues, condIndex = conditionIndices)
    colnames(varianceProportions) <- .linregRobustMakePrettyNames(fit)
    data <- cbind(data, varianceProportions)
  }

  return(data)
}

.linregRobustGetEigenValues <- function(fit) {
  X           <- .linregRobustGetScaledPredictorMatrix(fit)
  eigenvalues <- svd(X)$d^2 # see Liao & Valliant (2012)

  return(eigenvalues)
}

.linregRobustGetConditionIndices <- function(fit) {
  eigenvalues       <- .linregRobustGetEigenValues(fit)
  conditionIndices  <- sqrt(max(eigenvalues) / eigenvalues)

  return(conditionIndices)
}

.linregRobustGetVarianceProportions <- function(fit) {
  X <- .linregRobustGetScaledPredictorMatrix(fit)

  ### ( see e.g., Liao & Valliant, 2012 )
  svdX  <- svd(X) # singular value decomposition
  M     <- svdX$v %*% solve(diag(svdX$d))
  Q     <- M*M # Hadamard (elementwise) product
  tQ    <- t(Q)

  for (i in seq_len(ncol(tQ)))
    tQ[ , i] <- tQ[ , i] / sum(tQ[ , i])

  colnames(tQ) <- names(fit$coefficients)

  return(tQ)
}

.linregRobustGetScaledPredictorMatrix <- function(fit) {
  X <- fit$x

  for (i in seq_len(ncol(X)))
    X[ , i] <- X[ , i] / sqrt(sum(X[ , i]^2)) # scale each column using Euclidean norm

  return(X)
}

.linregRobustGetCasewiseDiagnostics <- function(fit, options) {
  diagnostics <- list()

  if (!is.null(fit)) {
    predictedValuesAll    <- predict(fit)
    residualsAll          <- residuals(fit)
    stdPredictedValuesAll <- (predictedValuesAll - mean(predictedValuesAll)) / sd(predictedValuesAll)
    stdResidualsAll       <- rstandard(fit)
    # stdResidualsAll       <-  statmod::qresid(fit)
    # stdResidualsAll       <- rstudent(fit
    cooksDAll             <- cooks.distance(fit)

    if (options$residualCasewiseDiagnosticType == "cooksDistance")
      index <- which(abs(cooksDAll) > options$residualCasewiseDiagnosticCooksDistanceThreshold)
    else if (options$residualCasewiseDiagnosticType == "outliersOutside")
      index <- which(abs(stdResidualsAll) > options$residualCasewiseDiagnosticZThreshold)
    else # all
      index <- seq_along(predictedValuesAll)

    if (length(index) > 0) {
      caseNumbers <- as.numeric(rownames(model.frame(fit)))
      diagnostics[["caseNumber"]]   <- caseNumbers[index]
      diagnostics[["stdResidual"]]  <- stdResidualsAll[index]
      diagnostics[["dependent"]]    <- fit$model[index, 1]
      diagnostics[["predicted"]]    <- predictedValuesAll[index]
      diagnostics[["residual"]]     <- residualsAll[index]
      diagnostics[["cooksD"]]       <- cooksDAll[index]
    }
  }

  return(diagnostics)
}

.linregRobustGetResidualsStatistics <- function(fit, predictors) {
  residuals <- list()

  if (!is.null(fit)) {
    typesTranslated <- list("Predicted Value"=gettext("Predicted Value"), "Residual"=gettext("Residual"), "Std. Predicted Value"=gettext("Std. Predicted Value"), "Std. Residual"=gettext("Std. Residual"))
    types           <- names(typesTranslated)

    predicted     <- predict(fit)
    N             <- length(predicted)
    valuesPerType <- list("Predicted Value"       = predicted,
                          "Residual"              = residuals(fit),
                          "Std. Predicted Value"  = (predicted - mean(predicted)) / sd(predicted),
                          "Std. Residual"         = rstandard(fit))

    if (length(predictors) == 0)
      valuesPerType[["Std. Predicted Value"]] <- NA # cannot compute this for an intercept model

    residuals <- vector("list", length(types))
    for (i in seq_along(types)) {
      residuals[[i]]  <- list()
      type            <- types[i]

      residuals[[i]][["type"]] <- typesTranslated[[type]]
      residuals[[i]][["min"]]  <- min( valuesPerType[[type]], na.rm = TRUE)
      residuals[[i]][["max"]]  <- max( valuesPerType[[type]], na.rm = TRUE)
      residuals[[i]][["mean"]] <- mean(valuesPerType[[type]], na.rm = TRUE)
      residuals[[i]][["SD"]]   <- sd(  valuesPerType[[type]], na.rm = TRUE)
      residuals[[i]][["N"]]    <- N
    }
  }

  return(residuals)
}

.linregRobustGetDescriptives <- function(variables, dataset) {
  descriptives <- vector("list", length(variables))

  for (i in seq_along(variables)) {
    descriptives[[i]] <- list()

    variable  <- variables[i]
    data      <- na.omit(dataset[[variable]])

    descriptives[[i]][["var"]]  <- variable
    descriptives[[i]][["N"]]    <- length(data)
    descriptives[[i]][["mean"]] <- mean(data)
    descriptives[[i]][["SD"]]   <- sd(data)
    descriptives[[i]][["SE"]]   <- sd(data) / sqrt(length(data))
  }

  return(descriptives)
}

.linregRobustGetPartialPlotData = function(predictor, predictors, dataset, options) {
  predictors <- setdiff(predictors, predictor)
  if (length(predictors) == 0)
    predictors <- NULL

  weights <- dataset[[options$weights]]

  # Compute residuals dependent
  formulaDep    <- .linregRobustGetFormula(options$dependent, predictors = predictors, includeConstant = TRUE)
  fitDep        <- stats::lm(formula = formulaDep, data = dataset, weights = weights)
  residualsDep  <- residuals(fitDep)

  # Compute residuals predictor as dependent
  formulaPred   <- .linregRobustGetFormula(predictor, predictors = predictors, includeConstant = TRUE)
  fitPred       <- stats::lm(formula = formulaPred, data = dataset, weights = weights)
  residualsPred <- residuals(fitPred)

  return(data.frame(residualsPred = residualsPred, residualsDep = residualsDep))
}

.linregRobustPlotResiduals <- function(xVar = NULL, res = NULL, dfRes = Inf, xlab, ylab = gettext("Residuals"), cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2, lwdAxis=1.2,
                                 regressionLine = TRUE, confidenceIntervals = FALSE, confidenceIntervalsInterval = 0.95, predictionIntervals = FALSE, predictionIntervalsInterval = 0.95,
                                 standardizedResiduals = TRUE, intercept = 0, slope = 0) {

  # TODO: slope should consist of multiple values for factors with more than 2 levels
  d     <- data.frame(xx= xVar, yy= res)
  d     <- na.omit(d)
  xVar  <- d$xx
  res   <- d$yy

  # construct here the x-axis scale which can be categorical or continuous
  if (is.factor(xVar)) {
    xScale <- ggplot2::scale_x_discrete(name = xlab)
  } else {
    xlow   <- min(pretty(xVar))
    xhigh  <- max(pretty(xVar))
    xticks <- pretty(c(xlow, xhigh))
    xlabs  <- jaspGraphs::axesLabeller(xticks, digits = 3)
    xScale <- ggplot2::scale_x_continuous(name = xlab, breaks = xticks, labels = xlabs)
  }

  # y-axis scale is always continuous (since the dependent variable in linear regression should be continuous)
  ylow   <- min(pretty(res))
  yhigh  <- max(pretty(res))
  yticks <- pretty(c(ylow, yhigh, 0))
  ylabs  <- jaspGraphs::axesLabeller(yticks, digits = 3)
  ylim   <- range(yticks)

  if (standardizedResiduals) {

    stAxisTmp               <- pretty( yticks / sd(res) )
    stAxisOriginalScaleTmp  <- stAxisTmp * sd(res)
    stAxisOriginalScale     <- stAxisOriginalScaleTmp[stAxisOriginalScaleTmp < max(yticks) & stAxisOriginalScaleTmp > min(yticks)]
    stAxis                  <- stAxisOriginalScale / sd(res)

    yScaleSecAxis <- ggplot2::sec_axis(~.+0, breaks = stAxisOriginalScale, name = gettext("Standardized Residuals\n"),labels = stAxis)

  } else {
    yScaleSecAxis <- ggplot2::waiver()
  }

  yScale <- ggplot2::scale_y_continuous(name = ylab, breaks = yticks, labels = ylabs, limits = ylim, sec.axis = yScaleSecAxis)

  regLine <- confidenceIntervalLines <- predictionIntervalLines <- NULL
  if (regressionLine) {

    regLine <- if (is.factor(xVar)) {
      ggplot2::geom_line(
        data = data.frame(x = as.numeric(unique(xVar)), y = intercept + slope),
        mapping = ggplot2::aes(x = x, y = y),
        col = "darkred", size = .5
      )
    } else {
      ggplot2::geom_line(
        data = data.frame(x = c(min(xticks), max(xticks)), y = intercept + slope * c(min(xticks), max(xticks))),
        mapping = ggplot2::aes(x = x, y = y),
        col = "darkred", size = .5
      )
    }

    if (confidenceIntervals) {

      seConf <- sqrt(sum(res^2) / dfRes) *
        sqrt(1 / length(res) + (xVar - mean(xVar))^2 / sum((xVar - mean(xVar))^2))

      ciConf <- 1 - (1 - confidenceIntervalsInterval)/2

      upperConfInt <- (intercept + slope * xVar) + qt(ciConf, dfRes) * seConf
      lowerConfInt <- (intercept + slope * xVar) - qt(ciConf, dfRes) * seConf

      # ggplot2::geom_errorbar()

      confidenceIntervalLines <- if (is.factor(xVar)) {
        ggplot2::geom_errorbar(
          data = data.frame(x = levels(xVar), ymax = upperConfInt, ymin = lowerConfInt),
          mapping = ggplot2::aes(x = x, ymax = ymax, ymin = ymin),
          colour = "darkblue", linetype = "dashed", size = 1
        )
      } else {
        ggplot2::geom_line(
          data = data.frame(x = xVar, y = c(upperConfInt, lowerConfInt), g = rep(1:2, c(length(upperConfInt), length(lowerConfInt)))),
          mapping = ggplot2::aes(x = x, y = y, group = g),
          colour = "darkblue", linetype = "dashed", size = 1
        )
      }

    }

    if (predictionIntervals) {

      sePred <- sqrt(sum(res^2) / dfRes) *
        sqrt(1 + 1 / length(res) + (xVar - mean(xVar))^2 / sum((xVar - mean(xVar))^2))

      ciPred <- 1 - (1 - predictionIntervalsInterval)/2

      upperPredInt <- (intercept + slope * xVar) + qt(ciPred, dfRes) * sePred
      lowerPredInt <- (intercept + slope * xVar) - qt(ciPred, dfRes) * sePred

      predictionIntervalLines <- if (is.factor(xVar)) {
        ggplot2::geom_errorbar(
          data = data.frame(x = levels(xVar), ymax = upperPredInt, ymin = lowerPredInt),
          mapping = ggplot2::aes(x = x, ymax = ymax, ymin = ymin),
          colour = "darkgreen", linetype = "longdash"
        )
      } else {
        ggplot2::geom_line(data = data.frame(x = xVar, y = c(upperPredInt, lowerPredInt), g = rep(1:2, c(length(upperPredInt), length(lowerPredInt)))),
                           mapping = ggplot2::aes(x = x, y = y, group = g),
                           col = "darkgreen", linetype = "dashed", size = 1)
      }
    }


  }

  residualPoints <- jaspGraphs::geom_point(data = data.frame(x = xVar, y = res), mapping = ggplot2::aes(x = x, y = y))

  p <- ggplot2::ggplot() +
    xScale + yScale +
    regLine +
    confidenceIntervalLines +
    predictionIntervalLines +
    residualPoints +
    jaspGraphs::geom_rangeframe(sides = if (standardizedResiduals) "blr" else "bl") +
    jaspGraphs::themeJaspRaw(axis.title.cex = 1.2)

  return(p)
}

.linregRobustPlotResidualsHistogram <- function(res = NULL, resName = gettext("Residuals"), cexYlab= 1.3, lwd= 2, rugs= FALSE) {
  density <- density(res)

  h       <- hist(res, plot = FALSE)
  dens    <- density(res)
  yhigh   <- max(c(h$density, dens$y))
  ylow    <- 0
  xticks  <- base::pretty(c(res, h$breaks), min.n= 3)

  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(name = resName,            breaks = xticks,         labels = xticks, limits = range(xticks)) +
    ggplot2::scale_y_continuous(name = gettext("Density"), breaks = c(ylow, yhigh), labels = NULL) +
    ggplot2::geom_histogram(
      data = data.frame(res),
      mapping = ggplot2::aes(x = res, y = ..density..),
      binwidth = (h$breaks[2] - h$breaks[1]),
      fill = "grey", col = "black", size = .3,
      center = ((h$breaks[2] - h$breaks[1])/2)
    ) +
    ggplot2::geom_line(data = data.frame(x = density$x, y = density$y), mapping = ggplot2::aes(x = x, y = y), lwd = .7, col = "black") +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(axis.title.cex = 1.2) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  return(p)
}

.linregRobustGetPredictors <- function(modelTerms) {

  predictors <- NULL
  for (i in seq_along(modelTerms)) {
    components <- modelTerms[[i]]
    predictor <- paste0(components, collapse = ":")


    predictors <- c(predictors, predictor)

  }

  return(predictors)
}

.linregRobustGetFormula <- function(dependent, predictors = NULL, includeConstant, covariates = NULL, quadraticTerms = FALSE) {
  if (is.null(predictors) && includeConstant == FALSE)
    return(NULL)
    # stop(gettext("We need at least one predictor, or an intercept to make a formula"))

  if (length(predictors) == 0) {
    formula <- paste(dependent, "~", "1")

  } else {

    if (quadraticTerms) {
      # select only the model terms that are main effects and covariates
      quadraticTerms <- predictors[predictors %in% covariates & !.linregRobustIsInteraction(predictors)]
      if (length(quadraticTerms) > 0) {
        quadraticTerms <- paste0("I(", quadraticTerms, "^2)")
        predictors <- c(predictors, quadraticTerms)
      }
    }

    if (includeConstant)
      formula <- paste(dependent, "~", paste(predictors, collapse = "+"))
    else
      formula <- paste(dependent, "~", paste(predictors, collapse = "+"), "-1")
  }

  return(as.formula(formula, env = parent.frame(1)))
}


.linregRobustIsInteraction <- function(predictor) {
  grepl(":", predictor)
}

.linregRobustMakeCombinedVariableFromInteraction <- function(interaction, dataset) {
  terms   <- unlist(strsplit(interaction, split = ":"))
  newVar  <- rep(1, nrow(dataset))
  for (i in seq_along(terms))
    newVar <- newVar * dataset[[terms[i]]]

  return(newVar)
}

.linregRobustGetPredictorColumnNames <- function(model, modelTerms) {
  usedPredictors  <- unique(unlist(lapply(model, function(x) x$predictors)))
  allpredictors   <- .linregRobustGetPredictors(modelTerms)
  return(intersect(allpredictors, usedPredictors)) # ensures that the terms appear in the covariance matrix like they appear in the model terms box
}

.linregRobustGetParameterNames <- function(model) {
  UseMethod(".linregRobustGetParameterNames", model)
}

.linregRobustGetParameterNames.lm <- function(model) {
  return(colnames(model.matrix(model)))
}

.linregRobustGetParameterNames.list <- function(model) {
  usedParameterNames <- unique(unlist(lapply(model, function(x) .linregRobustGetParameterNames(x[["fit"]])), use.names = FALSE))
  return(usedParameterNames)
}

.linregRobustAddPredictorsAsColumns <- function(jaspTable, model, includeIntercept = TRUE, type = "number", format = NULL, overtitle = NULL) {

  titles <- .linregRobustMakePrettyNames(model)
  if (!includeIntercept)
    titles <- titles[-1L]

  for (title in titles)
    jaspTable$addColumnInfo(name = title, title = title, type = type, format = format, overtitle = overtitle)

}

.linregRobustAddPredictorsInModelFootnote <- function(jaspTable, modelTerms, modelIndex, covariates, quadraticTerms) {
  if (length(modelTerms) > 0) {
    predictorsInModel <- .linregRobustGetPredictors(modelTerms)

    if (quadraticTerms) {
      # select only the model terms that are main effects and covariates
      quadraticTerms <- predictorsInModel[predictorsInModel %in% covariates & !.linregRobustIsInteraction(predictorsInModel)]
      quadraticTerms <- paste0(quadraticTerms, "\u00B2")
      predictorsInModel <- c(predictorsInModel, quadraticTerms)
    }

    modelName <- gettextf("M%s", intToUtf8(0x2080 + modelIndex - 1, multiple = FALSE))
    jaspTable$addFootnote(message = gettextf("%1$s includes %2$s", modelName, paste0(predictorsInModel, collapse = ", ")))
  }
}

.linregRobustAddVovkSellke <- function(jaspTable, wantsVovkSellkeMPR) {
  if (wantsVovkSellkeMPR) {
    jaspTable$addColumnInfo(name = "vovksellke", title = gettext("VS-MPR"), type = "number")
    #Haven't I seen the following footnote before?
    jaspTable$addFootnote(symbol = "\u002A", colNames = "vovksellke", message = gettextf("Vovk-Sellke Maximum <em>p</em>-Ratio: Based on the <em>p</em>-value, the maximum
        possible odds in favor of H%1$s over H%2$s equals 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> %3$s .37 (Sellke, Bayarri, & Berger, 2001).", "\u2081", "\u2080", "\u2264"))
  }
}

.linregRobustAddInterceptNotShownFootnote <- function(jaspTable, model, options) {
  indicesOfModelsWithPredictors <- .linregRobustGetIndicesOfModelsWithPredictors(model, options)
  if (options$interceptTerm && length(indicesOfModelsWithPredictors) != length(model)) {
    if (length(indicesOfModelsWithPredictors) > 0)
      jaspTable$addFootnote(gettext("The intercept model is omitted, as no meaningful information can be shown."))
    else
      jaspTable$addFootnote(gettext("There is only an intercept model, no meaningful information can be shown."))
  }
}

.linregRobustGetIndicesOfModelsWithPredictors <- function(model, options) {
  predictorsInNull  <- model[[1]]$predictors
  indices           <- seq_along(model)

  if (length(model) >= 1 && options$interceptTerm && length(predictorsInNull) == 0)
    indices <- indices[-1]

  return(indices)
}

.linregRobustInsertPlot <- function(jaspPlot, func, ...) {
  p <- try(func(...))

  if (inherits(p, "try-error")) {
   errorMessage <- .extractErrorMessage(p)
   jaspPlot$setError(gettextf("Plotting is not possible: %s", errorMessage))
  } else {
    jaspPlot$plotObject <- p
    jaspPlot$status     <- "complete"
  }
}

.linregRobustCreatePlotPlaceholder <- function(container, index, title, width = 530, height = 400) {
  jaspPlot            <- createJaspPlot(title = title, width = width, height = height)
  jaspPlot$status     <- "running"
  container[[index]]  <- jaspPlot
}

.linregRobustGetParametersAndLevels <- function(model, ...) {
  UseMethod(".linregRobustGetParametersAndLevels", model)
}

.linregRobustGetParametersAndLevels.lm <- function(model) {
  predictors <- all.vars(formula(model))[-1L]
  .linregRobustGetParametersAndLevels(.linregRobustGetParameterNames(model), predictors)
}

.linregRobustGetParametersAndLevels.default <- function(model, predictors) {

  # exception for intercept only model
  if (length(predictors) == 0L && identical(model, "(Intercept)")) {
    return(list(
      paramsClean = model,
      levelsClean = "",
      paramsRaw   = model,
      levelsRaw   = ""
    ))
  }

  orderedPredictors <- predictors[order(nchar(predictors))]
  # escape the parentheses
  orderedPredictors[which(orderedPredictors == "(Intercept)")] <- "\\(Intercept\\)"
  regexAnyPredictor <- paste(orderedPredictors, collapse = "|")

  lvls <- gsub(regexAnyPredictor, "", model)
  lvls <- gsub("I\\(\\^2\\)", "", lvls) # remove squared syntax from lvls

  levelsRaw   <- strsplit(lvls, ":")
  levelsRaw[lengths(levelsRaw) == 0] <- list("")
  # the above works except for n-way interactions between continuous predictors there needs to be one more empty level
  # so that the number of levels matches the number of predictors. The regex below finds these interactions and adds an
  # extra "" to the levels.
  for (i in grep("^(:+)$", lvls))
    levelsRaw[[i]] <- c(levelsRaw[[i]], "")

  if (length(levelsRaw[[1L]]) == 1L && levelsRaw[[1L]] == "(Intercept)")
    levelsRaw[[1L]] <- ""

  levelsClean <- jaspBase::gsubInteractionSymbol(lvls)

  splitRnms <- strsplit(model, ":")
  allPredsRegex <- paste0("^(", regexAnyPredictor, ").*")
  paramsRaw <- lapply(splitRnms, gsub, pattern = allPredsRegex, replacement = "\\1")
  paramsClean <- unlist(lapply(paramsRaw, paste, collapse = jaspBase::interactionSymbol), use.names = FALSE)

  return(list(
    paramsClean = paramsClean,
    levelsClean = levelsClean,
    paramsRaw   = paramsRaw,
    levelsRaw   = levelsRaw
  ))
}

#' Change R names into pretty names, e.g., PredictorLevel ->
#'
#' @param info either an lm object, a list where each sub element $fit contains an lm object, or the result of .linregRobustGetParametersAndLevels.
#'
#' @details For example "PredictorLevel" becomes "Predictor" Level
.linregRobustMakePrettyNames <- function(info) {
  UseMethod(".linregRobustMakePrettyNames", info)
}

.linregRobustMakePrettyNames.lm <- function(info) {
  .linregRobustMakePrettyNames(.linregRobustGetParametersAndLevels(info))
}

.linregRobustMakePrettyNames.list <- function(info) {

  if (is.null(names(info)) && !is.null(info[[1L]][["fit"]])) {
    # we could also distinguish between these cases by giving the return value of .linregRobustGetParametersAndLevels a class.
    return(unique(unlist(lapply(info, function(x) .linregRobustMakePrettyNames.lm(x[["fit"]])), use.names = FALSE)))
  }

  title <- character(length(info[["paramsRaw"]]))
  for (j in seq_along(title)) {

    params <- info[["paramsRaw"]][[j]]
    levels <- info[["levelsRaw"]][[j]]

    ans <- character(length(params))
    for (i in seq_along(params)) {
      ans[i] <- if (is.na(levels[i]) || levels[i] == "") .linregRobustPrettyQuadraticName(params[i]) else paste0(params[i], " (", levels[i], ")")
    }
    title[j] <- paste(ans, collapse = " \u2009\u273b\u2009 ")
  }
  return(title)
}

.linregRobustPrettyQuadraticName <- function(coefName) {

  if (grepl(pattern = "I\\(([^)]+)\\^2\\)", coefName)) {
    # remove "I(" and the "^2)" parts
    cleanName <- gsub("I\\(([^)]+)\\^2\\)", "\\1", coefName)
    # remove the leftover " (I(^2))"
    cleanName <- gsub("\\s*\\(I\\(\\^2\\)\\)", "", cleanName)
    coefName <- paste0(cleanName, "\u00B2")
  }

  return(coefName)
}



.linregRobustRemoveFactors <- function(fit, predictors) {

  terms <- terms(fit)
  dataClasses <- attr(terms, "dataClasses")
  factors     <- attr(terms, "factors")
  result <- logical(length(predictors))
  for (i in seq_along(result)) {
    consistsOf <- names(which(factors[, predictors[i]] == 1))
    result[i] <- !any(dataClasses[consistsOf] == "factor")
  }
  return(predictors[result])
}

.linregRobustContainsFactor <- function(x, predictors) {
  UseMethod(".linregRobustContainsFactor", x)
}

.linregRobustContainsFactor.data.frame <- function(x, predictors) {
  idx <- .linregRobustIsInteraction(predictors)
  if (any(idx))
    predictors <- unique(unlist(strsplit(predictors, ":", fixed = TRUE), use.names = FALSE))

  return(any(vapply(x[predictors], is.factor, logical(1L))))

}

.linregRobustContainsFactor.lm <- function(x, predictors) {
  # returns TRUE if the predictor is a factor or if it is an interaction that contains a factor
  terms <- terms(x)
  dataClasses <- attr(terms, "dataClasses")
  factors     <- attr(terms, "factors")
  consistsOf <- names(which(factors[, predictors] == 1))
  return(any(dataClasses[consistsOf] == "factor"))

}



.linregRobustCreateMarginalPlots <- function(modelContainer, finalModel, dataset, options, position = 17) {
  marginalPlotsContainer <- createJaspContainer(gettext("Marginal Effects Plots"))
  marginalPlotsContainer$dependOn(c("marginalPlot", "marginalPlotCi", "marginalPlotCiLevel",
                                    "marginalPlotPredictionInterval", "marginalPlotPredictionIntervalLevel"))
  marginalPlotsContainer$position <- position
  modelContainer[["marginalPlotsContainer"]] <- marginalPlotsContainer

  if (!is.null(finalModel)) {
    predictors <- finalModel$predictors
    # marginal plots display only the main effects
    predictors <- predictors[!grepl(":", predictors)]
    for (predictor in predictors)
      .linregRobustCreatePlotPlaceholder(marginalPlotsContainer,
                                   index = predictor,
                                   title = gettextf("Marginal effect of %1$s on %2$s", predictor, options$dependent))

    for (predictor in predictors) {
      .linregRobustFillMarginalPlots(marginalPlotsContainer[[predictor]], predictor, finalModel$fit, dataset, options)
    }
  }
}


.linregRobustFillMarginalPlots <- function(marginalPlot, predictor, fit, dataset, options) {
  xVar <- dataset[[predictor]]
  xVar <- stats::na.omit(xVar)


  means_ls = list()
  if (length(options[["factors"]]) > 0) {
    for (var in options[["factors"]]) {
      column_value = dataset[[var]]
      column_levels = levels(column_value)
      means_ls[[var]] = column_levels[1]
    }
  }

  if (length(options[['covariates']]) > 0) {
    for (var in options[['covariates']]) {
      column_value = dataset[[var]]
      column_mean = mean(column_value, na.rm = TRUE)
      means_ls[[var]] = column_mean
    }
  }

  means_ls[[predictor]] = NULL

  dd_sim = data.frame(predictor = xVar)
  colnames(dd_sim) = predictor

  if (length(means_ls) > 0) {
    dd_sim = cbind(dd_sim, means_ls)
  }

  fitted = predict(fit, newdata = dd_sim, interval = "none")

  if (options$marginalPlotCi == TRUE) {
    matrix_conf = predict(fit,
                          newdata = dd_sim,
                          interval = "confidence",
                          level = options[["marginalPlotCiLevel"]])
    conf_min = matrix_conf[, 'lwr']
    conf_max = matrix_conf[, 'upr']
  }
  else {
    conf_min = NULL
    conf_max = NULL
  }

  if (options$marginalPlotPredictionInterval == TRUE) {
    matrix_pred = predict(fit,
                          newdata = dd_sim,
                          interval = "prediction",
                          level = options[["marginalPlotPredictionIntervalLevel"]])
    pred_min = matrix_pred[, 'lwr']
    pred_max = matrix_pred[, 'upr']
  }
  else {
    pred_min = NULL
    pred_max = NULL
  }

  .linregRobustInsertPlot(marginalPlot,
                    .linregRobustMarginalPlot,
                    xVar = xVar,
                    xlab = predictor,
                    yVar = fitted,
                    ylab = options$dependent,
                    conf_min = conf_min,
                    conf_max = conf_max,
                    pred_min = pred_min,
                    pred_max = pred_max)
}


.linregRobustMarginalPlot <- function(xVar, xlab, yVar, ylab,
                                conf_min, conf_max, pred_min, pred_max, options) {

  d <- data.frame(x = xVar,
                  y = yVar)

  if (is.factor(xVar)) {
    d_factor <- unique(d)
    d_factor <- cbind(d_factor, group = 1)

    basicMarginalPlot <- ggplot2::ggplot() +
      ggplot2::geom_line(data = d_factor,
                         mapping = ggplot2::aes(x = x, y = y, group = group),
                         size = 1) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab)

    factorPoints <- jaspGraphs::geom_point(data = d_factor,
                                           mapping = ggplot2::aes(x = x, y = y))



  } else {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(xVar)

    basicMarginalPlot <- ggplot2::ggplot() +
      ggplot2::geom_line(data = d,
                         mapping = ggplot2::aes(x = x, y = y),
                         size = 1) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
      ggplot2::geom_rug(data = d,
                        mapping = ggplot2::aes(x = x, y = y),
                        sides = "b",
                        alpha = 0.5)
    factorPoints <- NULL
  }

  if (!is.null(conf_min)) {
    d <- cbind(d, conf_lower = conf_min, conf_upper = conf_max)
    if (is.factor(xVar)) {
      d_factor <- unique(d)
      confidenceBounds <- ggplot2::geom_errorbar(data = d_factor,
                                                 ggplot2::aes(x = x, y = y, ymin = conf_lower, ymax = conf_upper),
                                                 linetype = "solid",
                                                 width = 0.1,
                                                 size = 1)
    } else {
      confidenceBounds <- ggplot2::geom_ribbon(mapping = ggplot2::aes(x = x, ymin = conf_lower, ymax = conf_upper),
                                               alpha = .1,
                                               data = d,
                                               size = 1)
    }

  } else {
    confidenceBounds = NULL
  }

  if (!is.null(pred_min)) {
    d <- cbind(d, pred_lower = pred_min, pred_upper = pred_max)
    if (is.factor(xVar)) {
      d_factor <- unique(d)
      predictionBound1 <- ggplot2::geom_errorbar(data = d_factor,
                                                 ggplot2::aes(x = x, y = y, ymin = pred_lower, ymax = pred_upper),
                                                 linetype = "dashed",
                                                 width = 0.1,
                                                 size = 1)
      predictionBound2 <- NULL

    } else {
      predictionBound1 <- ggplot2::geom_line(
        mapping = ggplot2::aes(x = x, y = pred_lower),
        #color = "red",
        linetype = "dashed",
        data = d,
        size = 1)

      predictionBound2 <-
        ggplot2::geom_line(
          mapping = ggplot2::aes(x = x, y = pred_upper),
          #color = "red",
          linetype = "dashed",
          data = d,
          size = 1)
    }
  } else {
    predictionBounds <- predictionBound1 <- predictionBound2 <- NULL
  }

  # base y-axis breaks on y and the prediction and/or confidence interval
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(jaspGraphs::getPrettyAxisBreaks(unlist(d[, -1])))

  finalMarginalPlot <- basicMarginalPlot +
    confidenceBounds +
    predictionBound1 +
    predictionBound2 +
    factorPoints +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(axis.title.cex = 1.2) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))

  return(finalMarginalPlot)
}

# function taken from {boot.pval} version 0.4
# adjusted precision bug (alpha close to 1 causes some boot.ci type to crash)
.boot.pval <- function(boot_res,
                      type = "perc",
                      theta_null = 0,
                      pval_precision = NULL,
                      ...)
{
  if(is.null(pval_precision)) { pval_precision = 1/boot_res$R }

  # Create a sequence of alphas:
  # EDITED:
  alpha_seq <- seq(1e-10, 1-1e-10, pval_precision)
  # END EDITED

  # Compute the 1-alpha confidence intervals, and extract
  # their bounds:
  ci <- suppressWarnings(boot::boot.ci(boot_res,
                                       conf = 1- alpha_seq,
                                       type = type,
                                       ...))

  bounds <- switch(type,
                   norm = ci$normal[,2:3],
                   basic = ci$basic[,4:5],
                   stud = ci$student[,4:5],
                   perc = ci$percent[,4:5],
                   bca = ci$bca[,4:5])

  # Find the smallest alpha such that theta_null is not contained in the 1-alpha
  # confidence interval:
  alpha <- alpha_seq[which.min(theta_null >= bounds[,1] & theta_null <= bounds[,2])]

  # Return the p-value:
  return(alpha)
}


# ---- Inlined external functions (from jaspRegression) ----

# QQ plot for standardized residuals
.linregRobustFillPlotResQQ <- function(residType, model, options) {
  ciLevel <- if (!is.null(options[["qqPlotCi"]]) && options[["qqPlotCi"]]) options[["qqPlotCiLevel"]] else NULL

  if (residType == "deviance") {
    stdResid <- rstandard(model)
  } else {
    stdResid <- rstandard(model)
  }

  p <- jaspGraphs::plotQQnorm(stdResid, ablineColor = "darkred", ablineOrigin = TRUE,
                              identicalAxes = TRUE,
                              ciLevel = ciLevel)
  return(p)
}

# Influential cases table (casewise diagnostics)
.linregRobustInfluenceTable <- function(jaspResults, model, dataset, options, ready, position) {
  tableOptionsOn <- c(options[["dfbetas"]],
                      options[["dffits"]],
                      options[["covarianceRatio"]],
                      options[["leverage"]],
                      options[["mahalanobis"]])

  nModels <- length(options$modelTerms)
  if (!ready || !options[["residualCasewiseDiagnostic"]] ||
      length(unlist(options$modelTerms[[nModels]][["components"]])) == 0)
    return()

  tableOptions <- c("dfbetas", "dffits", "covarianceRatio", "leverage", "mahalanobis")
  tableOptionsClicked <- tableOptions[tableOptionsOn]
  tableOptionsClicked <- c("cooksDistance", tableOptionsClicked)

  if (is.null(jaspResults[["influenceTable"]])) {
    influenceTable <- createJaspTable(gettext("Influential Cases"))
    influenceTable$dependOn(c(tableOptions, "dependent", "modelTerms", "interceptTerm",
                              "residualCasewiseDiagnostic", "residualCasewiseDiagnosticType",
                              "residualCasewiseDiagnosticZThreshold",
                              "residualCasewiseDiagnosticCooksDistanceThreshold"))
    influenceTable$position <- position
    influenceTable$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["influenceTable"]] <- influenceTable
  } else {
    return()
  }

  tableOptionToColName <- function(x) {
    switch(x,
           "dfbetas"  = "DFBETAS",
           "dffits"   = "DFFITS",
           "covarianceRatio" = "Covariance Ratio",
           "cooksDistance"   = "Cook's Distance",
           "leverage" = "Leverage",
           "mahalanobis" = "Mahalanobis")
  }

  if (is.null(model)) {
    for (option in tableOptionsClicked) {
      colTitle    <- tableOptionToColName(option)
      influenceTable$addColumnInfo(name = option, title = gettext(colTitle), type = "number")
    }
  } else {
    depType <- if (is.numeric(dataset[[options[["dependent"]]]])) "number" else "string"
    influenceTable$addColumnInfo(name = "caseN", title = "Case Number", type = "integer")
    influenceTable$addColumnInfo(name = "stdResidual", title = gettext("Std. Residual"),   type = "number", format = "dp:3")
    influenceTable$addColumnInfo(name = "dependent",   title = options$dependent,          type = depType)
    influenceTable$addColumnInfo(name = "predicted",   title = gettext("Predicted Value"), type = "number")
    influenceTable$addColumnInfo(name = "residual", title = gettext("Residual"),   type = "number", format = "dp:3")

    colNameList  <- c()
    alwaysPresent <- c("caseN", "stdResidual", "dependent", "predicted", "residual")
    for (option in tableOptionsClicked) {
      if (option == "dfbetas") {
        predictors <- names(model$coefficients)
        for (predictor in predictors) {
          dfbetasName  <- gettextf("DFBETAS_%1s", predictor)
          colNameList <- c(colNameList, dfbetasName)
          if (predictor == "(Intercept)")
            dfbetasTitle <- gettext("DFBETAS:Intercept")
          else
            dfbetasTitle <- gettextf("DFBETAS:%1s", gsub(":", "*", predictor))
          influenceTable$addColumnInfo(name = dfbetasName, title = dfbetasTitle, type = "number")
        }
      } else {
        colNameList <- c(colNameList, option)
        colTitle    <- tableOptionToColName(option)
        influenceTable$addColumnInfo(name = option, title = gettext(colTitle), type = "number")
      }
    }

    .linregRobustInfluenceTableFill(influenceTable, dataset, options, ready,
                              model = model,
                              influenceMeasures = tableOptionsClicked,
                              colNames = c(colNameList, alwaysPresent))
  }
}

.linregRobustInfluenceTableFill <- function(influenceTable, dataset, options, ready, model, influenceMeasures, colNames) {
  influenceRes <- influence.measures(model)
  nDFBETAS     <- length(names(model$coefficients))

  optionToColInd <- function(x, nDFBETAS) {
    switch(x,
           "dfbetas"  = 1:nDFBETAS,
           "dffits"   = (nDFBETAS+1),
           "covarianceRatio" = (nDFBETAS+2),
           "cooksDistance"   = (nDFBETAS+3),
           "leverage" = (nDFBETAS+4))
  }

  colInd <- c()
  for (measure in influenceMeasures) {
    colInd <- c(colInd, optionToColInd(measure, nDFBETAS))
  }

  tempResult <- influenceRes[["infmat"]][, colInd]
  resultContainsNaN <- any(is.nan(tempResult))
  tempResult[which(is.nan(tempResult))] <- NA
  influenceResData <- as.data.frame(tempResult)
  colnames(influenceResData)[1:length(colInd)] <- colNames[1:length(colInd)]

  influenceResData[["caseN"]] <- as.numeric(rownames(influenceRes[["infmat"]]))
  influenceResData[["stdResidual"]] <- rstandard(model)
  influenceResData[["dependent"]] <- model.frame(model)[[options$dependent]]
  influenceResData[["predicted"]] <- model$fitted.values
  influenceResData[["residual"]] <- model$residual

  modelMatrix <- as.data.frame(model.matrix(model))
  modelMatrix <- modelMatrix[colnames(modelMatrix) != "(Intercept)"]

  if (ncol(modelMatrix) > 0) {
    influenceResData[["mahalanobis"]] <- mahalanobis(modelMatrix, center = colMeans(modelMatrix), cov = cov(modelMatrix))
  } else if (options[["mahalanobis"]]) {
    influenceTable$addFootnote(
      gettext("Mahalanobis distance cannot be computed for the intercept only model.")
    )
  }

  if (options$residualCasewiseDiagnosticType == "cooksDistance")
    index <- which(abs(influenceResData[["cooksDistance"]]) > options$residualCasewiseDiagnosticCooksDistanceThreshold)
  else if (options$residualCasewiseDiagnosticType == "outliersOutside")
    index <- which(abs(influenceResData[["stdResidual"]]) > options$residualCasewiseDiagnosticZThreshold)
  else # all
    index <- seq.int(nrow(influenceResData))

  influenceResSig       <- subset(influenceRes[["is.inf"]], 1:nrow(influenceResData) %in% index, select = colInd)
  colnames(influenceResSig) <- colNames[1:length(colInd)]
  influenceResData <- influenceResData[index, ]

  if (length(index) == 0)
    influenceTable$addFootnote(gettext("No influential cases found."))
  else {
    influenceTable$setData(influenceResData)
    if (sum(influenceResSig, na.rm = TRUE) > 0) {
      for (thisCol in colnames(influenceResSig)) {
        if (sum(influenceResSig[, thisCol], na.rm = TRUE) > 0)
          influenceTable$addFootnote(
            gettext("Potentially influential case, according to the selected influence measure."),
            colNames = thisCol,
            rowNames = rownames(influenceResData)[influenceResSig[, thisCol]],
            symbol = "*"
          )
      }
    }

    if (resultContainsNaN) {
      influenceTable$addFootnote(
        gettext("Influence measures could not be computed for some cases due to extreme values, try another measure.")
      )
    }
  }
}

# Durbin-Watson test (adapted from car)
.durbinWatsonTest.lm <- function(model, max.lag=1, simulate=TRUE, reps=1000,
                                method=c("resample","normal"),
                                alternative=c("two.sided", "positive", "negative"), ...) {
  method <- match.arg(method)
  alternative <- if (max.lag == 1) match.arg(alternative)
  else "two.sided"
  residuals <- residuals(model)
  if (any(is.na(residuals))) stop('residuals include missing values')
  n <- length(residuals)
  r <- dw <- rep(0, max.lag)
  den <- sum(residuals^2)
  for (lag in 1:max.lag) {
    dw[lag] <- (sum((residuals[(lag+1):n] - residuals[1:(n-lag)])^2))/den
    r[lag] <- (sum(residuals[(lag+1):n]*residuals[1:(n-lag)]))/den
  }
  if (!simulate) {
    result <- list(r=r, dw=dw)
    class(result) <- "durbinWatsonTest"
    result
  } else {
    X <- unname(model.matrix(model))

    qrX <- qr(X)
    R <- qr.R(qrX)
    Rinv <- backsolve(r = R, x = diag(ncol(R)))
    inv_XTX_XT <- tcrossprod(tcrossprod(Rinv), X)

    bootResult <- boot::boot(unname(residuals), statistic = function(data, indices, inv_XTX_XT, max.lag) {
      y <- data[indices]
      resids <- c(y - X %*% (inv_XTX_XT %*% y))
      .durbinWatsonTest.default(resids, max.lag = max.lag)
    }, R = reps, inv_XTX_XT = inv_XTX_XT, max.lag = max.lag)
    DW <- bootResult$t

    p <- rep(0, max.lag)
    if (alternative == 'two.sided') {
      for (lag in 1:max.lag) {
        p[lag] <- (sum(dw[lag] < DW[lag,]))/reps
        p[lag] <- 2*(min(p[lag], 1 - p[lag]))
      }
    } else if (alternative == 'positive') {
      for (lag in 1:max.lag) {
        p[lag] <- (sum(dw[lag] > DW[lag,]))/reps
      }
    } else {
      for (lag in 1:max.lag) {
        p[lag] <- (sum(dw[lag] < DW[lag,]))/reps
      }
    }
    result <- list(r=r, dw=dw, p=p, alternative=alternative)
    class(result) <- "durbinWatsonTest"
    result
  }
}

.durbinWatsonTest.default <- function(model, max.lag=1, ...) {
  if ((!is.vector(model)) || (!is.numeric(model))) stop("requires vector of residuals")
  if (any(is.na(model))) stop('residuals include missing values')
  n <- length(model)
  dw <- rep(0, max.lag)
  den <- sum(model^2)
  for (lag in 1:max.lag) {
    dw[lag] <- (sum((model[(lag+1):n] - model[1:(n-lag)])^2))/den
  }
  dw
}
