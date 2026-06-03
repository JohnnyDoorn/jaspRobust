#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' RegressionLinearRobust
#'
RegressionLinearRobust <- function(
          data = NULL,
          version = "0.19",
          formula = NULL,
          dependent = list(types = list(), value = ""),
          covariates = list(types = list(), value = list()),
          factors = list(types = list(), value = list()),
          weights = list(types = list(), value = ""),
          modelTerms = list(optionKey = "components", types = list(), value = list()),
          nested = TRUE,
          interceptTerm = TRUE,
          quadraticTerms = FALSE,
          estimationMethod = "ols",
          rSquaredChange = TRUE,
          fChange = FALSE,
          modelAICBIC = FALSE,
          residualDurbinWatson = FALSE,
          coefficientEstimate = TRUE,
          coefficientBootstrap = FALSE,
          coefficientBootstrapSamples = 5000,
          coefficientCi = FALSE,
          coefficientCiLevel = 0.95,
          collinearityStatistic = FALSE,
          vovkSellke = FALSE,
          modelFit = TRUE,
          descriptives = FALSE,
          partAndPartialCorrelation = FALSE,
          covarianceMatrix = FALSE,
          collinearityDiagnostic = FALSE,
          equationTable = FALSE,
          residualStatistic = FALSE,
          residualCasewiseDiagnostic = FALSE,
          residualCasewiseDiagnosticType = "outliersOutside",
          residualCasewiseDiagnosticZThreshold = 3,
          residualCasewiseDiagnosticCooksDistanceThreshold = 1,
          dfbetas = FALSE,
          dffits = FALSE,
          covarianceRatio = FALSE,
          leverage = FALSE,
          mahalanobis = FALSE,
          residualVsDependentPlot = FALSE,
          residualVsCovariatePlot = FALSE,
          residualVsFittedPlot = FALSE,
          residualHistogramPlot = FALSE,
          residualHistogramStandardizedPlot = TRUE,
          residualQqPlot = FALSE,
          qqPlotCi = FALSE,
          qqPlotCiLevel = 0.95,
          partialResidualPlot = FALSE,
          partialResidualPlotCi = FALSE,
          partialResidualPlotCiLevel = 0.95,
          partialResidualPlotPredictionInterval = FALSE,
          partialResidualPlotPredictionIntervalLevel = 0.95,
          marginalPlot = FALSE,
          marginalPlotCi = FALSE,
          marginalPlotCiLevel = 0.95,
          marginalPlotPredictionInterval = FALSE,
          marginalPlotPredictionIntervalLevel = 0.95) {

   defaultArgCalls <- formals(jaspRob::RegressionLinearRobust)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   if (!is.null(formula)) {
      if (!inherits(formula, "formula")) {
         formula <- as.formula(formula)
      }
      options$formula <- jaspBase::jaspFormula(formula, data)
   }
   optionsWithFormula <- c("dependent", "covariates", "factors", "weights", "modelTerms")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)
   }

   return(jaspBase::runWrappedAnalysis("jaspRob", "RegressionLinearRobust", "RegressionLinearRobust.qml", options, version, FALSE))
}
