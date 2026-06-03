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

#' AnovaRepeatedMeasuresRobust
#'
AnovaRepeatedMeasuresRobust <- function(
          data = NULL,
          version = "0.19",
          formula = NULL,
          betweenModelTerms = list(optionKey = "components", types = list(), value = list()),
          betweenSubjectFactors = list(types = list(), value = list()),
          contrastCi = FALSE,
          contrastCiLevel = 0.95,
          contrasts = list(optionKey = "variable", types = list(), value = list()),
          covariates = list(types = list(), value = list()),
          descriptivePlotCiLevel = 0.95,
          descriptivePlotErrorBar = FALSE,
          descriptivePlotErrorBarType = "ci",
          descriptivePlotHorizontalAxis = list(types = list(), value = ""),
          descriptivePlotSeparateLines = list(types = list(), value = ""),
          descriptivePlotSeparatePlot = list(types = list(), value = ""),
          postHocCorrectionBonferroni = FALSE,
          postHocCorrectionHolm = FALSE,
          postHocCorrectionScheffe = FALSE,
          postHocCorrectionTukey = TRUE,
          postHocTerms = list(optionKey = "variable", types = list(), value = list()),
          rainCloudHorizontalAxis = list(types = list(), value = ""),
          rainCloudHorizontalDisplay = FALSE,
          rainCloudSeparatePlots = list(types = list(), value = ""),
          rainCloudYAxisLabel = "",
          repeatedMeasuresCells = list(types = list(), value = list()),
          repeatedMeasuresFactors = list(),
          withinModelTerms = list(optionKey = "components", types = list(), value = list())) {

   defaultArgCalls <- formals(jaspRob::AnovaRepeatedMeasuresRobust)
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
   optionsWithFormula <- c("betweenModelTerms", "betweenSubjectFactors", "contrasts",
                           "covariates", "descriptivePlotHorizontalAxis",
                           "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot",
                           "postHocTerms", "rainCloudHorizontalAxis", "rainCloudSeparatePlots",
                           "repeatedMeasuresCells", "repeatedMeasuresFactors", "withinModelTerms")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)
   }

   return(jaspBase::runWrappedAnalysis("jaspRob", "AnovaRepeatedMeasuresRobust", "AnovaRepeatedMeasuresRobust.qml", options, version, FALSE))
}
