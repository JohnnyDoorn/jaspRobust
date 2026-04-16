#
# Copyright (C) 2013-2026 University of Amsterdam
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

# Robust linear regression using MASS::rlm (M-estimation / MM-estimation).

.regressionRobustDeps <- c("dependent", "covariates", "factors",
                            "estimationMethod")

RegressionLinearInternal <- function(jaspResults, dataset, options) {
  ready <- options$dependent != "" && length(c(unlist(options$covariates), unlist(options$factors))) > 0

  .regressionRobustCheckErrors(dataset, options, ready)

  results <- .regressionRobustComputeResults(jaspResults, dataset, options, ready)

  .regressionRobustCoefficientTable(jaspResults, dataset, options, results, ready)

  .regressionRobustModelSummaryTable(jaspResults, dataset, options, results, ready)

  .regressionRobustResidualPlots(jaspResults, dataset, options, results, ready)
}

# Entry point for JASP (func: "RegressionLinearRobust" in Description.qml)
RegressionLinearRobustInternal <- RegressionLinearInternal


# ---- Error checking ----

.regressionRobustCheckErrors <- function(dataset, options, ready) {
  if (!ready) return()

  numericVars <- c(options$dependent, unlist(options$covariates))

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations", "variance"),
    infinity.target      = numericVars,
    variance.target      = numericVars,
    observations.target  = numericVars,
    observations.amount  = "< 3",
    exitAnalysisIfErrors = TRUE
  )
}


# ---- Compute results ----

.regressionRobustComputeResults <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()
  if (!is.null(jaspResults[["stateRegressionRobustResults"]]))
    return(jaspResults[["stateRegressionRobustResults"]]$object)

  formula <- .regressionRobustBuildFormula(options)
  method  <- options$estimationMethod

  psiFunc <- switch(method,
    "huber"    = MASS::psi.huber,
    "bisquare" = MASS::psi.bisquare,
    MASS::psi.huber
  )

  rlmMethod <- if (method == "mm") "MM" else "M"

  fit <- try(MASS::rlm(formula, data = dataset, method = rlmMethod, psi = psiFunc), silent = TRUE)

  if (isTryError(fit)) {
    msg <- trimws(gsub("^Error.*?:\\s*", "", as.character(fit)))
    .quitAnalysis(gettextf("Robust regression failed: %s", msg))
  }

  s <- summary(fit)

  # Wald p-values (rlm does not provide p-values by default)
  tvals <- s$coefficients[, "t value"]
  pvals <- 2 * stats::pnorm(-abs(tvals))

  results <- list(
    fit    = fit,
    summ   = s,
    pvals  = pvals,
    method = method
  )

  state <- createJaspState(results)
  state$dependOn(.regressionRobustDeps)
  jaspResults[["stateRegressionRobustResults"]] <- state

  return(results)
}


.regressionRobustBuildFormula <- function(options) {
  dependent  <- options$dependent
  predictors <- c(unlist(options$covariates), unlist(options$factors))
  if (length(predictors) == 0) return(NULL)

  rhs <- paste0("`", predictors, "`", collapse = " + ")

  formula <- as.formula(paste0("`", dependent, "` ~ ", rhs))

  return(formula)
}


# ---- Coefficient table ----

.regressionRobustCoefficientTable <- function(jaspResults, dataset, options, results, ready) {
  if (!is.null(jaspResults[["coefficientTable"]])) return()

  table <- createJaspTable(title = gettext("Robust Regression Coefficients"))
  table$dependOn(c(.regressionRobustDeps, "coefficientEstimate", "coefficientCi", "coefficientCiLevel"))
  table$showSpecifiedColumnsOnly <- TRUE
  table$position <- 1

  table$addColumnInfo(name = "term",     title = gettext("Term"),        type = "string")
  table$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type = "number")
  table$addColumnInfo(name = "se",       title = gettext("Std. Error"),  type = "number")
  table$addColumnInfo(name = "tval",     title = gettext("t"),           type = "number")
  table$addColumnInfo(name = "pval",     title = gettext("p"),           type = "pvalue")

  if (isTRUE(options$coefficientCi)) {
    ciLevel <- options$coefficientCiLevel
    ciTitle <- gettextf("%s%% CI", round(ciLevel * 100))
    table$addColumnInfo(name = "ciLower", title = gettext("Lower"), type = "number",
                        overtitle = ciTitle)
    table$addColumnInfo(name = "ciUpper", title = gettext("Upper"), type = "number",
                        overtitle = ciTitle)
  }

  methodLabel <- switch(options$estimationMethod,
    "huber"    = gettext("Huber M-estimation"),
    "bisquare" = gettext("Bisquare M-estimation"),
    "mm"       = gettext("MM-estimation"),
    gettext("M-estimation")
  )
  table$addFootnote(gettextf("Method: %s. P-values computed via Wald test.", methodLabel))

  jaspResults[["coefficientTable"]] <- table

  if (!ready || is.null(results)) return()

  coefs <- results$summ$coefficients
  termNames <- rownames(coefs)

  ciLower <- ciUpper <- NULL
  if (isTRUE(options$coefficientCi)) {
    ci <- confint.default(results$fit, level = options$coefficientCiLevel)
    ciLower <- ci[, 1]
    ciUpper <- ci[, 2]
  }

  for (i in seq_along(termNames)) {
    row <- list(
      term     = termNames[i],
      estimate = coefs[i, "Value"],
      se       = coefs[i, "Std. Error"],
      tval     = coefs[i, "t value"],
      pval     = results$pvals[i]
    )
    if (isTRUE(options$coefficientCi)) {
      row$ciLower <- ciLower[i]
      row$ciUpper <- ciUpper[i]
    }
    table$addRows(row)
  }
}


# ---- Model summary table ----

.regressionRobustModelSummaryTable <- function(jaspResults, dataset, options, results, ready) {
  if (!is.null(jaspResults[["modelSummaryTable"]])) return()

  table <- createJaspTable(title = gettext("Model Summary"))
  table$dependOn(c(.regressionRobustDeps))
  table$showSpecifiedColumnsOnly <- TRUE
  table$position <- 2

  table$addColumnInfo(name = "nObs",       title = gettext("n"),                 type = "integer")
  table$addColumnInfo(name = "scale",      title = gettext("Residual Scale"),    type = "number")
  table$addColumnInfo(name = "nDownweight", title = gettext("Downweighted Obs."), type = "integer")
  table$addColumnInfo(name = "converged",  title = gettext("Converged"),         type = "string")

  jaspResults[["modelSummaryTable"]] <- table

  if (!ready || is.null(results)) return()

  fit <- results$fit
  w   <- fit$w

  table$addRows(list(
    nObs        = length(w),
    scale       = results$summ$sigma,
    nDownweight = sum(w < 1),
    converged   = if (isTRUE(fit$converged)) "\u2713" else "\u2717"
  ))
}


# ---- Plots ----

.regressionRobustResidualPlots <- function(jaspResults, dataset, options, results, ready) {
  .regressionRobustResVsFittedPlot(jaspResults, options, results, ready)
  .regressionRobustQQPlot(jaspResults, options, results, ready)
  .regressionRobustWeightsPlot(jaspResults, options, results, ready)
}


.regressionRobustResVsFittedPlot <- function(jaspResults, options, results, ready) {
  if (!isTRUE(options$residualVsFittedPlot)) return()
  if (!is.null(jaspResults[["residualVsFittedPlot"]])) return()

  plot <- createJaspPlot(title = gettext("Residuals vs. Fitted"), width = 480, height = 320)
  plot$dependOn(c(.regressionRobustDeps, "residualVsFittedPlot"))
  plot$position <- 3
  jaspResults[["residualVsFittedPlot"]] <- plot

  if (!ready || is.null(results)) return()

  fit <- results$fit
  df  <- data.frame(x = fitted(fit), y = residuals(fit))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df$y)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    jaspGraphs::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "steelblue", formula = y ~ x) +
    ggplot2::scale_x_continuous(name = gettext("Fitted values"),  breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Residuals"),      breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
}


.regressionRobustQQPlot <- function(jaspResults, options, results, ready) {
  if (!isTRUE(options$residualQqPlot)) return()
  if (!is.null(jaspResults[["residualQqPlot"]])) return()

  plot <- createJaspPlot(title = gettext("Q-Q Plot of Residuals"), width = 400, height = 400)
  plot$dependOn(c(.regressionRobustDeps, "residualQqPlot"))
  plot$position <- 4
  jaspResults[["residualQqPlot"]] <- plot

  if (!ready || is.null(results)) return()

  fit  <- results$fit
  sres <- residuals(fit) / results$summ$sigma

  p <- jaspGraphs::plotQQnorm(sres, ablineColor = "darkred")
  plot$plotObject <- p
}


.regressionRobustWeightsPlot <- function(jaspResults, options, results, ready) {
  if (!isTRUE(options$weightsPlot)) return()
  if (!is.null(jaspResults[["weightsPlot"]])) return()

  plot <- createJaspPlot(title = gettext("Robust Weights"), width = 480, height = 320)
  plot$dependOn(c(.regressionRobustDeps, "weightsPlot"))
  plot$position <- 5
  jaspResults[["weightsPlot"]] <- plot

  if (!ready || is.null(results)) return()

  fit <- results$fit
  df  <- data.frame(x = seq_along(fit$w), y = fit$w)
  downweighted <- fit$w < 1

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1.05))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    jaspGraphs::geom_point(color = ifelse(downweighted, "tomato", "black")) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    ggplot2::scale_x_continuous(name = gettext("Observation"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Weight"),      breaks = yBreaks, limits = c(0, 1.05)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
}
