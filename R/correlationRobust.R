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

# Robust correlation using WRS2 (percentage bend / Winsorized correlation).

.correlationRobustDeps <- c("variables", "correlationMethod", "alternative")

CorrelationRobustInternal <- function(jaspResults, dataset, options) {
  ready <- length(options$variables) >= 2

  .correlationRobustCheckErrors(dataset, options, ready)

  corrResults <- .correlationRobustCompute(jaspResults, dataset, options, ready)

  .correlationRobustTable(jaspResults, dataset, options, corrResults, ready)

  .correlationRobustScatterPlots(jaspResults, dataset, options, ready)
}


# ---- Error checking ----

.correlationRobustCheckErrors <- function(dataset, options, ready) {
  if (!ready) return()

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations", "variance"),
    infinity.target      = options$variables,
    variance.target      = options$variables,
    observations.target  = options$variables,
    observations.amount  = "< 3",
    exitAnalysisIfErrors = TRUE
  )
}


# ---- Compute results ----

.correlationRobustCompute <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()
  if (!is.null(jaspResults[["stateCorrelationRobustResults"]]))
    return(jaspResults[["stateCorrelationRobustResults"]]$object)

  vars  <- options$variables
  pairs <- combn(vars, 2, simplify = FALSE)
  method <- options$correlationMethod

  results <- list()

  for (pair in pairs) {
    v1 <- pair[1]
    v2 <- pair[2]
    pairName <- paste(sort(c(v1, v2)), collapse = " - ")

    x <- dataset[[v1]]
    y <- dataset[[v2]]

    # Remove pairwise NA
    complete <- complete.cases(x, y)
    x <- x[complete]
    y <- y[complete]

    res <- try(switch(method,
      "percentageBend" = WRS2::pbcor(x, y),
      "winsorized"     = WRS2::wincor(x, y)
    ), silent = TRUE)

    if (isTryError(res)) {
      results[[pairName]] <- list(
        var1 = v1, var2 = v2,
        cor = NaN, stat = NaN, pvalue = NaN, n = length(x),
        error = .extractErrorMessage(res)
      )
    } else {
      pval <- res$p.value

      # Adjust for one-sided tests
      alt <- options$alternative
      if (alt != "twoSided") {
        tstat <- res$test
        if (alt == "greater") {
          pval <- if (tstat > 0) pval / 2 else 1 - pval / 2
        } else {
          pval <- if (tstat < 0) pval / 2 else 1 - pval / 2
        }
      }

      results[[pairName]] <- list(
        var1 = v1, var2 = v2,
        cor = res$cor, stat = res$test, pvalue = pval, n = res$n,
        error = NULL
      )
    }
  }

  state <- createJaspState(results)
  state$dependOn(c(.correlationRobustDeps))
  jaspResults[["stateCorrelationRobustResults"]] <- state

  return(results)
}


# ---- Correlation table ----

.correlationRobustTable <- function(jaspResults, dataset, options, corrResults, ready) {
  if (!is.null(jaspResults[["correlationTable"]])) return()

  methodLabel <- switch(options$correlationMethod,
    "percentageBend" = gettext("Percentage Bend"),
    "winsorized"     = gettext("Winsorized"),
    gettext("Robust")
  )

  table <- createJaspTable(title = gettextf("%s Correlation", methodLabel))
  table$dependOn(c(.correlationRobustDeps, "significanceFlagged", "sampleSize"))
  table$showSpecifiedColumnsOnly <- TRUE
  table$position <- 1

  table$addColumnInfo(name = "var1", title = "",                    type = "string")
  table$addColumnInfo(name = "var2", title = "",                    type = "string")
  table$addColumnInfo(name = "cor",  title = gettext("r"),          type = "number")
  table$addColumnInfo(name = "stat", title = gettext("t"),          type = "number")
  table$addColumnInfo(name = "pval", title = gettext("p"),          type = "pvalue")

  if (isTRUE(options$sampleSize))
    table$addColumnInfo(name = "n", title = gettext("n"), type = "integer")

  altLabel <- switch(options$alternative,
    "twoSided" = gettext("two-sided"),
    "greater"  = gettext("greater"),
    "less"     = gettext("less")
  )
  table$addFootnote(gettextf("Alternative hypothesis: %s.", altLabel))

  jaspResults[["correlationTable"]] <- table

  if (!ready || is.null(corrResults)) return()

  for (pairName in names(corrResults)) {
    r <- corrResults[[pairName]]

    if (!is.null(r$error)) {
      row <- list(var1 = r$var1, var2 = r$var2)
      table$addRows(row)
      table$addFootnote(gettextf("Computation failed for %1$s - %2$s: %3$s", r$var1, r$var2, r$error))
      next
    }

    corDisplay <- r$cor
    if (isTRUE(options$significanceFlagged) && !is.na(r$pvalue)) {
      if (r$pvalue < .001)     corDisplay <- gettextf("%s***", formatC(r$cor, format = "f", digits = 3))
      else if (r$pvalue < .01) corDisplay <- gettextf("%s**",  formatC(r$cor, format = "f", digits = 3))
      else if (r$pvalue < .05) corDisplay <- gettextf("%s*",   formatC(r$cor, format = "f", digits = 3))
    }

    row <- list(
      var1 = r$var1,
      var2 = r$var2,
      cor  = r$cor,
      stat = r$stat,
      pval = r$pvalue
    )

    if (isTRUE(options$sampleSize))
      row$n <- r$n

    table$addRows(row)
  }

  if (isTRUE(options$significanceFlagged))
    table$addFootnote(gettext("* p < .05, ** p < .01, *** p < .001"))
}


# ---- Scatter plots ----

.correlationRobustScatterPlots <- function(jaspResults, dataset, options, ready) {
  if (!isTRUE(options$scatterPlot)) return()
  if (!is.null(jaspResults[["scatterPlotContainer"]])) return()

  vars <- options$variables

  container <- createJaspContainer(gettext("Scatter Plots"))
  container$dependOn(c(.correlationRobustDeps, "scatterPlot"))
  container$position <- 2
  jaspResults[["scatterPlotContainer"]] <- container

  if (!ready) return()

  pairs <- combn(vars, 2, simplify = FALSE)

  for (pair in pairs) {
    v1 <- pair[1]
    v2 <- pair[2]
    plotName <- paste(v1, v2, sep = "-")

    plot <- createJaspPlot(title = gettextf("%1$s - %2$s", v1, v2), width = 480, height = 400)
    container[[plotName]] <- plot

    x <- dataset[[v1]]
    y <- dataset[[v2]]

    complete <- complete.cases(x, y)
    x <- x[complete]
    y <- y[complete]

    p <- try(jaspGraphs::JASPScatterPlot(
      x = x, y = y,
      xName = v1, yName = v2,
      addSmooth = TRUE,
      forceLinearSmooth = TRUE,
      plotAbove = "density",
      plotRight = "density"
    ), silent = TRUE)

    if (isTryError(p)) {
      plot$setError(gettextf("Plotting failed: %s", .extractErrorMessage(p)))
    } else {
      plot$plotObject <- p
    }
  }
}
