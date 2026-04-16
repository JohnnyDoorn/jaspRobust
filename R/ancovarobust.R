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

# Shared implementation for both ANOVA (no covariates) and ANCOVA (with covariates).
# AnovaInternal delegates here directly.

.ancovaRobustDeps <- c("dependent", "fixedFactors", "covariates",
                        "robustMethod", "trimProportion", "bootstrapSamples")

AncovaInternal <- function(jaspResults, dataset, options) {
  covariates <- unlist(options$covariates)
  hasCovariates <- length(covariates) > 0 && any(nzchar(covariates))

  factors <- unlist(options$fixedFactors)
  ready <- options$dependent != "" && length(factors) > 0 && any(nzchar(factors))

  dataset <- droplevels(dataset)

  .ancovaRobustCheckErrors(dataset, options, ready, hasCovariates)

  results <- .ancovaRobustComputeResults(jaspResults, dataset, options, ready, hasCovariates)

  if (hasCovariates) {
    .ancovaRobustTableAncova(jaspResults, dataset, options, results, ready)
  } else {
    .ancovaRobustTableAnova(jaspResults, dataset, options, results, ready)
    .ancovaRobustPostHocTable(jaspResults, dataset, options, results, ready)
  }

  .ancovaRobustDescriptivesPlots(jaspResults, dataset, options, ready)

  .ancovaRobustRainCloudPlots(jaspResults, dataset, options, ready)
}

# Entry point for JASP (func: "AncovaRobust" in Description.qml)
AncovaRobustInternal <- AncovaInternal


# ---- Error checking ----

.ancovaRobustCheckErrors <- function(dataset, options, ready, hasCovariates) {
  if (!ready) return()

  numericVariables <- c(options$dependent, unlist(options$covariates))

  .hasErrors(
    dataset              = dataset,
    type                 = "infinity",
    infinity.target      = numericVariables,
    exitAnalysisIfErrors = TRUE
  )

  if (hasCovariates) {
    factor  <- unlist(options$fixedFactors)[1]
    nLevels <- nlevels(dataset[[factor]])
    if (nLevels != 2)
      .quitAnalysis(gettextf(
        "Robust ANCOVA requires a factor with exactly 2 levels, but '%1$s' has %2$d levels.",
        factor, nLevels
      ))
  }
}


# ---- Compute results ----

.ancovaRobustComputeResults <- function(jaspResults, dataset, options, ready, hasCovariates) {
  if (!ready) return()
  if (!is.null(jaspResults[["stateAncovaRobustResults"]])) return(jaspResults[["stateAncovaRobustResults"]]$object)

  dependent <- options$dependent
  factors   <- unlist(options$fixedFactors)
  method    <- options$robustMethod
  tr        <- options$trimProportion
  nboot     <- options$bootstrapSamples

  if (hasCovariates) {
    results <- .ancovaRobustComputeAncova(dependent, factors, options$covariates[[1]],
                                           method, tr, nboot, dataset)
  } else {
    results <- .ancovaRobustComputeAnova(dependent, factors, method, tr, nboot, dataset)
  }

  state <- createJaspState(results)
  state$dependOn(.ancovaRobustDeps)
  jaspResults[["stateAncovaRobustResults"]] <- state

  return(results)
}


.ancovaRobustComputeAncova <- function(dependent, factors, covariate, method, tr, nboot, dataset) {
  factor  <- factors[1]
  formula <- as.formula(paste0("`", dependent, "` ~ `", factor, "` + `", covariate, "`"))

  if (method == "trimmedMeansBootstrap") {
    result <- try(WRS2::ancboot(formula, data = dataset, tr = tr, nboot = nboot), silent = TRUE)
  } else {
    result <- try(WRS2::ancova(formula, data = dataset, tr = tr), silent = TRUE)
  }

  if (isTryError(result)) {
    msg <- trimws(gsub("^Error.*?:\\s*", "", as.character(result)))
    .quitAnalysis(gettextf("Robust ANCOVA failed: %s", msg))
  }

  list(result = result, method = method, type = "ancova")
}


.ancovaRobustComputeAnova <- function(dependent, factors, method, tr, nboot, dataset) {
  nFactors <- length(factors)
  formula  <- as.formula(paste0("`", dependent, "` ~ `",
                                paste(factors, collapse = "` * `"), "`"))

  result <- NULL

  if (method == "trimmedMeans") {
    if (nFactors == 1) {
      result <- try(WRS2::t1way(formula, data = dataset, tr = tr), silent = TRUE)
    } else if (nFactors == 2) {
      result <- try(WRS2::t2way(formula, data = dataset, tr = tr), silent = TRUE)
    } else if (nFactors == 3) {
      result <- try(WRS2::t3way(formula, data = dataset, tr = tr), silent = TRUE)
    } else {
      .quitAnalysis(gettext("Robust ANOVA with trimmed means supports at most 3 factors."))
    }

  } else if (method == "trimmedMeansBootstrap") {
    if (nFactors == 1) {
      result <- try(WRS2::t1waybt(formula, data = dataset, tr = tr, nboot = nboot), silent = TRUE)
    } else {
      .quitAnalysis(gettext("Bootstrap robust ANOVA is currently only supported for one-way designs."))
    }

  } else if (method == "medians") {
    if (nFactors == 1) {
      result <- try(WRS2::med1way(formula, data = dataset), silent = TRUE)
    } else if (nFactors == 2) {
      result <- try(WRS2::med2way(formula, data = dataset), silent = TRUE)
    } else {
      .quitAnalysis(gettext("Robust ANOVA with medians supports at most 2 factors."))
    }
  }

  if (isTryError(result)) {
    msg <- trimws(gsub("^Error.*?:\\s*", "", as.character(result)))
    .quitAnalysis(gettextf("Robust ANOVA failed: %s", msg))
  }

  list(result = result, method = method, type = "anova", nFactors = nFactors)
}


# ---- ANCOVA table (with covariates) ----

.ancovaRobustTableAncova <- function(jaspResults, dataset, options, results, ready) {
  if (!is.null(jaspResults[["ancovaRobustTable"]])) return()

  ancovaTable <- createJaspTable(title = gettext("Robust ANCOVA"))
  ancovaTable$dependOn(.ancovaRobustDeps)
  ancovaTable$showSpecifiedColumnsOnly <- TRUE

  ancovaTable$addColumnInfo(name = "covariate", title = gettext("Covariate Value"), type = "number")
  ancovaTable$addColumnInfo(name = "n1",        title = gettext("n (Group 1)"),     type = "integer")
  ancovaTable$addColumnInfo(name = "n2",        title = gettext("n (Group 2)"),     type = "integer")
  ancovaTable$addColumnInfo(name = "trDiff",    title = gettext("\u0394 Trimmed Mean"),  type = "number")
  ancovaTable$addColumnInfo(name = "se",        title = gettext("SE"),              type = "number")
  ancovaTable$addColumnInfo(name = "ciLower",   title = gettext("Lower"),           type = "number",
                             overtitle = gettext("95% CI"))
  ancovaTable$addColumnInfo(name = "ciUpper",   title = gettext("Upper"),           type = "number",
                             overtitle = gettext("95% CI"))
  ancovaTable$addColumnInfo(name = "test",      title = gettext("Test Statistic"),  type = "number")
  ancovaTable$addColumnInfo(name = "p",         title = gettext("p"),               type = "pvalue")

  jaspResults[["ancovaRobustTable"]] <- ancovaTable

  if (!ready) return()

  res    <- results$result
  method <- results$method
  factor <- options$fixedFactors[[1]]
  lvls   <- levels(dataset[[factor]])

  nPts <- length(res$evalpts)
  for (i in seq_len(nPts)) {
    row <- list(
      covariate = res$evalpts[i],
      n1        = res$n1[i],
      n2        = res$n2[i],
      trDiff    = res$trDiff[i],
      se        = res$se[i],
      ciLower   = if (!is.null(res$ci.low))  res$ci.low[i]  else NA,
      ciUpper   = if (!is.null(res$ci.hi))   res$ci.hi[i]   else NA,
      test      = res$test[i],
      p         = res$p.vals[i]
    )
    ancovaTable$addRows(row)
  }

  ancovaTable$addFootnote(gettextf("Group 1: %1$s; Group 2: %2$s", lvls[1], lvls[2]))

  if (method == "trimmedMeansBootstrap") {
    ancovaTable$addFootnote(gettextf(
      "Bootstrap ANCOVA with trimmed means (%.0f%% trimming, %d bootstrap samples).",
      options$trimProportion * 100, options$bootstrapSamples
    ))
  } else {
    ancovaTable$addFootnote(gettextf("Trimmed means compared using %.0f%% trimming.",
                                      options$trimProportion * 100))
  }

  ancovaTable$addCitation(gettext("Wilcox, R. (2012). Introduction to Robust Estimation and Hypothesis Testing (3rd ed.). Elsevier."))
}


# ---- ANOVA table (no covariates) ----

.ancovaRobustTableAnova <- function(jaspResults, dataset, options, results, ready) {
  if (!is.null(jaspResults[["anovaRobustTable"]])) return()

  anovaTable <- createJaspTable(title = gettext("Robust ANOVA"))
  anovaTable$dependOn(.ancovaRobustDeps)
  anovaTable$showSpecifiedColumnsOnly <- TRUE

  method   <- if (ready) results$method   else options$robustMethod
  nFactors <- if (ready) results$nFactors else length(options$fixedFactors)

  if (method %in% c("trimmedMeans", "trimmedMeansBootstrap") && nFactors == 1) {
    anovaTable$addColumnInfo(name = "test", title = gettext("Test Statistic"), type = "number")
    if (method == "trimmedMeans") {
      anovaTable$addColumnInfo(name = "df1",  title = gettext("df1"),            type = "number")
      anovaTable$addColumnInfo(name = "df2",  title = gettext("df2"),            type = "number")
    }
    anovaTable$addColumnInfo(name = "p",    title = gettext("p"),              type = "pvalue")
    anovaTable$addColumnInfo(name = "effsize", title = gettext("\u03BE"),      type = "number")
    if (method == "trimmedMeansBootstrap")
      anovaTable$addColumnInfo(name = "varExplained", title = gettext("Var. Explained"), type = "number")

  } else if (nFactors >= 2) {
    anovaTable$addColumnInfo(name = "effect", title = gettext("Effect"),         type = "string")
    anovaTable$addColumnInfo(name = "test",   title = gettext("Test Statistic"), type = "number")
    anovaTable$addColumnInfo(name = "p",      title = gettext("p"),              type = "pvalue")

  } else {
    anovaTable$addColumnInfo(name = "test",    title = gettext("Test Statistic"), type = "number")
    anovaTable$addColumnInfo(name = "critVal", title = gettext("Critical Value"), type = "number")
    anovaTable$addColumnInfo(name = "p",       title = gettext("p"),              type = "pvalue")
  }

  jaspResults[["anovaRobustTable"]] <- anovaTable

  if (!ready) return()

  res <- results$result

  if (method == "trimmedMeans" && nFactors == 1) {
    anovaTable$addRows(list(test = res$test, df1 = res$df1, df2 = res$df2, p = res$p.value,
                           effsize = res$effsize))
    anovaTable$addFootnote(gettextf("Heteroscedastic one-way ANOVA for trimmed means (%.0f%% trimming).",
                                     options$trimProportion * 100))
    anovaTable$addFootnote(gettext("\u03BE denotes the explanatory measure of effect size."))

  } else if (method == "trimmedMeansBootstrap" && nFactors == 1) {
    anovaTable$addRows(list(test = res$test, p = res$p.value,
                           effsize = res$Effect.Size, varExplained = res$Var.Explained))
    anovaTable$addFootnote(gettextf(
      "Percentile t-bootstrap one-way ANOVA for trimmed means (%.0f%% trimming, %d bootstrap samples).",
      options$trimProportion * 100, options$bootstrapSamples
    ))
    anovaTable$addFootnote(gettext("\u03BE denotes the explanatory measure of effect size."))

  } else if (method == "medians" && nFactors == 1) {
    anovaTable$addRows(list(test = res$test, critVal = res$crit.val, p = res$p.value))
    anovaTable$addFootnote(gettext("Heteroscedastic one-way ANOVA for medians."))

  } else if (nFactors == 2) {
    factors <- unlist(options$fixedFactors)
    anovaTable$addRows(list(effect = factors[1], test = res$Qa,  p = res$A.p.value))
    anovaTable$addRows(list(effect = factors[2], test = res$Qb,  p = res$B.p.value))
    anovaTable$addRows(list(effect = paste(factors[1], "\u273B", factors[2]),
                            test = res$Qab, p = res$AB.p.value))
    if (method == "trimmedMeans") {
      anovaTable$addFootnote(gettextf("Two-way ANOVA for trimmed means (%.0f%% trimming).",
                                       options$trimProportion * 100))
    } else {
      anovaTable$addFootnote(gettext("Two-way ANOVA for medians."))
    }

  } else if (nFactors == 3) {
    factors <- unlist(options$fixedFactors)
    anovaTable$addRows(list(effect = factors[1], test = res$Qa,   p = res$A.p.value))
    anovaTable$addRows(list(effect = factors[2], test = res$Qb,   p = res$B.p.value))
    anovaTable$addRows(list(effect = factors[3], test = res$Qc,   p = res$C.p.value))
    anovaTable$addRows(list(effect = paste(factors[1], "\u273B", factors[2]),
                            test = res$Qab,  p = res$AB.p.value))
    anovaTable$addRows(list(effect = paste(factors[1], "\u273B", factors[3]),
                            test = res$Qac,  p = res$AC.p.value))
    anovaTable$addRows(list(effect = paste(factors[2], "\u273B", factors[3]),
                            test = res$Qbc,  p = res$BC.p.value))
    anovaTable$addRows(list(effect = paste(factors[1], "\u273B", factors[2], "\u273B", factors[3]),
                            test = res$Qabc, p = res$ABC.p.value))
    anovaTable$addFootnote(gettextf("Three-way ANOVA for trimmed means (%.0f%% trimming).",
                                     options$trimProportion * 100))
  }

  anovaTable$addCitation(gettext("Wilcox, R. (2012). Introduction to Robust Estimation and Hypothesis Testing (3rd ed.). Elsevier."))
}


# ---- Post Hoc table (ANOVA only, no covariates) ----

.ancovaRobustPostHocTable <- function(jaspResults, dataset, options, results, ready) {
  if (is.null(options$postHocTerms) || length(options$postHocTerms) == 0) return()
  if (!is.null(jaspResults[["postHocContainer"]])) return()

  postHocContainer <- createJaspContainer(title = gettext("Post Hoc Tests"))
  postHocContainer$dependOn(c(.ancovaRobustDeps, "postHocTerms",
                               "postHocCorrectionHochberg", "postHocCorrectionBonferroni",
                               "postHocCorrectionHolm", "postHocCi", "postHocCiLevel",
                               "postHocSignificanceFlag"))
  jaspResults[["postHocContainer"]] <- postHocContainer

  if (!ready) return()

  method <- results$method

  for (postHocTerm in options$postHocTerms) {
    termName <- postHocTerm
    if (is.list(postHocTerm))
      termName <- postHocTerm$components

    myTitle <- gettextf("Post Hoc Comparisons - %s", termName)
    postHocTable <- createJaspTable(title = myTitle)
    postHocTable$showSpecifiedColumnsOnly <- TRUE

    postHocTable$addColumnInfo(name = "contrast_A", title = " ",                  type = "string", combine = TRUE)
    postHocTable$addColumnInfo(name = "contrast_B", title = " ",                  type = "string")
    postHocTable$addColumnInfo(name = "estimate",   title = gettext("Difference"), type = "number")

    if (isTRUE(options$postHocCi)) {
      ciLevel <- options$postHocCiLevel
      ciTitle <- gettextf("%.0f%% CI", ciLevel * 100)
      postHocTable$addColumnInfo(name = "ciLower", title = gettext("Lower"), type = "number", overtitle = ciTitle)
      postHocTable$addColumnInfo(name = "ciUpper", title = gettext("Upper"), type = "number", overtitle = ciTitle)
    }

    corrections <- .ancovaRobustGetPostHocCorrections(options)
    for (corrName in names(corrections))
      postHocTable$addColumnInfo(name = corrName, title = corrections[[corrName]], type = "pvalue")

    if (isTRUE(options$postHocSignificanceFlag))
      postHocTable$addColumnInfo(name = "sig", title = " ", type = "string")

    postHocContainer[[termName]] <- postHocTable

    dependent <- options$dependent
    formula <- as.formula(paste0("`", dependent, "` ~ `", termName, "`"))
    tr <- options$trimProportion

    if (method == "trimmedMeans") {
      corrMethod <- if (isTRUE(options$postHocCorrectionHochberg)) "hochberg"
                    else if (isTRUE(options$postHocCorrectionHolm)) "holm"
                    else if (isTRUE(options$postHocCorrectionBonferroni)) "bonferroni"
                    else "hochberg"

      phResult <- try(WRS2::lincon(formula, data = dataset, tr = tr, method = corrMethod), silent = TRUE)

      if (isTryError(phResult)) {
        postHocTable$setError(gettextf("Could not compute post hoc comparisons for %s.", termName))
        next
      }

      comp   <- phResult$comp
      fnames <- phResult$fnames
      for (i in seq_len(nrow(comp))) {
        row <- list(
          contrast_A = fnames[comp[i, 1]],
          contrast_B = fnames[comp[i, 2]],
          estimate   = comp[i, "psihat"],
          ciLower    = comp[i, "ci.lower"],
          ciUpper    = comp[i, "ci.upper"]
        )

        for (corrName in names(corrections)) {
          corrM  <- sub("^p_", "", corrName)
          phCorr <- try(WRS2::lincon(formula, data = dataset, tr = tr, method = corrM), silent = TRUE)
          if (!isTryError(phCorr))
            row[[corrName]] <- phCorr$comp[i, "p.value"]
        }

        if (isTRUE(options$postHocSignificanceFlag)) {
          pVal <- comp[i, "p.value"]
          row$sig <- ifelse(pVal < 0.001, "***", ifelse(pVal < 0.01, "**", ifelse(pVal < 0.05, "*", "")))
        }

        postHocTable$addRows(row)
      }

    } else if (method == "trimmedMeansBootstrap") {
      phResult <- try(WRS2::mcppb20(formula, data = dataset, tr = tr, nboot = options$bootstrapSamples),
                      silent = TRUE)

      if (isTryError(phResult)) {
        postHocTable$setError(gettextf("Could not compute post hoc comparisons for %s.", termName))
        next
      }

      comp   <- phResult$comp
      fnames <- phResult$fnames
      for (i in seq_len(nrow(comp))) {
        row <- list(
          contrast_A = fnames[comp[i, 1]],
          contrast_B = fnames[comp[i, 2]],
          estimate   = comp[i, "psihat"],
          ciLower    = comp[i, "ci.lower"],
          ciUpper    = comp[i, "ci.upper"]
        )

        for (corrName in names(corrections))
          row[[corrName]] <- comp[i, "p-value"]

        if (isTRUE(options$postHocSignificanceFlag)) {
          pVal <- comp[i, "p-value"]
          row$sig <- ifelse(pVal < 0.001, "***", ifelse(pVal < 0.01, "**", ifelse(pVal < 0.05, "*", "")))
        }

        postHocTable$addRows(row)
      }

    } else if (method == "medians") {
      postHocTable$setError(gettext("Post hoc comparisons are not available for the medians method."))
    }
  }
}

.ancovaRobustGetPostHocCorrections <- function(options) {
  corrections <- list()
  if (isTRUE(options$postHocCorrectionHochberg))
    corrections[["p_hochberg"]]   <- gettext("p<sub>Hochberg</sub>")
  if (isTRUE(options$postHocCorrectionBonferroni))
    corrections[["p_bonferroni"]] <- gettext("p<sub>Bonf</sub>")
  if (isTRUE(options$postHocCorrectionHolm))
    corrections[["p_holm"]]       <- gettext("p<sub>Holm</sub>")
  return(corrections)
}


# ---- Descriptives Plots (shared) ----

.ancovaRobustDescriptivesPlots <- function(jaspResults, dataset, options, ready) {
  if (options$descriptivePlotHorizontalAxis == "" || !ready) return()
  if (!is.null(jaspResults[["descriptivesPlotContainer"]])) return()

  plotContainer <- createJaspContainer(title = gettext("Descriptives Plots"))
  plotContainer$dependOn(c("dependent", "fixedFactors", "covariates",
                            "descriptivePlotHorizontalAxis", "descriptivePlotSeparateLines",
                            "descriptivePlotSeparatePlot", "descriptivePlotErrorBar",
                            "descriptivePlotErrorBarType", "descriptivePlotCiLevel"))
  jaspResults[["descriptivesPlotContainer"]] <- plotContainer

  horizontalAxis <- options$descriptivePlotHorizontalAxis
  separateLines  <- if (options$descriptivePlotSeparateLines != "") options$descriptivePlotSeparateLines else NULL
  separatePlot   <- if (options$descriptivePlotSeparatePlot != "")  options$descriptivePlotSeparatePlot  else NULL

  dependent <- options$dependent
  plotErrorBars <- isTRUE(options$descriptivePlotErrorBar)
  ciLevel       <- options$descriptivePlotCiLevel

  # covariate on horizontal axis -> scatterplot (like regular ANCOVA)
  if (horizontalAxis %in% unlist(options$covariates)) {

    scatterOpts <- options
    scatterOpts[["colorPalette"]]                     <- "colorblind3"
    scatterOpts[["scatterPlotLegend"]]                <- TRUE
    scatterOpts[["scatterPlotRegressionLine"]]        <- TRUE
    scatterOpts[["scatterPlotRegressionLineCi"]]      <- plotErrorBars
    scatterOpts[["scatterPlotRegressionLineType"]]    <- "linear"
    scatterOpts[["scatterPlotGraphTypeAbove"]]        <- "none"
    scatterOpts[["scatterPlotGraphTypeRight"]]        <- "none"
    scatterOpts[["scatterPlotRegressionLineCiLevel"]] <- ciLevel

    if (!is.null(separatePlot) && separatePlot != "") {
      for (thisLevel in levels(dataset[[separatePlot]])) {
        subData <- dataset[dataset[[separatePlot]] == thisLevel, ]
        thisPlotName <- paste0(horizontalAxis, " - ", dependent, ": ", separatePlot, " = ", thisLevel)
        jaspDescriptives::.descriptivesScatterPlots(plotContainer, subData, c(horizontalAxis, dependent),
                                                    split = separateLines, options = scatterOpts, name = thisPlotName,
                                                    dependOnVariables = FALSE)
      }
    } else {
      jaspDescriptives::.descriptivesScatterPlots(plotContainer, dataset, c(horizontalAxis, dependent),
                                                  split = separateLines, options = scatterOpts, dependOnVariables = FALSE)
    }

    return()
  }

  # factor on horizontal axis -> grouped means plot
  groupVars <- c(horizontalAxis, separateLines, separatePlot)
  groupVars <- groupVars[!is.null(groupVars)]
  errorBarType  <- options$descriptivePlotErrorBarType

  summaryStat <- jaspTTests::.summarySE(as.data.frame(dataset), measurevar = dependent,
                                        groupvars = groupVars, conf.interval = ciLevel,
                                        na.rm = TRUE, .drop = FALSE, errorBarType = errorBarType)

  plotLevels <- if (!is.null(separatePlot)) levels(dataset[[separatePlot]]) else list(NULL)

  for (level in plotLevels) {
    plotTitle <- if (!is.null(level)) paste0(separatePlot, ": ", level) else ""
    subData  <- if (!is.null(level)) summaryStat[summaryStat[[separatePlot]] == level, ] else summaryStat

    plotObj <- createJaspPlot(title = plotTitle, width = 480, height = 320)

    p <- jaspGraphs::descriptivesPlot(
      x       = subData[[horizontalAxis]],
      y       = subData[[dependent]],
      ciLower = if (plotErrorBars) subData[[dependent]] - subData[["ci"]] else NULL,
      ciUpper = if (plotErrorBars) subData[[dependent]] + subData[["ci"]] else NULL,
      group   = if (!is.null(separateLines)) subData[[separateLines]] else NULL,
      xName   = horizontalAxis,
      yName   = dependent,
      groupName = separateLines
    )

    plotObj$plotObject <- p
    plotContainer[[paste0("plot_", level)]] <- plotObj
  }
}


# ---- Rain Cloud Plots (shared) ----

.ancovaRobustRainCloudPlots <- function(jaspResults, dataset, options, ready) {
  if (options$rainCloudHorizontalAxis == "" || !ready) return()
  if (!is.null(jaspResults[["rainCloudContainer"]])) return()

  rainCloudContainer <- createJaspContainer(title = gettext("Raincloud Plots"))
  rainCloudContainer$dependOn(c("dependent", "fixedFactors",
                                 "rainCloudHorizontalAxis", "rainCloudSeparatePlots",
                                 "rainCloudHorizontalDisplay"))
  jaspResults[["rainCloudContainer"]] <- rainCloudContainer

  horizontalAxis <- options$rainCloudHorizontalAxis
  separatePlot   <- if (!is.null(options$rainCloudSeparatePlots) && options$rainCloudSeparatePlots != "")
                      options$rainCloudSeparatePlots else NULL

  dependent <- options$dependent

  plotLevels <- if (!is.null(separatePlot)) levels(dataset[[separatePlot]]) else list(NULL)

  for (level in plotLevels) {
    plotTitle <- if (!is.null(level)) paste0(separatePlot, ": ", level) else ""
    plotData  <- if (!is.null(level)) dataset[dataset[[separatePlot]] == level, ] else dataset

    plotObj <- createJaspPlot(title = plotTitle, width = 480, height = 320)

    horiz <- if (!is.null(options$rainCloudHorizontalDisplay) && options$rainCloudHorizontalDisplay) TRUE else FALSE
    p <- try(jaspTTests::.descriptivesPlotsRainCloudFill(plotData, dependent, horizontalAxis, dependent, horizontalAxis, FALSE, horiz, NULL))
    if (isTryError(p))
      plotObj$setError(.extractErrorMessage(p))
    else
      plotObj$plotObject <- p
    rainCloudContainer[[paste0("rainCloud_", level)]] <- plotObj
  }
}
