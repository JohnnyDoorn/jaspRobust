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

.rmRobustDependentName <- "JaspColumn_.dependent._Encoded"
.rmRobustSubjectName   <- "JaspColumn_.subject._Encoded"

.rmRobustDeps <- c("repeatedMeasuresCells", "repeatedMeasuresFactors",
                    "betweenSubjectFactors", "robustMethod",
                    "trimProportion", "bootstrapSamples")

AnovaRepeatedMeasuresInternal <- function(jaspResults, dataset, options) {
  ready <- all(options$repeatedMeasuresCells != "")


  cat(
      "\ndataset colnames:", paste(names(dataset), collapse = ", "),
      "\n=== END DEBUG ===\n\n")


  # Convert wide to long format
  longData <- .rmRobustReadData(dataset, options, ready)
  if (isTryError(longData))
    .quitAnalysis(gettextf("Error while loading data: %s", .extractErrorMessage(longData)))

  .rmRobustCheckErrors(longData, dataset, options, ready)

  rmResults <- .rmRobustComputeResults(jaspResults, longData, dataset, options, ready)

  .rmRobustTableMain(jaspResults, options, rmResults, ready)

  .rmRobustPostHocTable(jaspResults, longData, options, rmResults, ready)

  .rmRobustDescriptivesPlots(jaspResults, longData, options, ready)

  .rmRobustRainCloudPlots(jaspResults, longData, options, ready)
}

# Entry point for JASP (func: "AnovaRepeatedMeasuresRobust" in Description.qml)
AnovaRepeatedMeasuresRobustInternal <- AnovaRepeatedMeasuresInternal


# ---- Wide to Long conversion ----

.rmRobustReadData <- function(dataset, options, ready) {
  if (!ready)
    return(dataset)

  rm.vars    <- unlist(options$repeatedMeasuresCells)
  bs.factors <- unlist(options$betweenSubjectFactors)
  bs.factors <- bs.factors[nzchar(bs.factors)]
  rm.factors <- options$repeatedMeasuresFactors

  keys <- "repeatedMeasuresCells"
  if (length(bs.factors) > 0)
    keys <- c(keys, "betweenSubjectFactors")

  dataset <- readDataSetByVariableTypes(options, keys = keys,
                                        exclude.na.listwise = c(rm.vars, bs.factors))



  longData <- try(
    .shortToLong(dataset, rm.factors, rm.vars, bs.factors,
                 dependentName = .rmRobustDependentName, subjectName = .rmRobustSubjectName),
    silent = TRUE
  )

  return(longData)
}


# ---- Error checking ----

.rmRobustCheckErrors <- function(longData, dataset, options, ready) {
  if (!ready) return()

  nRMFactors <- length(options$repeatedMeasuresFactors)
  nBSFactors <- length(options$betweenSubjectFactors)

  if (nRMFactors > 1)
    .quitAnalysis(gettext("Robust repeated measures ANOVA currently supports only one within-subjects factor."))

  if (nBSFactors > 1)
    .quitAnalysis(gettext("Robust repeated measures ANOVA currently supports at most one between-subjects factor."))
}


# ---- Compute results ----

.rmRobustComputeResults <- function(jaspResults, longData, dataset, options, ready) {
  if (!ready) return()
  if (!is.null(jaspResults[["stateRMRobustResults"]])) return(jaspResults[["stateRMRobustResults"]]$object)

  method    <- options$robustMethod
  tr        <- options$trimProportion
  nboot     <- options$bootstrapSamples
  hasBetween <- length(options$betweenSubjectFactors) > 0

  depVar      <- .rmRobustDependentName
  subjectVar  <- .rmRobustSubjectName

  # Get the single RM factor name from the long data
  rmFactorName <- options$repeatedMeasuresFactors[[1]]$name

  if (hasBetween) {
    # Mixed design: between + within — use bwtrim / sppba+sppbb+sppbi
    bsFactor <- options$betweenSubjectFactors[[1]]
    formula  <- as.formula(paste0("`", depVar, "` ~ `", bsFactor, "` * `", rmFactorName, "`"))

    if (method == "trimmedMeansBootstrap") {
      resultA  <- try(WRS2::sppba(formula, id = subjectVar, data = longData, tr = tr, nboot = nboot), silent = TRUE)
      resultB  <- try(WRS2::sppbb(formula, id = subjectVar, data = longData, tr = tr, nboot = nboot), silent = TRUE)
      resultAB <- try(WRS2::sppbi(formula, id = subjectVar, data = longData, tr = tr, nboot = nboot), silent = TRUE)

      errors <- c()
      if (isTryError(resultA))  errors <- c(errors, paste("Between-subjects:", .rmRobustExtractError(resultA)))
      if (isTryError(resultB))  errors <- c(errors, paste("Within-subjects:",  .rmRobustExtractError(resultB)))
      if (isTryError(resultAB)) errors <- c(errors, paste("Interaction:",      .rmRobustExtractError(resultAB)))

      if (length(errors) > 0)
        .quitAnalysis(gettextf("Bootstrap mixed RM ANOVA failed:\n%s", paste(errors, collapse = "\n")))

      result <- list(between = resultA, within = resultB, interaction = resultAB)
    } else {
      result <- try(WRS2::bwtrim(formula, id = subjectVar, data = longData, tr = tr), silent = TRUE)
      if (isTryError(result))
        .quitAnalysis(gettextf("Mixed RM ANOVA failed: %s", .rmRobustExtractError(result)))
    }

    designType <- "mixed"
  } else {
    # Pure within-subjects design — use rmanova / rmanovab
    y      <- longData[[depVar]]
    groups <- longData[[rmFactorName]]
    blocks <- longData[[subjectVar]]

    if (method == "trimmedMeansBootstrap") {
      result <- try(WRS2::rmanovab(y = y, groups = groups, blocks = blocks, tr = tr, nboot = nboot), silent = TRUE)
    } else {
      result <- try(WRS2::rmanova(y = y, groups = groups, blocks = blocks, tr = tr), silent = TRUE)
    }

    if (isTryError(result))
      .quitAnalysis(gettextf("Robust RM ANOVA failed: %s", .rmRobustExtractError(result)))

    designType <- "within"
  }

  results <- list(result = result, method = method, designType = designType)

  state <- createJaspState(results)
  state$dependOn(.rmRobustDeps)
  jaspResults[["stateRMRobustResults"]] <- state

  return(results)
}


.rmRobustExtractError <- function(tryResult) {
  trimws(gsub("^Error.*?:\\s*", "", as.character(tryResult)))
}


# ---- Main table ----

.rmRobustTableMain <- function(jaspResults, options, rmResults, ready) {
  if (!is.null(jaspResults[["rmRobustTable"]])) return()

  rmTable <- createJaspTable(title = gettext("Robust Repeated Measures ANOVA"))
  rmTable$dependOn(.rmRobustDeps)
  rmTable$showSpecifiedColumnsOnly <- TRUE

  rmTable$addColumnInfo(name = "effect", title = gettext("Effect"),         type = "string")
  rmTable$addColumnInfo(name = "test",   title = gettext("Test Statistic"), type = "number")
  rmTable$addColumnInfo(name = "df1",    title = gettext("df1"),            type = "number")
  rmTable$addColumnInfo(name = "df2",    title = gettext("df2"),            type = "number")
  rmTable$addColumnInfo(name = "p",      title = gettext("p"),              type = "pvalue")

  jaspResults[["rmRobustTable"]] <- rmTable

  if (!ready || is.null(rmResults)) return()

  method     <- rmResults$method
  designType <- rmResults$designType
  res        <- rmResults$result

  if (designType == "within") {
    # rmanova / rmanovab: single row
    rmFactorName <- options$repeatedMeasuresFactors[[1]]$name
    rmTable$addRows(list(
      effect = rmFactorName,
      test   = res$test,
      df1    = if (!is.null(res$df1)) res$df1 else NA,
      df2    = if (!is.null(res$df2)) res$df2 else NA,
      p      = res$p.value
    ))

  } else if (designType == "mixed") {
    bsFactor     <- options$betweenSubjectFactors[[1]]
    rmFactorName <- options$repeatedMeasuresFactors[[1]]$name

    if (method == "trimmedMeansBootstrap") {
      # sppba/sppbb/sppbi results
      .rmRobustAddBootstrapMixedRow(rmTable, res$between,     bsFactor)
      .rmRobustAddBootstrapMixedRow(rmTable, res$within,      rmFactorName)
      .rmRobustAddBootstrapMixedRow(rmTable, res$interaction, paste(bsFactor, "\u273B", rmFactorName))
    } else {
      # bwtrim result: Qa (between), Qb (within), Qab (interaction)
      rmTable$addRows(list(
        effect = bsFactor,
        test   = res$Qa, df1 = res$A.df1, df2 = res$A.df2, p = res$A.p.value
      ))
      rmTable$addRows(list(
        effect = rmFactorName,
        test   = res$Qb, df1 = res$B.df1, df2 = res$B.df2, p = res$B.p.value
      ))
      rmTable$addRows(list(
        effect = paste(bsFactor, "\u273B", rmFactorName),
        test   = res$Qab, df1 = res$AB.df1, df2 = res$AB.df2, p = res$AB.p.value
      ))
    }
  }

  if (method == "trimmedMeansBootstrap") {
    rmTable$addFootnote(gettextf(
      "Bootstrap with trimmed means (%.0f%% trimming, %d bootstrap samples).",
      options$trimProportion * 100, options$bootstrapSamples
    ))
  } else {
    rmTable$addFootnote(gettextf("Trimmed means (%.0f%% trimming).",
                                  options$trimProportion * 100))
  }

  rmTable$addCitation(gettext("Wilcox, R. (2012). Introduction to Robust Estimation and Hypothesis Testing (3rd ed.). Elsevier."))
}


.rmRobustAddBootstrapMixedRow <- function(table, res, effectName) {
  table$addRows(list(
    effect = effectName,
    test   = if (!is.null(res$test))    res$test    else NA,
    df1    = if (!is.null(res$df1))     res$df1     else NA,
    df2    = if (!is.null(res$df2))     res$df2     else NA,
    p      = if (!is.null(res$p.value)) res$p.value else NA
  ))
}


# ---- Post Hoc table ----

.rmRobustPostHocTable <- function(jaspResults, longData, options, rmResults, ready) {
  if (is.null(options$postHocTerms) || length(options$postHocTerms) == 0 || !ready || is.null(rmResults)) return()
  if (!is.null(jaspResults[["rmRobustPostHocContainer"]])) return()

  postHocContainer <- createJaspContainer(title = gettext("Post Hoc Tests"))
  postHocContainer$dependOn(c(.rmRobustDeps, "postHocTerms",
                               "postHocCorrectionHochberg", "postHocCorrectionBonferroni",
                               "postHocCorrectionHolm"))
  jaspResults[["rmRobustPostHocContainer"]] <- postHocContainer

  method     <- rmResults$method
  designType <- rmResults$designType
  tr         <- options$trimProportion
  nboot      <- options$bootstrapSamples

  depVar      <- .rmRobustDependentName
  subjectVar  <- .rmRobustSubjectName
  rmFactorName <- options$repeatedMeasuresFactors[[1]]$name

  for (postHocTerm in options$postHocTerms) {
    variables <- unlist(postHocTerm$components)
    termName  <- paste(variables, collapse = " \u273B ")

    myTitle <- gettextf("Post Hoc Comparisons - %s", termName)
    postHocTable <- createJaspTable(title = myTitle)
    postHocTable$showSpecifiedColumnsOnly <- TRUE

    postHocTable$addColumnInfo(name = "contrast_A", title = " ",                      type = "string", combine = TRUE)
    postHocTable$addColumnInfo(name = "contrast_B", title = " ",                      type = "string")
    postHocTable$addColumnInfo(name = "psihat",     title = gettext("\u0394 Trimmed Mean"), type = "number")
    postHocTable$addColumnInfo(name = "ciLower",    title = gettext("Lower"),          type = "number",
                               overtitle = gettext("95% CI"))
    postHocTable$addColumnInfo(name = "ciUpper",    title = gettext("Upper"),          type = "number",
                               overtitle = gettext("95% CI"))
    postHocTable$addColumnInfo(name = "p",          title = gettext("p"),              type = "pvalue")

    postHocContainer[[termName]] <- postHocTable

    # Post hoc only available for within-subjects factor
    isWithinFactor <- length(variables) == 1 && variables[1] == rmFactorName

    if (!isWithinFactor) {
      postHocTable$addFootnote(gettext("Robust post hoc tests are only available for within-subjects factors."))
      next
    }

    y      <- longData[[depVar]]
    groups <- longData[[rmFactorName]]
    blocks <- longData[[subjectVar]]

    if (method == "trimmedMeansBootstrap") {
      phResult <- try(WRS2::pairdepb(y = y, groups = groups, blocks = blocks, tr = tr, nboot = nboot), silent = TRUE)
    } else {
      phResult <- try(WRS2::rmmcp(y = y, groups = groups, blocks = blocks, tr = tr), silent = TRUE)
    }

    if (isTryError(phResult)) {
      postHocTable$setError(gettextf("Post hoc tests failed: %s", .rmRobustExtractError(phResult)))
      next
    }

    # rmmcp/pairdepb return a list with comp matrix: columns [Group1, Group2, psihat, ci.lower, ci.upper, p.value]
    compMatrix <- phResult$comp
    lvls <- levels(groups)

    for (i in seq_len(nrow(compMatrix))) {
      g1 <- as.integer(compMatrix[i, 1])
      g2 <- as.integer(compMatrix[i, 2])
      row <- list(
        contrast_A = lvls[g1],
        contrast_B = lvls[g2],
        psihat     = compMatrix[i, 3],
        ciLower    = compMatrix[i, 4],
        ciUpper    = compMatrix[i, 5],
        p          = compMatrix[i, 6]
      )
      postHocTable$addRows(row)
    }

    # Apply p-value corrections
    rawP <- compMatrix[, 6]
    .rmRobustAddCorrectedPValues(postHocTable, rawP, options)
  }
}


.rmRobustAddCorrectedPValues <- function(postHocTable, rawP, options) {
  corrections <- list()
  if (isTRUE(options$postHocCorrectionHochberg))
    corrections[["hochberg"]] <- list(label = gettext("p<sub>Hochberg</sub>"), values = p.adjust(rawP, method = "hochberg"))
  if (isTRUE(options$postHocCorrectionBonferroni))
    corrections[["bonferroni"]] <- list(label = gettext("p<sub>Bonf</sub>"), values = p.adjust(rawP, method = "bonferroni"))
  if (isTRUE(options$postHocCorrectionHolm))
    corrections[["holm"]] <- list(label = gettext("p<sub>Holm</sub>"), values = p.adjust(rawP, method = "holm"))

  for (corrName in names(corrections)) {
    postHocTable$addColumnInfo(name = corrName, title = corrections[[corrName]]$label, type = "pvalue")
    for (i in seq_along(corrections[[corrName]]$values))
      postHocTable$addRows(setNames(list(corrections[[corrName]]$values[i]), corrName), rowIndex = i)
  }
}


# ---- Descriptives Plots ----

.rmRobustDescriptivesPlots <- function(jaspResults, longData, options, ready) {
  if (options$descriptivePlotHorizontalAxis == "" || !ready) return()
  if (!is.null(jaspResults[["rmRobustDescriptivesPlotContainer"]])) return()

  plotContainer <- createJaspContainer(title = gettext("Descriptives Plots"))
  plotContainer$dependOn(c("repeatedMeasuresCells", "repeatedMeasuresFactors",
                            "betweenSubjectFactors", "descriptivePlotHorizontalAxis",
                            "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot",
                            "descriptivePlotErrorBar", "descriptivePlotErrorBarType",
                            "descriptivePlotCiLevel"))
  jaspResults[["rmRobustDescriptivesPlotContainer"]] <- plotContainer

  horizontalAxis <- options$descriptivePlotHorizontalAxis
  separateLines  <- if (options$descriptivePlotSeparateLines != "") options$descriptivePlotSeparateLines else NULL
  separatePlot   <- if (options$descriptivePlotSeparatePlot != "")  options$descriptivePlotSeparatePlot  else NULL

  depVar <- .rmRobustDependentName
  groupVars <- c(horizontalAxis, separateLines, separatePlot)
  groupVars <- groupVars[!is.null(groupVars)]

  plotErrorBars <- isTRUE(options$descriptivePlotErrorBar)
  errorBarType  <- options$descriptivePlotErrorBarType
  ciLevel       <- options$descriptivePlotCiLevel

  summaryStat <- jaspTTests::.summarySE(as.data.frame(longData), measurevar = depVar,
                                        groupvars = groupVars, conf.interval = ciLevel,
                                        na.rm = TRUE, .drop = FALSE, errorBarType = errorBarType)

  plotLevels <- if (!is.null(separatePlot)) unique(longData[[separatePlot]]) else list(NULL)

  for (level in plotLevels) {
    plotTitle <- if (!is.null(level)) paste0(separatePlot, ": ", level) else ""
    subData  <- if (!is.null(level)) summaryStat[summaryStat[[separatePlot]] == level, ] else summaryStat

    plotObj <- createJaspPlot(title = plotTitle, width = 480, height = 320)

    p <- jaspGraphs::descriptivesPlot(
      x       = subData[[horizontalAxis]],
      y       = subData[[depVar]],
      ciLower = if (plotErrorBars) subData[[depVar]] - subData[["ci"]] else NULL,
      ciUpper = if (plotErrorBars) subData[[depVar]] + subData[["ci"]] else NULL,
      group   = if (!is.null(separateLines)) subData[[separateLines]] else NULL,
      xName   = horizontalAxis,
      yName   = "Dependent",
      groupName = separateLines
    )

    plotObj$plotObject <- p
    plotContainer[[paste0("plot_", level)]] <- plotObj
  }
}


# ---- Rain Cloud Plots ----

.rmRobustRainCloudPlots <- function(jaspResults, longData, options, ready) {
  if (options$rainCloudHorizontalAxis == "" || !ready) return()
  if (!is.null(jaspResults[["rmRobustRainCloudContainer"]])) return()

  rainCloudContainer <- createJaspContainer(title = gettext("Raincloud Plots"))
  rainCloudContainer$dependOn(c("repeatedMeasuresCells", "repeatedMeasuresFactors",
                                 "betweenSubjectFactors", "rainCloudHorizontalAxis",
                                 "rainCloudSeparatePlots"))
  jaspResults[["rmRobustRainCloudContainer"]] <- rainCloudContainer

  horizontalAxis <- options$rainCloudHorizontalAxis
  separatePlot   <- if (!is.null(options$rainCloudSeparatePlots) && options$rainCloudSeparatePlots != "")
                      options$rainCloudSeparatePlots else NULL

  depVar <- .rmRobustDependentName

  plotLevels <- if (!is.null(separatePlot)) unique(longData[[separatePlot]]) else list(NULL)

  for (level in plotLevels) {
    plotTitle <- if (!is.null(level)) paste0(separatePlot, ": ", level) else ""
    plotData  <- if (!is.null(level)) longData[longData[[separatePlot]] == level, ] else longData

    plotObj <- createJaspPlot(title = plotTitle, width = 480, height = 320)

    addLines <- !(horizontalAxis %in% unlist(options[["betweenSubjectFactors"]]))
    horiz <- if (!is.null(options$rainCloudHorizontalDisplay) && options$rainCloudHorizontalDisplay) TRUE else FALSE
    p <- try(jaspTTests::.descriptivesPlotsRainCloudFill(plotData, depVar, horizontalAxis, depVar, horizontalAxis, addLines, horiz, NULL))
    if (isTryError(p))
      plotObj$setError(.extractErrorMessage(p))
    else
      plotObj$plotObject <- p
    rainCloudContainer[[paste0("rainCloud_", level)]] <- plotObj
  }
}
