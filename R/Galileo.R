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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

#' @import jaspBase

#' @export
Galileo <- function(jaspResults, dataset, options) {

  dataset <- if (galileoIsReady(options)) .readDataSetToEnd(columns = c(options[["dependent"]], options[["covariate"]]))

  # TODO:
  # checkErrors(options, dataset)

  modelObject <- galileoModelTable(jaspResults, dataset, options)

  galileoDataAndModelPredictionsPlot(jaspResults, dataset, options, modelObject)

}

galileoIsReady <- function(options) {
  options[["dependent"]] != "" && options[["covariate"]] != ""
}

galileoModelTable <- function(jaspResults, dataset, options) {

  recomputeTable <- is.null(jaspResults[["modelTable"]])
  if (recomputeTable)
    galileoInitializeModelTable(jaspResults, options)

  if (!galileoIsReady(options))
    return()

  modelObject <- jaspResults[["modelObject"]] %setOrRetrieve% (
    galileoFitModels(jaspResults, dataset, options) |>
      createJaspState(dependencies = jaspDeps(c("dependent", "covariate")))
  )

  if (recomputeTable)
    galileoFillModelTable(jaspResults, dataset, options, modelObject)

  return(modelObject)
}

galileoInitializeModelTable <- function(jaspResults, options) {

  # TODO: dependencies!
  modelTable <- createJaspTable(title = gettext("Models"))
  modelTable$dependOn(options = c("dependent", "covariate", "bayesFactorType", "bayesFactorReferenceModel", "specificReferenceModel"))

  bfTitle <- galileoGetBfTitle(options[["bayesFactorType"]], options[["bayesFactorReferenceModel"]], options[["specificReferenceModel"]])

  modelTable$addColumnInfo(name = "model",              title = gettext("Model"), type = "string")
  modelTable$addColumnInfo(name = "priorModelProb",     title = "P(M)",           type = "number")
  modelTable$addColumnInfo(name = "posteriorModelProb", title = "P(M|data)",      type = "number")
  modelTable$addColumnInfo(name = "bayesFactor",        title = bfTitle,          type = "number")

  jaspResults[["modelTable"]] <- modelTable

}

galileoFillModelTable <- function(jaspResults, dataset, options, modelObject) {

  maxDegree <- options[["maxDegree"]]

  basObject <- modelObject$basObject

  modelWhich <- extractModelIndices(basObject, maxDegree)

  postprobs  <- basObject$postprobs[modelWhich]
  priorprobs <- basObject$priorprobs[modelWhich]

  # renormalize since we excluded some models
  postprobs  <- postprobs  / sum(postprobs)
  priorprobs <- priorprobs / sum(priorprobs)

  referenceModelIndex <- if (options[["bayesFactorReferenceModel"]] == "bestModel") {
    which.max(postprobs) # only valid because we assume the modelprior is uniform
  } else {
    min(length(postprobs), as.integer(options[["specificReferenceModel"]]) + 1L)
  }

  BF10s <- (postprobs / postprobs[referenceModelIndex]) / (priorprobs / priorprobs[referenceModelIndex])

  BFs <- jaspBase::.recodeBFtype(BF10s, newBFtype = options[["bayesFactorType"]], oldBFtype = "BF10")

  modelNames <- degreeToName(seq_len(maxDegree + 1))

  tableData <- data.frame(
    model              = modelNames,
    priorModelProb     = priorprobs,
    posteriorModelProb = postprobs,
    bayesFactor        = BFs
  )

  jaspResults[["modelTable"]]$setData(tableData)

}

galileoFitModels <- function(jaspResults, dataset, options) {

  # TODO: figure out all common models and set inclusion probabilities to one
  fullModelFormula <- as.formula(sprintf("%s ~ poly(%s, %d)",
                                         options[["dependent"]], options[["covariate"]], options[["maxDegree"]]))

  basObject <- BAS::bas.lm(
    formula    = fullModelFormula,
    data       = dataset,
    # TODO: make these two adjustable!
    prior      = "JZS",
    modelprior = BAS::uniform()
  )

  # TODO: probable we want do renormalize here, no?

  return(list(
    basObject = basObject
  ))
}

galileoDataAndModelPredictionsPlot <- function(jaspResults, dataset, options, modelObject) {

  if (!options[["dataAndModelPredictionsPlot"]])
    return()

  jaspResults[["dataAndModelPredictionsPlot"]] %setOrRetrieve% (
    galileoFillDataAndModelPredictionsPlot(
      dataset           = dataset,
      dependent         = options[["dependent"]],
      covariate         = options[["covariate"]],
      coefficientsList  = galileoExtractCoefficientsList(modelObject, options[["maxDegree"]]),
      colorPalette      = options[["colorPalette"]]
    ) |>
      createJaspPlot(
        title        = gettext("Scatter Plot with Model predictions"),
        dependencies = c("dependent", "covariate", "dataAndModelPredictionsPlot", "colorPalette"),
        width  = 480,
        height = 360
      )
  )
}

galileoExtractCoefficientsList <- function(modelObject, maxDegree) {

  basObject <- modelObject[["basObject"]]
  modelWhich <- extractModelIndices(basObject, maxDegree)

  return(basObject[["mle"]][modelWhich])

}

galileoFillDataAndModelPredictionsPlot <- function(dataset, dependent, covariate, coefficientsList, colorPalette) {

  if (length(coefficientsList) == 0L)
    return(NULL)

  xBreaks <- pretty(dataset[[covariate]])
  xLimits <- range(xBreaks)
  xValues <- seq(xLimits[1L], xLimits[2L], length.out = 2048L)

  yValues <- do.call(cbind, lapply(coefficientsList, evaluate_polynomial, x = xValues))

  df <- data.frame(
    x     = rep(xValues, maxOrder),
    y     = c(yValues),
    order = rep(factor(seq_len(maxOrder)), each = length(xValues))
  )

  # ggplot2, sigh...
  dependentSym <- rlang::sym(dependent)
  covariateSym <- rlang::sym(covariate)
  pointMapping <- ggplot2::aes(x = !!covariateSym, y = !!dependentSym)

  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = x, y = y, group = order, color = order), inherit.aes = FALSE) +
    jaspGraphs::geom_point(data = dataset, mapping = pointMapping) +
    jaspGraphs::scale_JASPcolor_discrete(palette = colorPalette) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

  return(plot)

}

# helpers ----
evaluate_polynomial <- function(x, coefs) {

  sum <- numeric(length(x))
  for(i in 0:(length(coefs) - 1))
    sum <- sum + coefs[i+1]*x^i

  sum
}

extractModelIndices <- function(basObject, maxDegree) {
  indices <- vapply(0:maxDegree, function(i) {
    for (j in seq_along(basObject[["which"]])) {
      if (identical(basObject[["which"]][[j]], 0:i))
        return(j)
    }
  }, FUN.VALUE = integer(1L))
  indices[order(lengths(basObject[["which"]][indices]))]
}

degreeToName <- function(index) {

  # index with degree + 1
  values <- c(
    gettext("constant"),
    gettext("linear"),
    gettext("quadratic"),
    gettext("cubic"),
    gettext("quartic"),
    gettext("quintic"),
    gettext("sextic"),
    gettext("septic"),
    gettext("octic"),
    gettext("nonic"),
    gettext("decic")
  )

  if (missing(index))
    return(values)

  if (is.character(index))
    index <- as.integer(index)

  return(values[index])
}

galileoGetBfTitle <- function(bayesFactorType, referenceModel, specificReferenceModel) {
  if (referenceModel == "bestModel") {
    bfTitle <- switch(
      bayesFactorType,
      "BF10"      = gettext("BF<sub>1b</sub>"),
      "BF01"      = gettext("BF<sub>b1</sub>"),
      "Log(BF10)" = gettext("Log(BF<sub>1b</sub>)")
    )
  } else if (referenceModel == "bestModel") {
    bfTitle <- switch(
      bayesFactorType,
      "BF10"      = gettext("BF<sub>10</sub>"),
      "BF01"      = gettext("BF<sub>01</sub>"),
      "Log(BF10)" = gettext("Log(BF<sub>10</sub>)")
    )
  } else {
    name <- degreeToName(as.integer(specificReferenceModel) + 1L)
    bfTitle <- switch(
      bayesFactorType,
      "BF10"      = gettextf("BF<sub>1, %s</sub>",      name),
      "BF01"      = gettextf("BF<sub>%s, 1</sub>",      name),
      "Log(BF10)" = gettextf("Log(BF<sub>1, %s</sub>)", name)
    )
  }
  return(bfTitle)
}
