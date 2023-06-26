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

  # galileoPlotDataAndModelPredictions(jaspResults, dataset, options, modelObject)


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
  modelTable$addColumnInfo(name = "model", title = gettext("Model"),        type = "string")
  modelTable$addColumnInfo(name = "BF",    title = gettext("Bayes factor"), type = "number")

  jaspResults[["modelTable"]] <- modelTable

}

galileoFillModelTable <- function(jaspResults, dataset, options, modelObject) {

  maxDegree <- options[["maxDegree"]]

  basObject <- modelObject$basObject

  modelNames <- degreeToName(seq_len(maxDegree + 1))
  modelWhich <- vapply(0:maxDegree, function(i) {
    for (j in seq_along(basObject$which)) {
      if (identical(basObject$which[[j]], 0:i))
        return(j)
    }
  }, FUN.VALUE = integer(1L))

  postprobs  <- basObject$postprobs[modelWhich]
  priorprobs <- basObject$priorprobs[modelWhich]

  postprobs  <- postprobs  / sum(postprobs)
  priorprobs <- priorprobs / sum(priorprobs)

  bestModelIdx <- which.max(postprobs)
  BFs          <- (postprobs / postprobs[bestModelIdx]) / (priorprobs / priorprobs[bestModelIdx])

  BFs <- jaspBase::.recodeBFtype(BFs, newBFtype = options[["bayesFactorType"]], oldBFtype = "BF10")

  jaspResults[["modelTable"]]$setData(data.frame(
    model = modelNames,
    BF    = BFs
  ))

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

  return(values[index])
}
