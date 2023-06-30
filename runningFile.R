library(BAS)
mround <- function(x, base) base * round(x / base)



data(galileo, package = "UsingR")

polynomial = function(x,coefs, raw = FALSE) {
  if (inherits(coefs, "lm"))
    coefs <- coef(coefs)

  z <- cbind(1, poly(x, length(coefs) - 1))
  tcrossprod(z, coefs)
  dim(z)
  length(coefs)
  coefs %*% z
  sum(coefs * z[1, ])
  sum(coefs * z[2, ])
  coefs %*% z

  sum <- 0
  for(i in 0:(length(coefs)-1)) {
    sum <- sum + coefs[i+1]*x^i
  }
  sum
}

maxDegree <- 3L

resLm <- vector("list", maxDegree + 1)
resLm[[1]] <- lm(h.d ~ 1, data = galileo)
for (i in 1:maxDegree) {
  fNew <- as.formula(sprintf("h.d ~ poly(init.h, %d, raw = TRUE)", i))
  resLm[[i + 1]] <- lm(fNew, data = galileo)
}


xBreaks <- pretty(galileo$init.h)
xLimits <- range(xBreaks)
xValues <- seq(xLimits[1L], xLimits[2L], length.out = 2^10)

yValues <- do.call(cbind, lapply(resLm, polynomial, x = xValues))



df <- data.frame(
    x = rep(xValues, maxDegree + 1L),
    y = c(yValues),
    order = rep(factor(seq_len(maxDegree + 1L)), each = length(xValues))
)

ggplot2::ggplot() +
  ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = x, y = y, group = order, color = order), inherit.aes = FALSE) +
  jaspGraphs::geom_point(data = galileo, mapping = ggplot2::aes(x = init.h, y = h.d)) +
  jaspGraphs::geom_rangeframe() +
  jaspGraphs::themeJaspRaw(legend.position = "right")

raw <- FALSE
fullModel <- h.d ~ poly(init.h, maxDegree, raw = raw)

basObject <- BAS::bas.lm(
  formula    = fullModel,
  data       = galileo,
  prior      = "JZS",
  modelprior = BAS::uniform()
)

# dput(names(which(lengths(basObject) == 8)))
namesToSubset <- c("which", "logmarg", "postprobs", "priorprobs", "sampleprobs",
                   "mse", "mle", "mle.se", "shrinkage", "size", "R2", "rank", "postprobs.RN",
                   "df")
idx <- extractModelIndices(basObject, maxDegree)

for (nm in namesToSubset)
  basObject[[nm]] <- basObject[[nm]][idx]

basObject$postprobs  <- basObject$postprobs  / sum(basObject$postprobs)
basObject$priorprobs <- basObject$priorprobs / sum(basObject$priorprobs)

posteriorInclProb <- rev(cumsum(rev(basObject$postprobs)))
priorInclProb     <- rev(cumsum(rev(basObject$priorprobs)))
inclBF            <- (posteriorInclProb / (1 - posteriorInclProb)) / (priorInclProb / (1 - priorInclProb))
inclBF[1]         <- NA_real_ # no evidence

debugonce(BAS:::predict.bas)
newData        <- data.frame(init.h = xValues)
predictionsObj <- predict(basObject, newdata = newData, estimator = "BMA", top = length(basObject[["which"]]), se.fit = TRUE)
predictionsObj$Ypred


df <- data.frame(
  x = rep(xValues, maxDegree + 1L),
  y = c(t(predictionsObj[["Ypred"]])),
  order = rep(factor(seq_len(maxDegree + 1L)), each = length(xValues))
)

ggplot2::ggplot() +
  ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = x, y = y, group = order, color = order), inherit.aes = FALSE) +
  jaspGraphs::geom_point(data = galileo, mapping = ggplot2::aes(x = init.h, y = h.d)) +
  jaspGraphs::geom_rangeframe() +
  jaspGraphs::themeJaspRaw(legend.position = "right")



basObject$postprobs[3] + basObject$postprobs[4]


cumsum(basObject$postprobs)



basObject$X[, -1] == resLm[[5]]$model[, -1]

basObject$which <- basObject$which[idx]
basObject$mle[idx]
lapply(resLm, \(x) unname(coef(x)))

coefMat <- matrix(0, length(idx), maxDegree + 1L)
for (i in seq_along(idx)) {
  coefs <- basObject$mle[[idx[i]]]
  coefMat[i, seq_along(coefs)] <- coefs
}


xBreaks <- pretty(galileo$init.h)
xLimits <- range(xBreaks)
xValues <- seq(xLimits[1L], xLimits[2L], length.out = 2^10)

coefsList <- galileoExtractCoefficientsList(list(basObject = basObject), maxDegree)
coefs <- coefsList[[4]]
z <- poly(galileo$init.h, degree = maxDegree, raw = raw)
coefs[1] + scale(z, scale = FALSE) %*% coefs[-1]

ee <- basObject
ee$pos
predict()

debugonce(BAS:::predict.bas)

predict(basObject, estimator = "HPM")$fit

newData <- model.matrix(~ 0 + poly(x, maxDegree, raw = raw), data.frame(x = galileo$init.h))
newData <- sweep(newData, 2L, colMeans(newData))
(newData %*% coefs[-1]) * basObject$shrinkage[idx[length(idx)]] + coefs[1]




model.matrix(~ 0 + poly(x, 3), data.frame(x = 1:5))
model.matrix(~ 1 + poly(x, 3), data.frame(x = 1:5))

basObject
(basObject$X %*% coefs) * basObject$shrinkage
yValues <- do.call(cbind, lapply(resLm, polynomial, x = xValues, raw = raw))
debugonce(galileoExtractCoefficientsList)

df <- data.frame(
  x = rep(xValues, maxDegree + 1L),
  y = c(yValues),
  order = rep(factor(seq_len(maxDegree + 1L)), each = length(xValues))
)

ggplot2::ggplot() +
  ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = x, y = y, group = order, color = order), inherit.aes = FALSE) +
  jaspGraphs::geom_point(data = galileo, mapping = ggplot2::aes(x = init.h, y = h.d)) +
  jaspGraphs::geom_rangeframe() +
  jaspGraphs::themeJaspRaw(legend.position = "right")


basObject$postprobs
predict(basObject, estimator = "HPM")$fit


predictBasSpecificModel <- function(basObject, modelOrder) {

  modelIndex <- 3
  newdata <- basObject$X

  models <- basObject$which[modelIndex]
  beta <- basObject$mle[modelIndex]
  gg <- basObject$shrinkage[modelIndex]
  intercept <- basObject$intercept[modelIndex]
  beta.m <- beta[[1]]
  model.m <- models[[1]]
  Ypred <- (newdata[, model.m[-1], drop = FALSE] %*% beta.m[-1]) * gg[1] + beta.m[1]

}


raw <- FALSE
fullModel <- h_d ~ poly(init_h, maxDegree, raw = raw)
galileo_jasp_bas <- galileo
galileo_jasp_bas$init_h <- ordered(galileo_jasp_bas$init_h)
galileo_jasp_bas$h_d    <- ordered(galileo_jasp_bas$h_d)
basObject <- BAS::bas.lm(
  formula    = fullModel,
  data       = galileo_jasp_bas,
  prior      = "JZS",
  modelprior = BAS::uniform()
)

