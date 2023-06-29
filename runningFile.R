library(BAS)
mround <- function(x, base) base * round(x / base)



data(galileo, package = "UsingR")

polynomial = function(x,coefs) {
  if (inherits(coefs, "lm"))
    coefs <- coef(coefs)

  sum <- 0
  for(i in 0:(length(coefs)-1)) {
    sum <- sum + coefs[i+1]*x^i
  }
  sum
}

maxDegree <- 4L

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
    x = rep(xValues, maxDegree),
    y = c(yValues),
    order = rep(factor(seq_len(maxDegree)), each = length(xValues))
)

ggplot2::ggplot() +
  ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = x, y = y, group = order, color = order), inherit.aes = FALSE) +
  jaspGraphs::geom_point(data = galileo, mapping = ggplot2::aes(x = init.h, y = h.d)) +
  jaspGraphs::geom_rangeframe() +
  jaspGraphs::themeJaspRaw(legend.position = "right")


fullModel <- h.d ~ poly(init.h, maxDegree, raw = TRUE)

basObject <- BAS::bas.lm(
  formula    = fullModel,
  data       = galileo,
  prior      = "JZS",
  modelprior = BAS::uniform()
)
basObject$X[, -1] == resLm[[5]]$model[, -1]

idx <- extractModelIndices(basObject, maxDegree)
basObject$which[idx]
basObject$mle[idx]
lapply(resLm, \(x) unname(coef(x)))

coefMat <- matrix(0, length(idx), maxDegree + 1L)
for (i in seq_along(idx)) {
  coefs <- basObject$mle[[idx[i]]]
  coefMat[i, seq_along(coefs)] <- coefs
}


basObject$X %*% coefMat

fitted(basObject)

BAS:::fitted.bas
BAS:::predict.bas

basFit$postprobs


summary(basFit)

BAS:::predict.bas()

m_quadratic <- which(sapply(basFit$which, \(m) identical(m, 0:2)))
m_cubic     <- which(sapply(basFit$which, \(m) identical(m, 0:3)))

basFit$logmarg

basFit[[5]]
exp(basFit$logmarg[m_quadratic] - basFit$logmarg[m_cubic])

summary(basFit)
