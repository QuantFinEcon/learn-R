library(zoo)
library(randomForest)

# remove all rows with NA in any columns -> avoid rolling reg error
query = function(data, header) {
  out = as.data.frame(data[, header, drop = FALSE])
  out[complete.cases(out), , drop = FALSE]
}

rollRegPredict = function(data, width, outcome, predictor) {
  dolm <- function(x, outcome, predictor) {
    co <-
      predict(lm(x[, outcome] ~ x[, predictor], as.data.frame(x)))
    co[end(co)[1]]
  }
  d = query(data, c(outcome, predictor))
  out = rollapply(
    d,
    width = width,
    FUN = dolm,
    by.column = FALSE,
    align = "right",
    outcome = outcome,
    predictor = predictor
  )
  out = as.data.frame(out)
  return(list(actual = d[width:dim(d)[1], outcome, drop = FALSE], 
              fit = out))
}

rollRegR2 = function(data, width, outcome, predictor) {
  dolm <- function(x, outcome, predictor) {
    summary(lm(x[, outcome, drop=FALSE] ~ x[, predictor, drop=FALSE], 
               as.data.frame(x)))$r.squared
  }
  
  d = query(data, c(outcome, predictor))
  r = rownames(d)
  out = rollapply(
    d,
    width = width,
    FUN = dolm,
    by.column = FALSE,
    align = "right",
    outcome = outcome,
    predictor = predictor
  )
  out = as.data.frame(out)
  rownames(out) = r[width:length(r)]
  colnames(out) = "R2"
  out
}


rollRegStats = function(data, width, outcome, predictor, stats) {
  idx = switch(stats,
               "beta" = 1,
               "p-value" = 4)
  dolm <- function(x, outcome, predictor) {
    co <-
      coef(summary(lm(x[, outcome] ~ x[, predictor], as.data.frame(x))))
    # Estimate  Std. Error   t value    Pr(>|t|)
    # c(Est = co[, 1], SE = co[, 2], t = co[, 3], P = co[, 4])
    co[, idx]
  }
  d = query(data, header = c(outcome, predictor))
  r = rownames(d)
  out = rollapply(
    d,
    width = width,
    FUN = dolm,
    by.column = FALSE,
    align = "right",
    outcome = outcome,
    predictor = predictor
  )
  colnames(out) = c("Intercept", predictor)
  out = as.data.frame(out)
  rownames(out) = r[width:length(r)]
  out
}



varImp = function(data, outcome, predictor) {
  rfdata = query(data, c(outcome, predictor))
  rf_fit = randomForest::randomForest(
    x = rfdata[, predictor],
    y = rfdata[, outcome],
    # mtry=default for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
    ntree = 3000,
    importance = TRUE
  )
  vi = importance(rf_fit)
  vi[, "%IncMSE"]
}

rollIncR2 = function(data, width, outcome, predictor) {
  out = NULL
  subsetData = query(data, c(outcome, predictor))
  
  for (p in predictor) {
    rest = predictor[-which(predictor == p)]
    
    # leave-one-out-CV
    base = rollRegR2(
      subsetData,
      width = width,
      outcome = outcome,
      predictor = rest
    )
    extra = rollRegR2(
      subsetData,
      width = width,
      outcome = outcome,
      predictor = predictor
    )
    incR2 = extra - base
    
    if (is.null(out)) {
      out = rbind(out, as.data.frame(incR2))
    } else {
      out = cbind(out, as.data.frame(incR2))
    }
  }
  colnames(out) = predictor
  out
}

# test -----------------------------------------------------------------

# outcome= "Global.Macro"
# predictor = c("IXG_US_Equity","VVIX_Index","VIX_Index")
# width = 12
# data = prices
# 
# out = rollRegStats(data,width=5,outcome,predictor, stats="p-value")
# out = rollRegPredict(data,width=5,outcome,predictor)
# 
# # rolling R2 as attribution of explained variance
# r2 = rollRegR2(data,width=12,outcome=outcome,predictor=predictor)
# # rolling incremental R2 as proportion of total R2 -> attribution
# ir2 = rollIncR2(data,width=12,outcome=outcome,predictor=predictor)
# ir2_norm = t(apply(ir2,1, function(x) x/sum(x)))
# r2n = ir2_norm * rep(t(r2$R2),ncol(ir2_norm))

