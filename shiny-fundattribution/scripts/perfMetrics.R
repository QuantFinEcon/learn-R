formatRpt100perc = function(num) {
  return(paste0(sprintf(as.numeric(num) * 100, fmt = "%#.2f"), "%"))
}

formatRptDouble = function(num) {
  return(sprintf(as.numeric(num), fmt = "%#.2f"))
}

formatRptRange = function(rg) {
  return(paste(rg, collapse = " < "))
}

# Alpha, Beta
pmBeta = function(fund, benchmark) {
  m = lm(fund ~ benchmark)
  co = summary(m)$coefficients
  # Estimate  Std. Error    t value     Pr(>|t|)
  # (Intercept) 0.001320446 0.001824298  0.7238106 4.697518e-01
  # benchmark   1.059847840 0.040488802 26.1763199 4.031082e-79
  co = co[]
  return(list(
    alpha = c(co[1, 1] - co[1, 2],
              co[1, 1],
              co[1, 1] + co[1, 2]),
    beta = c(co[2, 1] - co[2, 2],
             co[2, 1],
             co[2, 1] + co[2, 2])
  ))
}

pmBetaBullBear = function(fund, benchmark, bear = TRUE) {
  # sensi to benchmark differentiate by benchmark performance
  # (ideal if BearBeta <= 0, BullBeta >>1)
  i = if (bear) {
    which(benchmark > 0)
  }
  else {
    which(benchmark < 0)
  }
  m = summary(lm(fund[i] ~ benchmark[i]))
  co = m$coefficients
  return(co[2, 1])
}

pmUpsideDownsideRisk = function(fund) {
  # right/positive skewness ~= vol(up ret)/vol(down ret)
  # moments::skewness(fund,na.rm=TRUE)
  sd_up = sd(fund[which(fund > 0)])
  sd_dw = sd(fund[which(fund < 0)])
  return(sd_up / sd_dw)
}

pmSharpeRatio = function(fund,
                         rf,
                         lookback = 12,
                         downside = FALSE) {
  # Sharpe Ratio - excess return per idiosyncratic risk unit
  # (> 1 ==> more than compensate (downside) vol)
  rb_p = fund[(length(fund) - lookback + 1):length(fund)]
  rb_rf = rf[(length(rf) - lookback + 1):length(rf)]
  meanFund = mean(rb_p, na.rm = TRUE)
  meanRF = mean(rb_rf, na.rm = TRUE)
  sdFund = if (downside) {
    sd(rb_p, na.rm = TRUE)
  } else {
    sd(rb_p[which(rb_p < 0)], na.rm = TRUE)
  }
  (meanFund - meanRF) / sdFund
}

pmTreynorRatio = function(fund, rf, benchmark, lookback = 12) {
  # Treynor Ratio - excess return per systematic risk unit
  # (excess % return scale by beta)
  rb_p = fund[(length(fund) - lookback + 1):length(fund)]
  rb_rf = rf[(length(rf) - lookback + 1):length(rf)]
  meanFund = mean(rb_p, na.rm = TRUE)
  meanRF = mean(rb_rf, na.rm = TRUE)
  beta_p = summary(lm(fund ~ benchmark))$coefficients[2, 1]
  (meanFund - meanRF) / beta_p
}

pmInformationRatio = function(fund, benchmark) {
  # mean(TE) / SD(TE)
  exRet = fund - benchmark
  mean(exRet, na.rm = TRUE) / sd(exRet, na.rm = TRUE)
}

# test ----------------------------------------------------------------


# colnames(all)
# fund = all[, 'Global.Macro']
# benchmark = all[, 'SPX_Index']
# rf = all[, 'yield_10Y']
# 
# pmBeta(fund, benchmark)
# formatRptRange(formatRpt100perc(pmBeta(fund, benchmark)$alpha))
# formatRptRange(formatRptDouble(pmBeta(fund, benchmark)$beta))
# 
# formatRptDouble(pmBetaBullBear(fund, benchmark, bear = TRUE))
# formatRptDouble(pmBetaBullBear(fund, benchmark, bear = FALSE))
# formatRptDouble(pmSharpeRatio(fund, rf, lookback = 12, downside = FALSE))
# formatRptDouble(pmSharpeRatio(fund, rf, lookback = 12, downside = TRUE))
# formatRpt100perc(pmTreynorRatio(fund, rf, benchmark, lookback = 12))
# 
# formatRptDouble(pmUpsideDownsideRisk(fund))
# 
# formatRptDouble(pmInformationRatio(fund, benchmark))
