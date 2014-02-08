library(lubridate)
library(rstan)   # Available from http://mc-stan.org/

rodents = read.csv(
  "workflows-and-analysis/portal-rodents/Portal_wide.csv", 
  as.is = TRUE
)
count = rowSums(rodents[,-(1:4)])
julian = as.numeric(julian(ymd(rodents$date)))
t = (julian - min(julian)) / 365.25
day = as.integer(as.factor(rodents$date))

total_removal = as.integer(1:24 %in% c(5,7,10,16,23,24))

data = data = list(
  N = nrow(rodents),
  t = t,
  count = count,
  day = day,
  N_periods = length(unique(rodents$period)),
  N_plots = length(unique(rodents$plot)),
  N_days = length(unique(day)),
  period = rodents$period,
  plot = rodents$plot,
  total_removal = total_removal
)

# Should add random effect for day in addition to period
model = stan(
  file = "workflows-and-analysis/portal-rodents/rodents.stan",
  data = data,
  chains = 2,
  iter = 200,
  verbose = TRUE
)

ex = extract(model)



x = with(
  as.list(sapply(ex, mean)),
  Intercept + 
    beta_t * t +
    beta_annual_period * sin(2/1 * pi * (t + annual_offset)) +
    beta_decadal_period * sin(2/decadal_period * pi * t + decadal_offset)
)

plot(count ~ t, cex = .5, pch = 16)
lines(
  t,
  45 * plogis(x),  
  col = 2,
  lwd = 8
)
lines(
  t,
  45 * plogis(x + mean(ex$beta_total_removal)),  
  col = "slateblue",
  lwd = 8
)


plot(apply(ex$alpha_period, 2, median), lwd = 2, type = "l")
abline(h = 0)
