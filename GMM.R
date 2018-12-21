start.time <-Sys.time()
library(forecast)
library(gmm)

#Setting seed for reproducible results. Just helpful in debugging.
set.seed(1) 

# Simulating series1 with parameters ß₀=0, ß₁=0, 𝛔²=1
S1_b0=0
S1_b1=0
S1_sigma_sq=1
S1_th_mean=S1_b0/(1-S1_b1)
S1_th_mean
S1_th_var=S1_sigma_sq/(1-S1_b1^2)
S1_th_var
S1_y0=S1_th_mean
S1_y=arima.sim(list(order=c(0,0,0)), n=200) + 0
S1_y_fixed=S1_y-S1_y[1]+S1_y0
S1_y_fixed

#Just to check if ACF
S1_acf<-acf(S1_y_fixed,type="correlation",plot=T)
S1_acf

# Simulating series2 with parameter ß₀=1, ß₁=0.9, 𝛔²=1
S2_b0=1
S2_b1=0.9
S2_sigma_sq=1
S2_th_mean=S2_b0/(1-S2_b1)
S2_th_mean
S2_th_var=S2_sigma_sq/(1-S2_b1^2)
S2_th_var
S2_y0=S2_th_mean
S2_y = arima.sim(list(order=c(1,0,0), ar=0.9), n=200) + 10
S2_y_fixed=S2_y-S2_y[1]+S2_y0
S2_y_fixed

#Just to check if ACF
S2_acf<-acf(S2_y_fixed,type="correlation",plot=T)
S2_acf

# Q1. Unconditional mean and variance for S1 and S2
mean(S1_y_fixed)
var(S1_y_fixed)
mean(S2_y_fixed)
var(S2_y_fixed)

# Q2. Expression for jth autocorrelation for S1 and S2
# Refer additional notes

# Q3. Plot simulated series
# Refer additional notes as well
ts.plot(S1_y_fixed)
ts.plot(S2_y_fixed)

# Q4. 1 to 10 step ahead forecast

# Defining geometric sum function 
geomsum = function(a, r, n) {
  x = 0
  for(i in 1:n) x = x + a * r^(i-1)
  return(x)
}

# Forecasting y for Series 1
S1_y_forecast=c()
for(i in 1:10){
  y=S1_b0*geomsum(1,S1_b1,i)+S1_y_fixed[200] *S1_b1^i
  S1_y_forecast=c(S1_y_forecast,y)
}
S1_y_forecast

#forecasting variance for Series 1
S1_y_forecast_var=c()
for(i in 1:10){
  y=S1_sigma_sq*geomsum(1,S1_b1^2,i)
  S1_y_forecast_var=c(S1_y_forecast_var,y)
}
S1_y_forecast_var

# Ploting forecast and confidence interval for Series 1
ts.plot(S1_y_fixed, xlim=c(1,210), ylim=c(-2,3))
points(201:210, S1_y_forecast, type="l",col=2)
points(201:210, S1_y_forecast - 1.96*(S1_y_forecast_var^(.5)), type="l",col=2,lty=2)
points(201:210, S1_y_forecast + 1.96*(S1_y_forecast_var^(.5)), type="l",col=2,lty=2)

# Forecasting y for Series 2
S2_y_forecast=c()
for(i in 1:10){
  y=S2_b0*geomsum(1,S2_b1,i)+S2_y_fixed[200] *S2_b1^i
  S2_y_forecast=c(S2_y_forecast,y)
}
S2_y_forecast

#forecasting variance for Series 2
S2_y_forecast_var=c()
for(i in 1:10){
  y=S2_sigma_sq*geomsum(1,S2_b1^2,i)
  S2_y_forecast_var=c(S2_y_forecast_var,y)
}
S2_y_forecast_var

# Ploting forecast and confidence interval for Series 2
ts.plot(S2_y_fixed, xlim=c(1,210), ylim=c(2,15))
points(201:210, S2_y_forecast, type="l",col=2)
points(201:210, S2_y_forecast - 1.96*(S2_y_forecast_var^(.5)), type="l",col=2,lty=2)
points(201:210, S2_y_forecast + 1.96*(S2_y_forecast_var^(.5)), type="l",col=2,lty=2)

# Exporting simulated data 
write.csv(S1_y_fixed, "Task1.csv")
write.csv(S2_y_fixed, "Task1.csv")

# Exporting forecasted data
write.csv(S1_y_forecast, "Task1.csv")
write.csv(S1_y_forecast - 1.96*(S1_y_forecast_var^(.5)), "Task1.csv")
write.csv(S1_y_forecast + 1.96*(S1_y_forecast_var^(.5)), "Task1.csv")

write.csv(S2_y_forecast, "Task1.csv")
write.csv(S2_y_forecast - 1.96*(S2_y_forecast_var^(.5)), "Task1.csv")
write.csv(S2_y_forecast + 1.96*(S2_y_forecast_var^(.5)), "Task1.csv")

# Recoding time
end.time <-Sys.time()
time.taken<-round(end.time-start.time,2)
time.taken
