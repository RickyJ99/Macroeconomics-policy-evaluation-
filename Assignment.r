setwd(getwd())
library(xts)
library(zoo)
library(MultipleBubbles)
library(aTSA)
library(urca)
library(flexmix)
library(forecast)
library(vars)
library(ggplot2)
#function

adf_test <- function(timeseries) { # nolint

    out <- matrix(NA, nrow = 0, ncol = 7)

    out_colnames <- c("N of lags", "Type", "lag", "ADF",
     "p.value", "Stationary at 5%", "Stationary at 10%")

    colnames(out) <- out_colnames

    for (count in 1:12) {
        i   <-  adf.test(timeseries, nlag = count, output = FALSE)

        for (count2 in 1:3) {

           for (count3 in 1:count) {
            if (count2 == 1) {
               rw <- c(count, count2, count3,
                i$type1[count3, 2], i$type1[count3, 3], NA, NA)

            } else if (count2 == 2) {
               rw <- c(count, count2, count3,
                i$type2[count3, 2], i$type2[count3, 3], NA, NA)

            } else {
                rw <- c(count, count2, count3,
                i$type3[count3, 2], i$type3[count3, 3], NA, NA)

            }
            names(rw) <- out_colnames
            rw[1] <- as.integer(rw[1])
            rw[2] <- as.integer(rw[2])
            rw[3] <- as.integer(rw[3])
            rw["ADF"] <- round(rw["ADF"], digits = 4)
            rw["p.value"] <- round(rw["p.value"], digits = 4)
            if (rw["p.value"] < .05) {
                rw[6] <- "No Stat."
                } else {
                rw[6] <- "Stat"
                }

            if (rw["p.value"] < .01) {
                rw[7] <- "No Stat."
                } else {
                rw[7] <- "Stat"
                }
            if (rw["Type"] == 1) {
                rw["Type"] <- "no costant no trend"
            } else if (rw["Type"] == 2) {
                rw["Type"] <- "with costant no trend"
            } else {
                rw["Type"] <- "with costant and trend"
            }
            out <- rbind(out, rw)
           }
        }

    }

return(out)

}

time_series_plot <- function(timeseries) {
    out1 <- plot(timeseries)
    out2 <- acf(timeseries)
    out3 <- pacf(timeseries)
    #Stationarity
    out4 <- adf_test(timeseries)
    out5 <- ADF_IC(ts(timeseries), adflag = 12, mflag = 1, IC = 1)
    out <- list(out1, out2, out3, out4, out5)
    return(out)
}
bic_score <- function(k, n, l) {
    x <- k * log(n) - 2 * l
    return(x)
}

#best arima select with BIC
bestarima <- function(timeseries, maxlag) {
    plag    <- 1:maxlag
    qlag    <- 1:maxlag

    model1   <- matrix(NA, nrow = 0, ncol = 3)
    colnames(model1) <- c("p", "q", "BIC")
    for (p in plag) {
       for (q in qlag) {
        out <- tryCatch(
        {
            # Just to highlight: if you want to use more than one 
            # R expression in the "try" part then you'll have to 
            # use curly brackets.
            # 'tryCatch()' will return the last evaluated expression 
            # in case the "try" part was completed successfully

            arima(timeseries, order = c(p, 0, q))
            # The return value of `readLines()` is the actual value 
            # that will be returned in case there is no condition 
            # (e.g. warning or error). 
            # You don't need to state the return value via `return()` as code 
            # in the "try" part is not wrapped inside a function (unlike that
            # for the condition handlers for warnings and error below)
        },
        error=function(cond) {
            # Choose a return value in case of error
            return(NA)
        },
        warning=function(cond) {
            # Choose a return value in case of warning
            return(NA)
        }
    )    
    if(any(!is.na(out))){
        x <- arima(timeseries, order = c(p, 0, q))
        x_bic <- bic_score(length(x$coef), x$nobs, x$loglik)
        

        
       } else {
          x_bic <- 9999
       }
       model1 <- rbind(model1, c(p, q, x_bic))
    }
    }
    p <- model1[which.min(model1[, "BIC"]), "p"]
    q <- model1[which.min(model1[, "BIC"]), "q"]
    out <- arima(timeseries, order = c(p, 0, q))
    acf(out$residuals)
   return(c(p,q))
}
#import dataser

oil     <- read.csv(file = "oil_data_1.csv")

#convert to xts
from    <- as.Date("1973-02-01")
to      <- as.Date("2007-12-01")

#generate xts
yq      <- seq(from, to, by = "month") 
oil     <- xts(oil, order.by = yq)
l_yq    <- length(yq)
View(oil)

#plot series

par(mfrow = c(1,1))
plot(oil)



#I(1)
out     <- time_series_plot(ts(oil$Dprod))
plot(oil$Dprod)
plot(out[[2]])
plot(out[[3]])
View(out[[4]])

#Point2
#analysis ts
timeseries     <-   diff(oil$rea)
timeseries[1]  <-   0 
timeseries     <-   xts(timeseries, order.by = yq)
View(timeseries)
out     <-          time_series_plot(timeseries)
plot(out[[1]])
plot(out[[2]])
plot(out[[3]])
View(out[[4]])
test <- out[[4]]
test <- test[which.min(test[,4]), ]

test


best_arima <- bestarima(timeseries, 4)
plot(arroots(arima(timeseries,order(best_arima))

#estimate var yt
#========================================================
# VAR model in level
#========================================================

type1   <- c("none", "const", "trend", "both")
# lag length
out     <- VARselect(oil, lag.max = 24, type = "const")
lag     <-  out$selection[1]
lag

# estimation
var_model_lev <- VAR(oil, p = lag, type = "const")
res           <- residuals(var_model_lev)
par(mfrow = c(3,1))
acf(res[ ,1])
acf(res[ ,2])
acf(res[ ,3])

par(mfrow = c(3,1))
pacf(res[ ,1])
pacf(res[ ,2])
pacf(res[ ,3])
plot(arroots(var_model_lev$y))

# Calculate summary statistics
model_summary <- summary(var_model_lev)

# Obtain variance-covariance matrix
model_summary$covres
model_summary$corres
roots(var_model_lev) #no stationary

oir <- irf(var_model_lev, impulse = colnames(oil),
            response = colnames(oil), n.ahead = 25,
             ortho = TRUE, runs = 1000, seed = 12345
)

##############################
###         POINT 5        ###
##############################
oir <- irf(var_model_lev, impulse = colnames(oil)[1],
            response = colnames(oil)[1], n.ahead = 25,
             ortho = TRUE, runs = 1000, seed = 12345
)
plot(oir)
oir <- irf(var_model_lev, impulse = colnames(oil)[2],
            response = colnames(oil)[2], n.ahead = 25,
             ortho = TRUE, runs = 1000, seed = 12345
)
plot(oir)
oir <- irf(var_model_lev, impulse = colnames(oil)[3],
            response = colnames(oil)[3], n.ahead = 25,
             ortho = TRUE, runs = 1000, seed = 12345
)
plot(oir)
#plot(ts(var_model_lev$y[, 1]))
#lines(oil$Dprod, col = "red")
# forecast of lev data
var_pred    <- predict(var_model_lev, n.ahead = as.integer(fit_l))

var_pred     <- xts(var_pred, order.by = yq[336:l_yq])

#par(mai = rep(0.4, 4)); plot(var_pred)
par(mai = rep(0.4, 4)); fanchart(var_pred)
par(mai = rep(0.4, 4)); fanchart(var_pred)
fcst     <- var_pred$fcst
df_fcst_ddrop  <- data.frame(fcst$Dprod[,2:4]) 
plot(df_fcst_ddrop$CI)
t4 <- oil[335:l_yq,1]
t4
points(oil[335:l_yq,1])
