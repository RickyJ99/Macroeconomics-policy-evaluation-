setwd("C:\Users\feder\OneDrive\Desktop\InflationGrowth\assignment")

library(xts)
library(zoo)
library(MultipleBubbles)
library(aTSA)
library(urca)
library(flexmix)
library(forecast)
library(vars)
library(ggplot2)
library(knitr)
library(erer)

#function
adf_test <- function(timeseries) { # nolint

    out <- matrix(NA, nrow = 0, ncol = 7)

    out_colnames <- c("N of lags", "Type", "lag", "ADF",
     "p.value", "Stationary at 5%", "Stationary at 10%")

    colnames(out) <- out_colnames

    for (count in 1:12) {
        i   <<-  adf.test(timeseries, nlag = count, output = FALSE)
        #i   <-  ur.df2( timeseries, type ="none", lags = 12, 
        #                selectlags = "BIC", digit = 2)

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
            if (rw["p.value"] >= .05) {
                rw[6] <- "No Stat."
                } else {
                rw[6] <- "Stat"
                }

            if (rw["p.value"] >= .1) {
                rw[7] <- "No Stat."
                } else {
                rw[7] <- "Stat"
                }
            if (rw["Type"] == 1) {
                rw["Type"] <- "no drift no trend"
            } else if (rw["Type"] == 2) {
                rw["Type"] <- "with drift no trend"
            } else {
                rw["Type"] <- "with drift and trend"
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
    out4 <- ur.df2(timeseries, type = "drift",
    lags = 12, selectlags = "BIC", digit = 2)
    out5 <- ur.df2(timeseries, type = "trend",
    lags = 12, selectlags = "BIC", digit = 2)
    out6 <- ur.df2(timeseries, type = "none",
    lags = 12, selectlags = "BIC", digit = 2)
    out7 <- adf_test(timeseries)  
    out <- list(out1, out2, out3, out4, out5, out6, out7)
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
   return(c(p, 0, q))
}

arroots <- function(object){
  if(!("Arima" %in% class(object)) &
     !("ar" %in% class(object)))
    stop("object must be of class Arima or ar")
  if("Arima" %in% class(object))
    parvec <- object$model$phi
  else
    parvec <- object$ar
  if(length(parvec) > 0)
  {
    last.nonzero <- max(which(abs(parvec) > 1e-08))
    if (last.nonzero > 0)
      return(structure(list(
          roots=polyroot(c(1,-parvec[1:last.nonzero])),
          type="AR"),
        class='armaroots'))
  }
  return(structure(list(roots=numeric(0), type="AR"),
    class='armaroots'))
}

# Compute MA roots
maroots <- function(object)
{
  if(!("Arima" %in% class(object)))
    stop("object must be of class Arima")
  parvec <- object$model$theta
  if(length(parvec) > 0)
  {
    last.nonzero <- max(which(abs(parvec) > 1e-08))
    if (last.nonzero > 0)
      return(structure(list(
          roots=polyroot(c(1,parvec[1:last.nonzero])),
          type="MA"),
        class='armaroots'))
  }
  return(structure(list(roots=numeric(0), type="MA"),
    class='armaroots'))
}

plot.armaroots <- function(x, xlab="Real", ylab="Imaginary",
    main=paste("Inverse roots of", x$type,
          "characteristic polynomial"),
    ...){
  oldpar <- par(pty='s')
  on.exit(par(oldpar))
  plot(c(-1,1), c(-1,1), xlab=xlab, ylab=ylab,
       type="n", bty="n", xaxt="n", yaxt="n", main=main, ...)
  axis(1, at=c(-1,0,1), line=0.5, tck=-0.025)
  axis(2, at=c(-1,0,1), label=c("-i","0","i"),
    line=0.5, tck=-0.025)
  circx <- seq(-1,1,l=501)
  circy <- sqrt(1-circx^2)
  lines(c(circx,circx), c(circy,-circy), col='gray')
  lines(c(-2,2), c(0,0), col='gray')
  lines(c(0,0), c(-2,2), col='gray')
  if(length(x$roots) > 0)
  {
    inside <- abs(x$roots) > 1
    points(1/x$roots[inside], pch=19, col='black')
    if(sum(!inside) > 0)
      points(1/x$roots[!inside], pch=19, col='red')
  }
}

#import dataser

oil     <- read.csv("oil_data_1.csv")

#convert to xts
from    <- as.Date("1973-02-01")
to      <- as.Date("2007-12-01")





# Point 1



#generate xts
yq      <- seq(from, to, by = "month") 

oil     <- xts(oil, order.by = yq)
l_yq    <- length(yq)

plot(oil, col = c("#0077ff", "#ff00a2", "#48ff00"))
legend("top", legend = c("prod", "rea", "price"),
       col = c("#0077ff", "#ff00a2", "#48ff00"),
       lty = 1:1, cex = 1)
title("Yield curve at 2020")
#I(1)


timeseries <- ts(oil$rea)
out <- time_series_plot(timeseries)

kable(out[[7]])

print("Without constant and without time trend")
print(out[[6]])
#plot(out[[6]])
print("With constant and without time trend")
print(out[[4]])
#plot(out[[4]])
print("With constant and with time trend")
print(out[[5]])
#plot(out[[5]])






#first diff

timeseries     <-   diff(oil$rea)
timeseries     <-  timeseries[-1]
timeseries     <-  xts(timeseries, order.by = yq[-1])

out <- time_series_plot(timeseries)

plot(out[[1]])
plot(out[[2]])
plot(out[[3]])


#Point2
#analysis ts

print("Without constant and without time trend")
out[[6]]
print("With constant and without time trend")
out[[4]]
print("With constant and with time trend")
out[[5]]


