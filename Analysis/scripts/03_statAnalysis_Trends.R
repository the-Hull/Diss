
# Fitting Linear Model to decomposed time series. -------------------------



trendplot <- function(){
      ## Maximizes the number of trophic groups in steady state by identifying
      ## ideal cutoff

res <- numeric()

      for(i in 50:95){
            trends <- ts_test(statsFull,cutoff=i*12, expno=1:7)

            res[i-49] <- sum(trends$Trend, na.rm=T)
            print(i)
      }
      x11()
      plot(50:95,
           res,
           type="l",
           lwd=1.2,
           xlab="Time (years)",
           ylab="Non-zero slopes (n)")

      abline(v=(which(res==min(res))+49),
             lty=2,
             col="grey70")

      ideal <- which(res==min(res))+49
      return(ts_test(statsFull,cutoff=ideal*12, expno=1:7))

}
