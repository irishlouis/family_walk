#' get.peak.summary
#'
#' @param v - vector to be smoothed / considered
#' @param k - rolling average
#' @param freq - frequency device was recording at
#'
#' @return vector summarising the peak for v
#' 
get.peak.summary <- function(v, k, freq) {
  require(zoo)
  v.smooth <- rollapply(v, k, mean)
  switch.dir <- sapply(seq_along(v.smooth), 
                       function(x) ifelse(x<length(v.smooth),
                                          v.smooth[x]<v.smooth[x+1],
                                          F))
  peaks.per.sec <- sum(sapply(seq_along(v.smooth), 
                              function(x) ifelse(x<length(v.smooth), 
                                                 switch.dir[x] != switch.dir[x+1], 
                                                 F))) / ((length(v.smooth)+k-1)/freq)
  if(peaks.per.sec == 0) return(list(rep(F, 5)))
  
  period <- rollapply(which(sapply(seq_along(v.smooth), 
                                   function(x) ifelse(x<length(v.smooth), 
                                                      switch.dir[x] != switch.dir[x+1], 
                                                      F)) == T),
                      2, function(x) (x[2] - x[1])/freq)
  
  ampl <- rollapply(which(sapply(seq_along(v.smooth), 
                                   function(x) ifelse(x<length(v.smooth), 
                                                      switch.dir[x] != switch.dir[x+1], 
                                                      F)) == T),
                      2, function(x) abs(v.smooth[x[1]] - v.smooth[x[2]]))
  
  avg.period <- mean(period)
  sd.period <- sd(period)
  avg.amp <- mean(ampl)
  sd.amp <- sd(ampl)
  
  return(list(peaks.per.sec = peaks.per.sec, 
           avg.period = avg.period, 
           sd.period = sd.period,
           avg.amp = avg.amp,
           sd.amp = sd.amp))
}