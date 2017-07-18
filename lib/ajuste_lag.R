ajuste_lag <- function(x){
  
  ifelse( is.na( x - lag(x) ) == TRUE, x, x - lag(x) )
  
}
