mysim$finaid$award[which(as.vector(rmultinom(1,1,mysim$finaid$instate)) == 1)]



Rmultinom <- Vectorize(rmultinom, vectorize.args = "prob")

Rmultinom(1,1, list(c(.001,.001,.998), c(.001,.998,.001)))




testhm <- function(indata) {
  mutate(indata, amount = case_when( 
    x == 0 ~ which(as.vector(Rmultinom(1,1, c(.001,.001,.001))) == 1 ), 
    x == 1 ~ which(as.vector(Rmultinom(1,1, c(.001,.001,.001))) == 1),
    TRUE ~ 4
    )) -> out
  return(out)  
}



testdf <- data.frame(x = c(0,0,0,0,0,1,1,1,1,1), y = c(0,0,0,0,0,1,1,1,1,1)   )


apply(p,1,function(x) rmultinom(a,n = 1,x))

apply(testdf,1,function(x) ifelse(x == 1, 
                                  which(as.vector(rmultinom(1,1, c(.001,.001,.001))) ==1),
                                  which(as.vector(rmultinom(1,1, c(.001,.001,1))) == 1)
                                  ))
testdf$z <- c(0,0,0,0,0,1,1,1,1,1)



