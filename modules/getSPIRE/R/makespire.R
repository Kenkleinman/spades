## Function to grab n students from a specified sc/co from a spire data frame

grab_students <- function(sc, n, year, res, schoolnumber, colnumber, spire.from.ms,sim) {
shortspire <- spire.from.ms[spire.from.ms$`Primary School/College Short` == sc &
                            spire.from.ms$`Academic Level BOT` == year &
                            spire.from.ms$`Tuition Residency Group` == res,]
uni <- unique(shortspire$ID)  

if (n == 1) { 
  shortspire$newid <- paste0(schoolnumber, colnumber,".",shortspire$ID)
  output <- shortspire}
else {
  output <- spire.from.ms[0,]
  for (i in 1:n) {
    stud <- sample(uni,size = 1, replace = TRUE)
    new <- shortspire[shortspire$ID == stud,]
    newid <- paste0(schoolnumber,".", colnumber,".",i,".",new$ID)
    new$newid <- newid
    output <- rbind(output, new)
  }
 }
  return(output)
}



## how much financial aid??



Rmultinom <- Vectorize(rmultinom, vectorize.args = c("n","size","prob"))

# howmuch <- function(indata, sim){
#   transmute(indata, amount = case_when(
#     (finaid.yn == 1 & `Tuition Residency Group` == "In State")  
#       ~ mysim$finaid$award[which(as.vector(rmultinom(n=1,size =1,prob = sim$finaid$instate)) == 1)],
#     (finaid.yn == 1 & `Tuition Residency Group` != "In State")
#       ~ mysim$finaid$award[which(as.vector(rmultinom(n=1,size=1,prob=sim$finaid$nonres)) == 1)],
#       TRUE ~ 0 )) -> amount
#   return(amount)
# }

howmuch <- function(indata, sim){
  in.fin <- filter(indata, (`Tuition Residency Group` == "In State")) 
  finaidonly <-  in.fin %>% select(finaid.yn)
#  browser()
  in.fin.amount <- apply(finaidonly, 1, function (x) ifelse(x == 1,
        sim$finaid$award[which(as.vector(rmultinom(n=1,size =1,prob = sim$finaid$instate)) == 1)],
        0))

  out.fin <- filter(indata, (`Tuition Residency Group` != "In State")) 
  finaidonly <- out.fin %>%  select(finaid.yn)
  out.fin.amount <- apply(finaidonly, 1, function (x) ifelse(x == 1,
      sim$finaid$award[which(as.vector(rmultinom(n=1,size =1,prob = sim$finaid$nonres)) == 1)],
      0))

  in.fin$amount <- in.fin.amount
  out.fin$amount <- out.fin.amount
  
#  browser()
  
  amount <- rbind(in.fin,out.fin)
  return(amount)
}







## Add in financial aid status.  For now, we assume: 1) all sc/co get same finaid 
## 2) binary prob of finaid per student
## 3) International students get the same as OoS

grab_finaid <- function(indata, sim){
  
instateprob <- sim$n.finaid$n.instate/sim$n.restype$instate
oosprob <- sim$n.finaid$n.nonres/sim$n.restype$oos_int
indata %>% select(`Tuition Residency Group`, newid) %>% unique() -> people
people$prob <- ifelse(
       people$`Tuition Residency Group` == "In State", instateprob, oosprob
              )
  people$finaid.yn <-  rbinom(dim(people)[1],1,people$prob)
  howmuchaid <- howmuch(people, sim)  #returns finaid with an amount column
  #finaid2 <- cbind(finaid, howmuchaid)
  output = right_join(howmuchaid, indata)
  return(output)
}

## Function to ask for N students from each of the sc/colleges, 
## for each year, for each res status paste them together

makespire <- function(sc.vec,n.df, spire.from.gS,sim){
  year <- c("Freshman", "Sophomore", "Junior", "Senior")
  resid <- c("In State", "Out of State", "International")
  year.resid <- expand_grid(year,resid)
  year.resid$comb <- paste0(year.resid$year,".",year.resid$resid)
  
  output <- spire.from.gS[0,]
  for (j in 1:12) {
    n.vec <- unlist(select(n.df,matches(year.resid$comb[j])))
     
    for (i in 1:10) {
      if (n.vec[i] > 0) {
        new <- grab_students(
               sc.vec[i], n.vec[i], year.resid$year[j], 
               year.resid$resid[j], 
               i, j, 
               spire.from.gS,sim)
        output <- rbind(output, new)
      }
    }
  }
  
  # Now that we have the students for each s/c and (importantly) res type, get the fin aid status
  output <- grab_finaid(output, sim)
  return(output)

}


## Commented lines below are for making a fake spire dataset.  Not needed now that I have a real spire data set
# 
#
# origspire <- function(){
#   spireid <- rep(1:1000, times = 4)
#   school_college <- rep(sample( c("CNS","SBS", "SPHHS", "HFA"), 1000, replace = T), times = 4)
#   dept <- sample( c("MATH", "STAT", "PUBHLTH", "SOC", "POLI", "BIO", "ART", "ENG"), 4000, replace = T)
#   course_number <- sample( c(127, 152, 101, 200, 300, 301, 302, 400), 4000, replace = T)
#   spire <- data.frame(spireid, school_college, dept, course_number)
#   return(spire)
# }
# 
# spire <- origspire()
# 


