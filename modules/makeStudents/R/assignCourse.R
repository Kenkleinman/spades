
## Replace thing below with courses drawn from spire

assignCourse <-function(sim) {
  # Eventually do different thing depending on time (time 1, 1.5 = year 1)
  # and depending on school = sim$student[,2]
  t(replicate(sim$totalNstudents, floor(runif(4,0,1000)) ))
}

# course.student <- floor(runif(4,0,1000))   # a student gets four course "numbers"

#t(replicate(10, floor(runif(4,0,1000)) ))



