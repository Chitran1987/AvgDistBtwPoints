rm(list=ls())
n <- 500 #no. of points
numseq <- runif(n, min=3, max = 6) #sequence of numbers to work with
dist <- function(p1,p2){
  return(abs(p1 - p2))
} #The distance function

#Run the period function
resvec <- NULL
for (i in 1:(length(numseq)-1)) {
  for (j in (i+1):(length(numseq))) {
    a <- dist(numseq[i], numseq[j])
    if(is.na(a) == T){
      stop('you have NA values')
    }else{
      resvec <- c(resvec, a)
    }
  }
}
hist(resvec)
mean(resvec)

#plotting the points on the line
subplot(c(1,3))
hist(resvec)
abline(v=mean(resvec), col='blue')
hist(numseq)
y <- rep(1, times=length(numseq))
plot(numseq, y, col='blue', type = 'b')
