#' Simulate and Visualize a Binomial Distribution
#'
#' @param iter number of iterations of the experiment
#' @param n size of the sample
#' @param p probability of success in each trial
#'
#' @importFrom graphics barplot
#' @importFrom grDevices rainbow
#'
#' @return A numeric vector with the proportions of each possible number of successes (from 0 to `n`)
#' over the `iter` iterations.
#' @export
#'
#' @examples
#' # Simulate a binomial distribution with 500 iterations, 20 trials per iteration,
#' # and 70% success rate
#' mybin(iter = 500, n = 20, p = 0.7)

mybin=function(iter=100, n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main=paste("Binomial simulation \n Number of iterations:", iter), xlab="Number of successes")
  succ.tab/iter
}

