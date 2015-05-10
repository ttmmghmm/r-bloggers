# set up a group constraint that dynamically adjusts the min/max based on the date.

define your own objective function. 
two ways to accomplish this:
  1. custom objective function - define a custom objective function to
penalize portfolio weights that violate benchmark weights
2. loop over the returns and benchmark weights, modifying the constraints
each iteration

Here is an example with box constraints. 
question asks for group constraints, but the same concept applies and doing this first with
box constraints should be easier to understand.

###########
install.packages("PortfolioAnalytics", repos="http://R-Forge.R-project.org")
library(PortfolioAnalytics)
data(edhec)
data(weights)

# Use the first 4 columns in edhec for a returns object
R <- edhec[, 1:4]
colnames(R) <- c("CA", "CTAG", "DS", "EM")
head(R, 5)

# benchmark weights
bweights <- weights[,1:4]
# normalize to sum to 1
bweights <- xts(t(apply(bweights, 1, function(x) x / sum(x))),
                index(weights))
colnames(bweights) <- c("CA", "CTAG", "DS", "EM")
head(bweights)

# subset the returns object to begin after bweights
R <- R[paste(first(index(bweights)),"/")]

# Get a character vector of the fund names
funds <- colnames(R)

# Construct initial portfolio with basic constraints.
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum",
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="long_only")

# objective to minimize portfolio standard deviation
init.portf <- add.objective(portfolio=init.portf, type="risk",
                            name="StdDev")

##### Solution 1: custom objective function #####
# objective function to penalize portfolio weights that violate benchmark
# weights by +/- 2%.
foo <- function(R, weights, benchmark_weights){
  idx <- index(last(R))
  
  # this will use the last observation before idx of weights in the
  benchmark
  # if the index of the returns does not match the index of
  benchmark_weights
  # e.g. annual benchmark weights and quarterly rebalancing period
  
  # should do some other checks in here to make sure you get valid weights
  bw <- as.numeric(last(benchmark_weights[paste("/",idx,sep="")]))
  
  # penalize weights that are outside of the benchmark weights +/- 2%
  out <- 0
  penalty <- 1e4
  N <- length(bw)
  
  bmax <- bw + 0.02
  # Only go to penalty term if any of the weights violate max
  if(any(weights > bmax)){
    out <- out + sum(weights[which(weights > bmax[1:N])] -
                       bmax[which(weights > bmax[1:N])]) * penalty
  }
  
  bmin <- bw - 0.02
  # Only go to penalty term if any of the weights violate min
  if(any(weights < bmin)){
    out <- out + sum(bmin[which(weights < bmin[1:N])] -
                       weights[which(weights < bmin[1:N])]) * penalty
  }
  
  # objective function must return a single value to minimize
  return(out)
}

# use type = "risk" because we want to minimize the penalty term for
# portfolio weights that are outside of the bounds of the benchmark weights
portf <- add.objective(portfolio=init.portf, type="risk", name="foo",
                       arguments=list(benchmark_weights=bweights))

rp <- random_portfolios(init.portf, 1000)
rebal <- "years"
train <- 36
opt <- optimize.portfolio.rebalancing(R, portf,
                                      training_period = train,
                                      rebalance_on = rebal,
                                      optimize_method="random", rp=rp,
                                      trace=TRUE)
opt
extractWeights(opt)
chart.Weights(opt)

##### Solution 2: loop over returns and change the constraints #####
library(foreach)
library(iterators)

ep.i <- endpoints(R, on = rebal)[which(endpoints(R, on = rebal) >= train)]
out_list<-foreach::foreach(ep=iterators::iter(ep.i), .errorhandling='pass',
                           .packages='PortfolioAnalytics') %dopar% {
                             tmpR <- R[1:ep,]
                             idx <- index(last(tmpR))
                             
                             # this will use the last observation before idx of weights in the
                             benchmark
                             # if the index of the returns does not match the index of
                             benchmark_weights
                             # e.g. annual benchmark weights and quarterly rebalancing period
                             # should do some other checks in here to make sure you get valid weights
                             bw <- as.numeric(last(bweights[paste("/",idx,sep="")]))
                             init.portf$constraints[[2]]$min <- bw - 0.02
                             init.portf$constraints[[2]]$max <- bw + 0.02
                             optimize.portfolio(R[1:ep,], portfolio=init.portf,
                                                optimize_method="random", trace=TRUE,
                                                rp=rp, parallel=FALSE)
                           }

# extract the optimal weights
xts(do.call(rbind, lapply(out_list, function(x) x$weights)), index(R)[ep.i])

# optimal weights for solution 1 and 2 should be equal
all.equal(xts(do.call(rbind, lapply(out_list, function(x) x$weights)),
              index(R)[ep.i]),
          extractWeights(opt), check.attributes = FALSE)
# ##########
# 
# Regards,
# Ross
# 
# On Fri, Apr 17, 2015 at 12:02 PM, Peter <peter.michaels@gmail.com> wrote:
#   
#   Hi,
# 
# I am just starting to use PortfolioAnalytics and I am trying to 
# 
# I want to constrain the portfolio sector weights to always be +/- 2% from
# the benchmark sector weights.  Is it possible to set the constraint so that
# I can provide a function which resets the min/max parameters?  I don't see
# how the optimize.portfolio.rebalancing call can be used for a benchmark-ed
# portfolio without changing the group constraint for every rebalance period.