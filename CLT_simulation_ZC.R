# Chen Zhu: The Central Limit Theorem Simulation in R
# For Applied Statistics Class
rm(list=ls(pat="*"))     # Remove all objects

#############################


## -------- Example 1: Binomial distribution ------------------

## Central limit theorem (CLT)    
# states that, given certain conditions, the arithmetic mean of a sufficiently 
# large number of iterates of independent random variables, each with a well-defined 
# expected value and well-defined variance, will be approximately normally distributed, 
# regardless of the underlying distribution.


# packages to be used:
library(reshape2)
library(ggplot2)


## Simulation

# The variable:
# How many samples we're going to draw on each step
n <- c(1:32, 2^(6:9)) # Sample size
p <- .5 # binomial probability
sd <- 2 # normal standard definition
pmf <- c("binomial")

# Parameters
nosim <- 1000 # number of simulation in each step
m <- sapply(n , function(n) { # sapply is just a for()
  
  x <- if(pmf=="normal") {
    rnorm(nosim * n, sd=sd)
  } else if(pmf=="uniform") {
    runif(nosim * n)
  } else if(pmf=="binomial") {
    sample(0:1, nosim * n, replace=TRUE, prob=c(p, 1-p))
  }
  
  apply(matrix(x, nosim), 1, mean)
})
colnames(m) <- n # n is my main variable, I have to attach it as data label

# melt is a function from reshape2 package. It will get the matrix or data.frame column names and put them in a collumn repeated several times linked to its correspondent value.
# I can put the parameters and variables in the same line as my simulation values and one simulation per unique parameter combination. It's easier to subset them.
# It's called tidy data. It's wonderful. It's a very good standard to handle with Big Data.
x <- melt(m)
names(x) <- c("simulation", "sampleSize", "mean")


## Results:
# (1) Binomial Distribution
# plot use ggplot2:
g <- ggplot(x, aes(mean, fill=as.factor(sampleSize))) + # Metadata
  geom_histogram(aes(y = ..density..)) + # Histogram bars
  geom_density() + # Density lines
  facet_wrap(~sampleSize) + # Broke in a matrix of chart
  ggtitle("Central Limit Theorem of a binomial distribution\nTested with different sample sizes") +
  labs(
    caption = "\nBy Chen Zhu, 2024-03-18"
  ) +
  theme(legend.title=element_blank())

print(g)

# (2) Uniform Distribution
pmf <- c("uniform")
nosim <- 1000 # number of simulation in each step
m <- sapply(n , function(n) { # sapply is just a for()
  
  x <- if(pmf=="normal") {
    rnorm(nosim * n, sd=sd)
  } else if(pmf=="uniform") {
    runif(nosim * n)
  } else if(pmf=="binomial") {
    sample(0:1, nosim * n, replace=TRUE, prob=c(p, 1-p))
  }
  
  apply(matrix(x, nosim), 1, mean)
})
colnames(m) <- n # n is my main variable, I have to attach it as data label

# plot use ggplot2:
g <- ggplot(x, aes(mean, fill=as.factor(sampleSize))) + # Metadata
  geom_histogram(aes(y = ..density..)) + # Histogram bars
  geom_density() + # Density lines
  facet_wrap(~sampleSize) + # Broke in a matrix of chart
  ggtitle("Central Limit Theorem of a uniform distribution\nTested with different sample sizes") +
  labs(
    caption = "\nBy Chen Zhu, 2024-03-18"
  ) +
  theme(legend.title=element_blank())

print(g)



# (3) Normal Distribution
pmf <- c("normal")
nosim <- 1000 # number of simulation in each step
m <- sapply(n , function(n) { # sapply is just a for()
  
  x <- if(pmf=="normal") {
    rnorm(nosim * n, sd=sd)
  } 
  
  apply(matrix(x, nosim), 1, mean)
})
colnames(m) <- n # n is my main variable, I have to attach it as data label

# plot use ggplot2:
g <- ggplot(x, aes(mean, fill=as.factor(sampleSize))) + # Metadata
  geom_histogram(aes(y = ..density..)) + # Histogram bars
  geom_density() + # Density lines
  facet_wrap(~sampleSize) + # Broke in a matrix of chart
  ggtitle("Central Limit Theorem of a normal distribution\nTested with different sample sizes") +
  labs(
    caption = "\nBy Chen Zhu, 2024-03-18"
  ) +
  theme(legend.title=element_blank())

print(g)



## -------- Example 2 ------------------
f <- function(n, r=rnorm,  n.sim=1e3, name="Normal", ...) {
  sapply(n, function(n) {
    x <- scale(colMeans(matrix(r(n*n.sim, ...), n))) # Sample, take mean, standardize
    hist(x, sub=name, main=n, freq=FALSE, breaks=30) # Plot distribution
    curve(dnorm(x), col="Red", lwd=2, add=TRUE)      # Compare to standard Normal
  })
}
n <- c(5,20,100,500)
mfrow.old <- par(mfrow=c(4,length(n)))
f(n)
f(n, rgamma, shape=1/2, name="Gamma(1/2)")
f(n, function(n) runif(n) < 0.9, name="Bernoulli(9/10)")
f(n, rt, df=1, name="Cauchy")
par(mfrow=mfrow.old)


## -------- Example 3 ------------------
require(ggplot2)
set.seed(7759)

# Create a non-uniform population of 100,000 numbers between 1 and 100
pop1 <- rnorm(20000, mean = 10, sd = 3)
pop2 <- rnorm(80000, mean = 70, sd = 10)
pop <- c(pop1, pop2)

mu <- mean(pop) #calculate the population mean
sigma <- sd(pop) #calculate the population standard deviation
rm(pop1, pop2) #clean up


popdf <- as.data.frame(pop)
hg <- ggplot(popdf, aes(x = pop)) + geom_histogram(colour = "black", fill = "steelblue") + 
  ggtitle("Histogram of Population") + xlab("value")
hg

# Population mean μ=58.01
# Standard deviation σ=25.65


# Simulation
n <- c(1, 5, 10, 30, 50, 100) #set up number of samples
t <- c(10, 100, 1000, 10000) #set up number of trials in simulation

df <- data.frame() #initialize our empty data frame

# Run the simulation
for(i in n) { #for each value of n...
  col <- c()
  for(j in t) { #we loop through each value of t...
    trial <- 1:j
    counter <- j #set up an egg timer based on whichever t value we're on
    value <- c()
    while(counter > 0) {    # and extract n samples from the population...
      bucket <- sample(pop, i, replace = TRUE)
      xbar <- mean(bucket) #calculate the mean...
      value <- c(value, xbar) # and add it to a vector
      counter <- counter - 1 #egg timer counts down and loops back until it hits 0
    }
    sbar <- sd(value) #calculate the standard deviation of our sample
    col <- cbind(trial, value, sbar, i, j) #stick all the info together...
    df <- rbind(df, col) #and attach it to our master data frame
  } #and we do it again for the next set of values until we're done!
  
}

rm(col, bucket, value, counter, i, j, n, sbar, t, xbar, trial) #clean up

# Let's take a look!
str(df)

# We tidy up our data frame to get it ready for graphing. Note that we built it in "tall"
# form so it's already structured for ggplot

names(df) <- c("trial#", "value", "sdev", "samples", "trials")

# Creating the plot
g <- ggplot(df, aes(x = value)) + geom_density(fill = "steelblue") + 
  facet_grid(samples ~ trials, labeller = label_both) + 
  ggtitle("Demonstrating The Central Limit Theorem With Simulation") + 
  geom_vline(xintercept = mu, linetype = "dashed")
g

