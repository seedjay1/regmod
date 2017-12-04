# create sample from unit disk
set.seed(42) # for repro
numpoints <- 100
radius <- 1

points.disk_sample <- NULL
for (i in 1:numpoints)
{
  r <- radius * sqrt(runif(1)) # because area is proportional to r^2
  degs <- 360*runif(1)
  
  theta <- 2*pi*degs/360
  
  x <- r * sin(theta)
  y <- r * cos(theta)
  
  points.disk_sample[[i]] <-c(x, y)
}


# create unit circle points
points.circle_sample <- NULL
for (i in 1:numpoints)
{
  r <- radius 
  degs <- 360 * ((i-1)/(numpoints-1)) # to fill in the gap at 0 that would result going from 1:100
  
  theta <- 2*pi*degs/360
  
  x <- r * sin(theta)
  y <- r * cos(theta)
  
  points.circle_sample[[i]] <-c(x, y)
}


# create transformation T
T <- matrix(c(1, 2, 3, 2), nrow=2)


# calculate T(samples)
points.tdisk <- NULL
points.tcircle <- NULL
for (i in 1:numpoints)
{
  points.tdisk[[i]] <- T %*% points.disk_sample[[i]]
  points.tcircle[[i]] <- T %*% points.circle_sample[[i]]
}


# create plot datasets
points.plot.circle <- NULL
points.plot.disk <- NULL
points.plot.tcircle <- NULL
points.plot.tdisk <- NULL
for (i in 1:numpoints)
{
  # unit circle
  points.plot.circle$x[i] <- points.circle_sample[[i]][[1]]
  points.plot.circle$y[i] <- points.circle_sample[[i]][[2]]
  
  # unit disk
  points.plot.disk$x[i] <- points.disk_sample[[i]][[1]]
  points.plot.disk$y[i] <- points.disk_sample[[i]][[2]]
  
  # T(unit circle)
  points.plot.tcircle$x[i] <- points.tcircle[[i]][[1]]
  points.plot.tcircle$y[i] <- points.tcircle[[i]][[2]]
  
  # T(unit disk)
  points.plot.tdisk$x[i] <- points.tdisk[[i]][[1]]
  points.plot.tdisk$y[i] <- points.tdisk[[i]][[2]]
}

# plot unit circle w/sample
plot(x=points.plot.circle$x, y=points.plot.circle$y, asp=T, type="n")
lines(x=points.plot.circle$x, y=points.plot.circle$y, asp=TRUE)
points(x=points.plot.disk$x, y=points.plot.disk$y, asp=T)

# plot T(unit circle w/sample)
plot(x=points.plot.tcircle$x, y=points.plot.tcircle$y, asp=T, type="n")
lines(x=points.plot.tcircle$x, y=points.plot.tcircle$y, asp=T)
points(x=points.plot.tdisk$x, y=points.plot.tdisk$y, asp=T)


#======================
# ok the above illustrates a linear operator on R^2
# now lets see if we can
# 1) visualize the eigenvalues
# 2) reconstruct the operator from the circle/t(circle)
# 3) reconstruct the operator from the sample/t(sample) values
# 4) check out the svd/pca eigenvalues/vectors
#======================

mydata.orig <- as.data.frame(points.plot.disk)
mydata.trans <- as.data.frame(points.plot.tdisk)

pca.orig <- prcomp(mydata.orig)
pca.trans <- prcomp(mydata.trans)


