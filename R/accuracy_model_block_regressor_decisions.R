#### K. Garner, 2022. Free to share, please cite
#### This code comes as is, with no guarantees
####------------------------------------------------------
# here I am working out which kind of function of x may
# provide the best regressor/fit the shape of learning better
###-------------------------------------------------------

# first scale the block factor
b <- 1:8
x <- scale(b)
plot(b, x)

# now I'll try some functions on x
# first, logistic
L = 1
k = c(2, 10, 50, 1000)
x0 = 0

y = lapply(k, function(y) L/(1+exp(-y^(x-x0))))
plot(x, y[[1]], type='l', col='grey', ylim=c(0,1))
points(x, y[[2]], type='l', col='blue')
points(x, y[[3]], type='l', col='green')
points(x, y[[4]], type='l', col='purple')

# now I'll try some functions on x
# now I'll try sigmoid
k = c(.01, .05, 1, 1.5, 2, 3, 8, 10)
y =  lapply(k, function(i) 1/(1+exp(-i*(x+1.5))))
plot(x, y[[1]], type='l', col='grey', ylim=c(0,1))
lapply(2:length(k), function(j) points(x, y[[j]], type='l'))
# looks like the relationship between accuracy and block fits
# a sigmoidal function, which means 1 parameter to fit (per subject)
