##replace with dynamic code using path[,,dim()]
dims <- read.csv("./data/guided_tour_dims.csv", header = FALSE, sep=" ")
dims <- dims[,c(1,3)]
dims[,3] <- sqrt(dims[,1]^2 + dims[,2]^2)
dims[,4] <- names(tour)
names(dims) <- c("x","y","distance","dimensions")

str(path)
head(str(attributes(path)))

attributes(path)
attr(path, "data")
attr(x,"dimnames")

cbind(path[,,dim(path)[3]], names(tour))
p=path[,,dim(path)[3]]
attributes(p) <- NULL
attributes(p)


x <- cbind(a = 1:3, pi = pi) # simple matrix with dimnames
attributes(x)
attr(x,"dimnames")
x[,,,2]
str(x)
x[[6]]
dimnames(path)

## strip an object's attributes:
attributes(x) <- NULL
x # now just a vector of length 6

mostattributes(x) <- list(mycomment = "really special", dim = 3:2,
                          dimnames = list(LETTERS[1:3], letters[1:5]), names = paste(1:6))
x # dim(), but not {dim}names
dimnames(x)

library(plyr)
head(arrange(dims,desc(distance)), n = 10)

top(dims, 10)
?order

dims
str(tour)
