# Singular Value Decomposition of an Image
# Jason Dean
# March, 2017

imagematrix <- function(mat, type=NULL, ncol=dim(mat)[1], nrow=dim(mat)[2],
                        noclipping=FALSE) {
  if (is.null(dim(mat)) && is.null(type)) stop("Type should be specified.")
  if (length(dim(mat)) == 2 && is.null(type)) type <- "grey"
  if (length(dim(mat)) == 3 && is.null(type)) type <- "rgb"
  if (type != "rgb" && type != "grey") stop("Type is incorrect.")
  if (is.null(ncol) || is.null(nrow)) stop("Dimension is uncertain.")
  imgdim <- c(ncol, nrow, if (type == "rgb") 3 else NULL)
  if (length(imgdim) == 3 && type == "grey") {
    # force to convert grey image
    mat <- rgb2grey(mat)
  }
  if (noclipping == FALSE && ((min(mat) < 0) || (1 < max(mat)))) {
    warning("Pixel values were automatically clipped because of range over.") 
    mat <- clipping(mat)
  }
  mat <- array(mat, dim=imgdim)
  attr(mat, "type") <- type
  class(mat) <- c("imagematrix", class(mat))
  mat
}

print.imagematrix <- function(x, ...) {
  x.dim <- dim(x)
  cat("size: ", x.dim[1], "x", x.dim[2], "\n")
  cat("type: ", attr(x, "type"), "\n")
}

plot.imagematrix <- function(x, ...) {
  colvec <- switch(attr(x, "type"),
                grey=grey(x),
                rgb=rgb(x[,,1], x[,,2], x[,,3]))
  if (is.null(colvec)) stop("image matrix is broken.")
  colors <- unique(colvec)
  colmat <- array(match(colvec, colors), dim=dim(x)[1:2])
  image(x = 0:(dim(colmat)[2]), y=0:(dim(colmat)[1]),
        z = t(colmat[nrow(colmat):1, ]), col=colors,
        xlab="", ylab="", axes=FALSE, asp=1, ...)
}

imageType <- function(x) {
  attr(x, "type")
}

rgb2grey <- function(img, coefs=c(0.30, 0.59, 0.11)) {
  if (is.null(dim(img))) stop("image matrix isn't correct.")
  if (length(dim(img))<3) stop("image matrix isn't rgb image.")
  imagematrix(coefs[1] * img[,,1] + coefs[2] * img[,,2] + coefs[3] * img[,,3],
              type="grey")
}
clipping <- function(img, low=0, high=1) {
  img[img < low] <- low
  img[img > high] <- high
  img
}
normalize <- function(img) {
  (img - min(img))/(max(img) - min(img))
}

# read the image
jTree <- readJPEG('jTree.JPG')
plot(imagematrix(jTree))

# imagematrix:  https://github.com/cran/ReadImages/blob/master/R/imagematrix.R

# break the image down into it's color components.  
red <- jTree[,,1]
green <- jTree[,,2]
blue <- jTree[,,3]

#calculate the singular value decomposition  
red.svd <- svd(red)
green.svd <- svd(green)
blue.svd <- svd(blue)

plot(seq(1,length(red.svd$d)), red.svd$d, xlab="singluar value (red)", ylab="value")

# there are many singular values, however  only a small fraction of them contribute to their sum total value
# this is further visualized below
redD.cdf <- ecdf(red.svd$d)
blueD.cdf <- ecdf(blue.svd$d)
greenD.cdf <- ecdf(green.svd$d)

plot(redD.cdf,xlim=c(0,20), col='red', xlab="singluar value", ylab="cumulative fraction", main='')
plot(blueD.cdf, add=TRUE, col='blue')
plot(greenD.cdf, add=TRUE, col='green')

#create a diagonal matrix containing singular values.
red.svd$d <- diag(red.svd$d)
green.svd$d <- diag(green.svd$d)
blue.svd$d <- diag(blue.svd$d)

#reconstruct the image using only the first singular value.
red1D <- red.svd$d
red1D[2:nrow(red1D),] <-0
red1 <- red.svd$u%*%red1D%*%t(red.svd$v)

blue1D <- blue.svd$d
blue1D[2:nrow(blue1D),] <-0
blue1 <- blue.svd$u%*%blue1D%*%t(blue.svd$v)

green1D <- green.svd$d
green1D[2:nrow(green1D),] <-0
green1 <- green.svd$u%*%green1D%*%t(green.svd$v)

#process the decomposed, reconstructed image using the imagematrix function.  
jTree.small <- jTree
jTree.small[,,1] <- red1
jTree.small[,,2] <- green1
jTree.small[,,3] <- blue1
jTree.small <- imagematrix(jTree.small)
plot(jTree.small, useRaster=TRUE)

#reconstruct the image using three singular values.  
red1D <- red.svd$d
red1D[4:nrow(red1D),] <-0
red1 <- red.svd$u%*%red1D%*%t(red.svd$v)

blue1D <- blue.svd$d
blue1D[4:nrow(blue1D),] <-0
blue1 <- blue.svd$u%*%blue1D%*%t(blue.svd$v)

green1D <- green.svd$d
green1D[4:nrow(green1D),] <-0
green1 <- green.svd$u%*%green1D%*%t(green.svd$v)

jTree.small <- jTree
jTree.small[,,1] <- red1
jTree.small[,,2] <- green1
jTree.small[,,3] <- blue1

jTree.small <- imagematrix(jTree.small)
plot(jTree.small, useRaster=TRUE)

#reconstruct the image with eight singular values
red1D <- red.svd$d
red1D[9:nrow(red1D),] <-0
red1 <- red.svd$u%*%red1D%*%t(red.svd$v)

blue1D <- blue.svd$d
blue1D[9:nrow(blue1D),] <-0
blue1 <- blue.svd$u%*%blue1D%*%t(blue.svd$v)

green1D <- green.svd$d
green1D[9:nrow(green1D),] <-0
green1 <- green.svd$u%*%green1D%*%t(green.svd$v)

jTree.small <- jTree
jTree.small[,,1] <- red1
jTree.small[,,2] <- green1
jTree.small[,,3] <- blue1

jTree.small <- imagematrix(jTree.small)
plot(jTree.small, useRaster=TRUE)
```

# and with fifteen singular values.....
red1D <- red.svd$d
red1D[16:nrow(red1D),] <-0
red1 <- red.svd$u%*%red1D%*%t(red.svd$v)

blue1D <- blue.svd$d
blue1D[16:nrow(blue1D),] <-0
blue1 <- blue.svd$u%*%blue1D%*%t(blue.svd$v)

green1D <- green.svd$d
green1D[16:nrow(green1D),] <-0
green1 <- green.svd$u%*%green1D%*%t(green.svd$v)

jTree.small <- jTree
jTree.small[,,1] <- red1
jTree.small[,,2] <- green1
jTree.small[,,3] <- blue1

jTree.small <- imagematrix(jTree.small)
plot(jTree.small, useRaster=TRUE)

#and finally with 100 singular values.  
red1D <- red.svd$d
red1D[101:nrow(red1D),] <-0
red1 <- red.svd$u%*%red1D%*%t(red.svd$v)

blue1D <- blue.svd$d
blue1D[101:nrow(blue1D),] <-0
blue1 <- blue.svd$u%*%blue1D%*%t(blue.svd$v)

green1D <- green.svd$d
green1D[101:nrow(green1D),] <-0
green1 <- green.svd$u%*%green1D%*%t(green.svd$v)

jTree.small <- jTree
jTree.small[,,1] <- red1
jTree.small[,,2] <- green1
jTree.small[,,3] <- blue1

jTree.small <- imagematrix(jTree.small)
plot(jTree.small, useRaster=TRUE)
