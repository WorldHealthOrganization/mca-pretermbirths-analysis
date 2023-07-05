#' Calculate Basis Splines
#'
#' Calculate basis splines (B-Splines) for use in P-splines
#'
#' @param x.i vector of x-values (without NAs) for which splines need to be calculated (determines the number of rows of B.ik)
#' @param x0 x-value which determines knot placement. By default, knot is placed half-interval before last observation
#' @param I interval length between two knots during observation period
#' @param degree degree of splines used. Currently tested only with degree 3
#' @export
#' @return A list containing: a matrix of B-spline values; and a vector of knot locations.
#' @seealso \code{\link{GetPSplines}}
#' @examples
#' ## visualize
#' x.i <- seq(1, 55, 0.025)
#' res <- GetSplines(x.i, I = 2.5)
#' dim(res$B.ik)
#' K <- length(res$knots.k); K
#' plot(res$B.ik[,1] ~ x.i, type= "n", xlab = "time", ylim = c(0,1), ylab = "Splines", xlim = range(x.i))
#' abline(v=res$knots.k, col = seq(1, K), lwd = 1)
#' for (k in 1:K){
#'  lines(res$B.ik[,k]~ x.i, type= "l", col = k, lwd = 1)
#'  }
GetSplines <- function(
  x.i,
  x0 = NULL,
  I = 2.5,
  degree=3
) {
  if (is.null(x0)) {
    x0 <- max(x.i)-0.5*I
  }
  # get knots, given that one knot needs to be in year0
  knots <- seq(x0-1000*I, x0+1000*I, I)
  while (min(x.i) < knots[1]) knots <- c(seq(knots[1]-1000*I, knots[1]-I,I), knots)
  while (max(x.i) > knots[length(knots)]) knots <- c(knots, seq(knots[length(knots)]+I,
                                                                knots[length(knots)]+1000*I, I))
  Btemp.ik <- bs(x.i, knots = knots[-c(1, length(knots))],  degree = degree,
                 Boundary.knots = knots[c(1, length(knots))])
  indicesofcolswithoutzeroes <- which(apply(Btemp.ik, 2, sum) > 0)
  # only remove columns with zeroes at start and end
  startnonzerocol <- indicesofcolswithoutzeroes[1]
  endnonzerocol <- indicesofcolswithoutzeroes[length(indicesofcolswithoutzeroes)]
  B.ik <- Btemp.ik[,startnonzerocol:endnonzerocol]
  colnames(B.ik) <- paste0("spline", seq(1, dim(B.ik)[2]))
  knots.k <- knots[startnonzerocol:endnonzerocol]
  names(knots.k) <- paste0("spline", seq(1, dim(B.ik)[2]))
  ##value<< List of B-splines containing:
  return(list(B.ik = B.ik, ##<< Matrix, each row is one observation, each column is one B-spline.
              knots.k = knots.k ##<< Vector of knots.
  ))
}


#THIS IS FROM THE DISTORTR PACKAGE - included 

getSplinesData <- function(nyears.c,
                           niso,
                           order,
                           I = 2.5){
  if(order!=1&order!=2){
    stop("Order of penalization must either be 1 or 2.")
  }
  max.nyears <- max(nyears.c)
  # get max dimensions
  x.t <- 1:max.nyears
  sp <- GetSplines(x.t, I = I)
  K <- length(sp$knots.k)
  B.tk <- sp$B.ik
  # this is the maximum dimensions of the basis splines.
  max.dim.B <- dim(B.tk)
  len.t <- max.dim.B[1]
  len.k <- max.dim.B[2]
  # for order =1 , H = k - 1
  # for order = 2 H = k-2
  # following from this, for order = 1, dim(Z.ih) will be t x (k-1)
  # for order = 2, dim(Z.ih) will be t x (k-2)
  
  K.c <- rep(NA, niso)
  H1.c <-rep(NA, niso)
  H2.c <-rep(NA, niso)
  B.tkc<- array(NA, c(len.t, len.k, niso))
  Z1.tkc<- array(0, c(len.t, len.k-1, niso)) #first diff
  Z2.tkc<- array(0, c(len.t, len.k-2, niso)) #second diff
  Delta1comb.khc <- array(NA, c(len.k, len.k-1, niso)) #first diff
  Delta2comb.khc <- array(NA, c(len.k, len.k -2, niso)) #second diff
  G.kdc <-  array(NA, c(len.k, 2, niso)) #second diff
  BG.tdc <-  array(NA, c(len.t, 2, niso)) #second diff
  
  for(i in 1:niso){
    x.t <- 1:nyears.c[i]
    sp <- GetSplines(x.t, I = I)
    K <- length(sp$knots.k)
    B.tk <- sp$B.ik
    B.tkc[1:nrow(B.tk),1:ncol(B.tk),i] <- B.tk
    K.c[i] <- K
    H1.c[i] <- K-1
    H2.c[i] <- K-2
    # stuff for reparameterization
    ## first difference
    Delta.hk <- diff(diag(K), diff = 1)
    Delta1comb.kh <- t(Delta.hk)%*%solve(Delta.hk%*%t(Delta.hk))
    Delta1comb.khc[1:K, 1:(K-1), i] <- Delta1comb.kh
    Z.ih <-B.tk%*%Delta1comb.kh
    Z1.tkc[1:nrow(B.tk),1:(ncol(B.tk)-1),i ] <- Z.ih
    ## second difference
    Delta.hk <- diff(diag(K), diff = 2) # difference matrix
    Delta2comb.kh <- t(Delta.hk)%*%solve(Delta.hk%*%t(Delta.hk))
    Delta2comb.khc[1:K, 1:(K-2), i] <- Delta2comb.kh
    Z.ih <-B.tk%*%Delta2comb.kh
    Z2.tkc[1:nrow(B.tk),1:(ncol(B.tk)-2),i ] <- Z.ih
    G.kd <- cbind(rep(1, K), seq(1, K)-K/2)
    G.kdc[1:K,,i] <-  G.kd
    BG.td <- B.tk%*%G.kd
    BG.tdc[1:nrow(B.tk), ,i] <- BG.td
  }
  if(order==1){
    return(list(K.c = K.c, H.c = H1.c, H = max(H1.c),B.tkc = B.tkc, Z.tkc = Z1.tkc))
  }
  if(order==2){
    return(list(K.c = K.c, H.c = H2.c, H = max(H2.c), D = 2, B.tkc = B.tkc, Z.tkc = Z2.tkc, BG.tdc = BG.tdc))
  }
}