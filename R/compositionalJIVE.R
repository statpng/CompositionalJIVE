#' @import r.jive
#' @export compositionalJIVE
compositionalJIVE <- function(X, nrank=2){
  d0 <- sum(svd(X)$d>1e-10)
  # if( nrank > (d0-1) ) stop("nrank should be smaller than or equal to rank(X)-1")

  n=nrow(X); p=ncol(X); mu=colMeans(X)
  uhat <- prcomp(X)$x[,1:nrank,drop=F]
  vhat <- prcomp(X)$rotation[,1:nrank,drop=F]
  xhat <- tcrossprod(rep(1,n), mu) + tcrossprod(uhat, vhat)

  return( list(mu=mu, uhat=uhat, vhat=vhat, xhat=xhat, X=X, type.projection="proj") )
}
