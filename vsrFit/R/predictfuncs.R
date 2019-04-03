predict.vsrfit <- function(beta, rdata){

  as.matrix(rdata[,3:ncol(rdata)]) %*% beta

}
