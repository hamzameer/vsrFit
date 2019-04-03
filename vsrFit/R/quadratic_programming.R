solve.qp.gl <- function(rdata, k = NULL, seed = 0, gamma = 0, lambda = 0, settings){


  rdata.complete <- rdata[rdata$status ==1,]
  rdata.cens <- rdata[rdata$status ==0,]

  # if you want to deal with lesser number of censored observations
  if (is.null(k)){
    k = nrow(rdata.cens)
  }
  nc = ncol(rdata)
  m = nc - 2
  set.seed(seed)

  cens <- sample(1:nrow(rdata.cens), k)
  rdata.train.cens <- rdata.cens[cens,]

  #Dmat
  A <- rdata.complete[,3:nc]
  Atild <- as.matrix(t(A))%*%as.matrix(A)
  zmk <- matrix(0, m,k)
  zkm <- matrix(0, k, m)
  zmm <- matrix(0, m, m)
  # G <- gamma*diag(k)
  # Dmat <- rbind(cbind(Atild ,-Atild, zmk ), cbind(-Atild,Atild, zmk), cbind(zkm,zkm,G))

  #dvec
  t <- t(as.matrix(rdata.complete[,1]))
  zk <- matrix(0,ncol = k, nrow=1)
  zm <- matrix(0,ncol = m, nrow=1)
  tA <- t%*%as.matrix(A)
  lambda.bar <- lambda*rep(1,m)
  dvec <- cbind(lambda.bar - 2*tA, lambda.bar + 2*tA, zk)

  #Amat
  #bvec
  c <- rdata.train.cens[,1]
  B <- unname(as.matrix(rdata.train.cens[,3:nc]))
  ik <- unname(diag(k))
  im <- unname(diag(m))
  zkk <- matrix(0, k, k)
  x1 <- unname(cbind(im,zmm,zmk))
  x2 <- unname(cbind(zmm,im,zmk))
  x3 <- unname(cbind(zkm,zkm,-ik))
  x4 <- unname(cbind(B, -B, -ik))
  Amat <- rbind(x1,x2,x3,x4)
  bvec = as.matrix(c(zm,zm,zk,c))

  #gamma_update
  G <- gamma*diag(k)
  Dmat <- rbind(cbind(Atild ,-Atild, zmk ), cbind(-Atild,Atild, zmk), cbind(zkm,zkm,G))
  #lambda_update
  lambda.bar <- lambda*rep(1,m)
  dvec <- cbind(lambda.bar - 2*tA, lambda.bar + 2*tA, zk)
  osqp_solution <- solve_osqp(Dmat, dvec, Amat, l=bvec, pars=settings)
  beta.plus <- osqp_solution$x[1:m]
  beta.minus <- osqp_solution$x[(m+1):(2*m)]
  beta = beta.plus - beta.minus
  # beta.mod = beta.plus + beta.minus

  beta
}
