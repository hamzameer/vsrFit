cv.gamma <- function(rdata,folds,settings){

  gamma.values <- c(5e-6, 1e-5, 5e-5, 1e-4, 5e-4, 1e-3, 5e-3, 1e-2)
  errors <- rep(0,8)
  lambda = 0
  for(i in 1:length(gamma.values)){
    gamma = gamma.values[i]
    for(k in 1:folds){
      #test
      rdata.complete <- rdata[rdata$status == 1,]
      rdata.cens <- rdata[rdata$status == 0,]
      r = nrow(rdata.complete)
      test <- ((k-1)*r/folds+1):(k*r/folds)
      rdata.test <- rdata.complete[test,]
      #train
      rdata.train <- rbind(rdata.complete[-test,],rdata.cens)
      #model
      beta <- solve.qp.gl(rdata.train, k = NULL, seed = 0,gamma,lambda, settings)
      error <- sae.error(beta,rdata.test)
      errors[i] <- errors[i]+error
    }
  }
  index <- which(errors == min(errors))
  gamma.star <- gamma.values[index]
  res <- list()
  res$gamma.star <- gamma.star
  res$gamma.values <- gamma.values
  res$error.values <- errors
  res
}


cv.lambda <- function(rdata, gamma.star, lambda1,settings){

  # no folds | simple cv over the same rdata.test etc.
  if(is.null(lambda1)){
    lambda1 <- c(1,10,50,75,100,200,500,1000)
    errors <- rep(0,8)
    rdata.complete <- rdata[rdata$status == 1,]
    rdata.cens <- rdata[rdata$status == 0,]
    r = nrow(rdata.complete)
    test <- sample(1:r,r/5)
    rdata.test <- rdata.complete[test,]
    #train
    rdata.train <- rbind(rdata.complete[-test,],rdata.cens)
    #model
    for(i in 1:length(lamda1)){
      lambda = lambda1.values[i]
      beta <- solve.qp.gl(rdata.train, k = NULL, seed = 0,gamma.star,lambda, settings)
      error <- sae.error(beta,rdata.test)
      errors[i] <- errors[i]+error
    }

  }else{
    errors <- rep(0,length(lambda1))
    rdata.complete <- rdata[rdata$status == 1,]
    rdata.cens <- rdata[rdata$status == 0,]
    r = nrow(rdata.complete)
    test <- sample(1:r,r/5)
    rdata.test <- rdata.complete[test,]
    #train
    rdata.train <- rbind(rdata.complete[-test,],rdata.cens)
    #model
    for(i in 1:length(lambda1)){
      lambda = lambda1[i]
      beta <- solve.qp.gl(rdata.train, k = NULL, seed = 0,gamma.star,lambda, settings)
      error <- sae.error(beta,rdata.test)
      errors[i] <- error
    }
  }
  index <- which(errors == min(errors))
  lambda1.star <- lambda1[index]
  res <- list()
  res$lambda1.star <- lambda1.star
  res$lambda1.values <- lambda1
  res$error.values <- errors
  res
}





# cv.gamma <- function(rdata,nc, gamma.guess){
#
#   # find the best value of gamma for the solution where time is predicted linearly
#   # divide into test and train set
#   set.seed(3)
#   test <- sample(1:nrow(rdata), nrow(rdata)/4)
#   rdata.train <- rdata[-test,]
#   rdata.train <- rdata[-test,][1:3000,]
#   rdata.test <- rdata[test,]
#   test.events <- rdata.test[,2] == 1
#   g.guess <- c(-12,-10,-8, -6, -5, -4,-3,-2)
#   g.guess <- c(0,2**(g.guess))
#   min.error = 10e10
#   nz = sum(rdata.train$status == 0)
#   cens <- which(rdata.train[,2] == 0)
#   rdata.train.cens <- rdata.train[cens,]
#   rdata.train.complete <- rdata.train[-cens,]
#   y.cens <- rdata.train.cens[,1]
#
#   for(gamma in g.guess){
#
#     beta <- Variable(nc-2)
#     z <- Variable(nz)
#     yhat <- rdata.train.cens[,3:(nc)]%*%beta
#     loss <- surv.loss.func(rdata.train.complete, gamma, beta, z, nc)
#     # obj <- loss + lasso(beta, lambda)
#     # obj <- loss +overlapping.glasso(beta, lambda1, lambda2, groups, nvar)
#     obj <- loss #+overlapping.glasso(beta, lambda1, lambda2, groups, nvar)
#
#     constraints <- list(z <= 0, z <= yhat - y.cens)
#     prob <- Problem(Minimize(obj),constraints = constraints)
#     tryCatch({
#       result <- solve(prob)
#     },error = function(e){next})
#
#     # result$status
#     beta.t <- result$getValue(beta)
#     that <- predict.surv.linear(rdata.test,beta.t,nc)
#     t <- rdata.test[,1]
#     status = rdata.test$status
#     error1 <- frac.error(that, t, status)
#     error <- c(error,error1)
#     g <- c(g,gamma)
#     # if (error < min.error){
#     #   gamma.min = gamma
#     #   min.error = error
#     # }
#   }
#   gamma.min
# }
# error
# # 13.99018
#
# gamma.star = 0.008
# nc
#
# cv.linsurv.lambda <- function(rdata,gamma.star,nc){
#
#   # find the best value of gamma for the solution where time is predicted linearly
#   # divide into test and train set
#   set.seed(3)
#   test <- sample(1:nrow(rdata), nrow(rdata)/4)
#   rdata.train <- rdata[-test,]
#   rdata.train <- rdata[-test,][1:2000,]
#   rdata.test <- rdata[test,]
#   test.events <- rdata.test[,2] == 1
#
#   min.error = 10e10
#   nz = sum(rdata.train$status == 0)
#   cens <- which(rdata.train[,2] == 0)
#   rdata.train.cens <- rdata.train[cens,]
#   rdata.train.complete <- rdata.train[-cens,]
#   y.cens <- rdata.train.cens[,1]
#
#   for(gamma in g.guess){
#
#     beta <- Variable(nc-2)
#     z <- Variable(nz)
#     yhat <- rdata.train.cens[,3:(nc)]%*%beta
#     loss <- surv.loss.func(rdata.train.complete, 0.01, beta, z, nc)
#     obj <- loss + lasso(beta, lambda)
#     obj <- loss +overlapping.glasso(beta, lambda, lambda2, groups, nvar)
#
#     constraints <- list(z <= 0, z <= yhat - y.cens)
#     prob <- Problem(Minimize(obj),constraints = constraints)
#     result <- solve(prob)
#     tryCatch({
#       # result <- solve(prob)
#     },error = function(e){next})
#
#     result$status
#     beta.t <- result$getValue(beta)
#     sum(beta.t > 1000)
#
#
#
#     that <- predict.surv.linear(rdata.test,beta.t,nc)
#     t <- rdata.test[,1]
#     status = rdata.test$status
#     error1 <- frac.error(that, t, status)
#     error <- c(error,error1)
#     g <- c(g,gamma)
#
#   }
# }
#
# cv.linsurv.lambda1<-function(rdata,gamma,nc,lambda2){
#
#   set.seed(3)
#   test <- sample(1:nrow(rdata), nrow(rdata)/7)
#   rdata.train <- rdata[-test,][1:2000,]
#   rdata.test <- rdata[test,]
#
#   test.events <- rdata.test[,2] == 1
#   # nc <- ncol(rdata)
#   l.guess <- c(-10,-6,-3,-2,-1,-0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,6,7,8,9)
#   l.guess <- 1.3**(l.guess)
#   # l.guess
#   min.error = 10e10
#   for(lambda1 in l.guess){
#     beta <- Variable(nc-1)
#     loss <- survival.sse(rdata.train,gamma,beta)
#     obj <- loss #+ lasso(beta, lambda)
#     prob <- Problem(Minimize(obj))
#     result <- solve(prob,solver = 'ECOS')
#     beta <- result$getValue(beta)
#
#     # test predictions
#     that <- predict.surv.linear(rdata.test,beta,nc)
#     t <- rdata.test[,1][test.events]
#     that <- that[test.events]
#     error <- norm(as.matrix(t-that),"1")
#     if (error < min.error){
#       lambda1.min = lambda1
#       min.error = error
#     }
#   }
#   lambda1.min
# }
#
# survival.sse <- function(X,gamma,beta){
#   # here beta <- variable(n-1)
#   n = ncol(X)
#   m = nrow(X)
#   inter <- rep(1,m)
#   cens <- which(X[,2] == 0)
#   X <- cbind(X[,1:2],inter,X[,3:n])
#   X.cens <- X[cens,]
#   X.complete <- X[-cens,]
#   y.cens <- X.cens[,1]
#   y.complete <- X.complete[,1]
#
#   # this is the first term in the loss function
#   sse <- 0.5*sum((X.complete[,3:(n+1)]%*%beta - y.complete )^2)
#
#   # the hinge loss
#
#   x <- X.cens[,3:(n+1)]%*%beta - y.cens
#   z <- rep(0,nrow(X.cens))
#   l <- min_elemwise(x,z)
#   hinge <- sum(l^2)
#   sse + (gamma/2)*hinge
# }
#
#
# survival.sse <- function(X,gamma,beta){
#   # here beta <- variable(n-1)
#   n = ncol(X)
#   m = nrow(X)
#   inter <- rep(1,m)
#   cens <- which(X[,2] == 0)
#   X <- cbind(X[,1:2],inter,X[,3:n])
#   X.cens <- X[cens,]
#   X.complete <- X[-cens,]
#   y.cens <- X.cens[,1]
#   y.complete <- X.complete[,1]
#   remove(X)
#   # this is the first term in the loss function
#   sse <- 0.5*cvxr_norm(X.complete[,3:(n+1)]%*%beta - y.complete )^2
#   sse
#   # # the hinge loss
#   x <- X.cens[,3:(n+1)]%*%beta - y.cens
#   x <- min_elemwise(rep(0,nrow(X.cens)),x)
#
#   # z <- rep(0,nrow(X.cens))
#   # l <- min_elemwise(x,z)
#   # hinge <- sum(l^2)
#   hinge <- cvxr_norm(x)^2
#   sse + (gamma/2)*hinge
# }
#
# a <- c(-5,2,-3,-1)
# b <- Variable(4)
# prob <- Problem(Minimize(cvxr_norm(min_elemwise(a, b))))
# result <- solve(prob)
# result$value
#
# cv.linear.gamma <- function(rdata,nc, gamma.list){
#
#   # find the best value of gamma for the solution where time is predicted linearly
#   # divide into test and trains set
#   set.seed(3)
#   test <- sample(1:nrow(rdata), nrow(rdata)/5)
#   rdata.train <- rdata[-test,][1:4000,]
#   rdata.test <- rdata[test,][1:1000,]
#   test.events <- rdata.test[,2] == 1
#   # nc <- ncol(rdata)
#   g.guess <- c(-4,-3,-2,-1.5,-1,-0.5,1,1.5,2,2.5,3,3.5,4)
#   # g.guess <- c(5,6,8,10,14)
#   g.guess <- c(0,2**(g.guess))
#   min.error = 10e10
#   for(gamma in g.guess){
#     beta <- Variable(nc-1)
#     loss <- survival.sse(rdata.train,gamma,beta)
#     obj <- loss + lasso(beta, lambda)
#     obj <- loss
#     prob <- Problem(Minimize(obj))
#     result <- solve(prob)
#     result$status
#     beta <- result$getValue(beta)
#     beta
#     # test predictions
#     that <- predict.surv.linear(rdata.test,beta,nc)
#     t <- rdata.test[,1]
#     error <- frac.error(that, t, rdata.test$status)
#     error
#     if (error < min.error){
#       gamma.min = gamma
#       min.error = error
#     }
#   }
#   gamma.min
# }
# error
# # 13.99018
# # 0 | 0.7723036
# # 0.025 | 0.7723036
# # 0.05 | 0.7723036
# # 0.1
# # 0.2
# # 0.5
#
# # 0.1
#
