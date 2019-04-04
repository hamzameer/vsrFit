library(osqp) # quadratic programming package
vsrfit <- function(rdata.obj,
                   var.sparse = FALSE,
                   gamma = NULL,
                   lambda1 = NULL,
                   lambda2 = 0,
                   crossvalidate = TRUE,
                   nfolds = 5,
                   n.toprules = 10,
                   seed = 0,
                   k = NULL,
                   ...)
{
  if (is.null(lambda1) & crossvalidate == FALSE){
    stop("If lambda1 is not mentioned then it has to be calculated via cross-validation")
  }

  # for ease of access
  family = rdata.obj$rules.obj$family
  var.sparse = rdata.obj$rules.obj$var.sparse
  var.ruleinfo = rdata.obj$rules.obj$var.ruleinfo
  rdata = rdata.obj$rdata
  nfolds = rdata.obj$rules.obj$nfolds
  nvar = rdata.obj$rules.obj$nvar
  nc = ncol(rdata)
  # osqp solver settings
  settings <- osqpSettings(verbose = FALSE, eps_abs=1e-20, eps_rel = 1e-20, max_iter = 5000)

  # function for extracting groups
  if(var.sparse){
    groups <- extractgroups(rule.exec, nvar, family)
    # adjust groups:
    for(i in 1:nvar){
      groups[[i]] <- (sqrt(sum(groups[[i]] == 1)))*groups[[i]]
    }
  }

  if (family == 'surv'){
    ## Steps
    # cross-validate to find the optimal gamma, lambda = 0
    if (is.null(gamma)){
      gamma <- cv.gamma(rdata,nfolds,settings)
      gamma.star <- gamma$gamma.star
      gamma.values <- gamma$gamma.values
      gamma.errors <- gamma$error.values
    }else{
      gamma.star = gamma
      gamma.values = NULL
      gamma.errors = NULL
    }

    # if var.sparse = FALSE, use this gamma, and either custom gamma or do cv to find lambda
    if (var.sparse == FALSE){
      if (is.null(lambda1)){
        lambda1.star <- cv.lambda(rdata, gamma.star, lambda1,settings)
      }else{
        if (length(lambda1) > 1){
          lambda1.cv <- cv.lambda(rdata, gamma.star, lambda1,settings)
          lambda1.star <- lambda1.cv$lambda1.star
          lambda1.values <- lambda1.cv$lambda1.values
          lambda1.errors <- lambda1.cv$error.values
        }else{
          lambda1.star <- lambda1
          lambda1.values <- NULL
          lambda1.errors <- NULL
        }
      }
      beta <- solve.qp.gl(rdata, k = NULL, seed = 0,gamma = gamma.star, lambda = lambda1.star, settings)

    }else{
      # var.sparse = TRUE
      stop('Rules sparse in variables will be implemented in near future; Please set var.sparse = FALSE')
    }

    # list out the top so many rules? how is this number decided? n.toprules along with rulenumbers!
    # return gamma.analysis, gamma*, lambda (if cv done), rules extracted
  }
  # beta.star <- which(beta > 100)-1
  # active.rules <- rule.exec[beta.star]

  res <- list()
  res$beta <- beta
  res$rules <- rdata.obj$rules.obj$rules
  res$gamma.star <- gamma.star
  res$gamma.values <- gamma.values
  res$gamma.errors <- gamma.errors
  res$lambda1.star <- lambda1.star
  res$lambda1.errors <- lambda1.errors
  # res$active.rules <- active.rules
  res
}









