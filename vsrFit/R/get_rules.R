get_rules <- function(formula,
                      data,
                      ntree = 200,
                      var.sparse = FALSE,
                      nodesize = NULL, nodedepth = 3,
                      digit = 10,
                      model = TRUE,
                      seed = NULL,
                      family = 'surv',
                      ...)
{

  ## survival / regression / classification (classification is currently not implemented)
  formulaPrelim <- parseFormula(formula, data)
  ## data cannot be missing
  if (missing(data)) stop("data is missing")
  ## conduct preliminary formula validation
  if (missing(formula) | (!missing(formula) && is.null(formula))) {
    stop("formula is missing or null")
  }
  # save the call/formula for the return object
  my.call <- match.call()
  my.call$formula <- eval(formula)

  ## finalize the formula based on the pre-processed data
  formulaDetail <- finalizeFormula(formulaPrelim, data)

  ## coherence checks on option parameters
  ntree <- round(ntree)
  if (ntree < 1) stop("Invalid choice of 'ntree'.  Cannot be less than 1.")
  nodedepth = round(nodedepth)
  if (nodedepth < 1) stop("Invalid choice of 'nodedepth'.  Cannot be less than 1.")

  if (!is.logical(var.sparse)) stop("Invalid choice of 'var.sparse'. Must be logical. ")
  ## initialize the seed
  # seed <- get.seed(seed)

  ## save the family for convenient access
  family <- formulaDetail$family

  ## save the names for convenient access
  xvar.names <- formulaDetail$xvar.names
  yvar.names <- formulaDetail$yvar.names
  nvar <- length(xvar.names)
  ## reality check on x and y
  ## are there any x-variables?
  if (length(xvar.names) == 0) {
    stop("something seems wrong: your formula did not define any x-variables")
  }
  ## .. are there any y-variables?
  if (length(yvar.names) == 0) {
    stop("something seems wrong: your formula did not define any y-variables")
  }

  if (family != 'surv' & family != 'regression' & family != 'class'){
    stop("family should be one of 'surv', 'regression', or 'class'.")
  }

  if (family == 'regression' | family == 'class'){
    stop('This package will soon be extended to regression and classification.')
  }
  if (family == 'surv'){
    ## remove unmentioned or unneeded variables from the data set
    newdata <- data[,c(yvar.names,xvar.names)]
    remove(data)

    ## this step simply omits the observation with missing values in the dataset (NOTE)
    newdata <- parseMissingData(newdata)

    ## interestingly using a function from previously built package: replace with native and check
    rf.ranger = ranger(formula, newdata, num.trees = ntree)
    tree.list <- Ranger2List(rf.ranger)

    rule.exec1 <- unique(extractRules(tree.list,newdata, ntree = ntree, maxdepth = 1, digits=digit))
    rule.exec2 <- unique(extractRules(tree.list,newdata, ntree = ntree, maxdepth = 2, digits=digit))
    rule.exec3 <- unique(extractRules(tree.list,newdata, ntree = ntree, maxdepth = 3, digits=digit))

    if(length(rule.exec1) > 300){
      rule.exec1 <- sample(rule.exec1, 300)
    }
    if(length(rule.exec2) > 1000){
      rule.exec2 <- sample(rule.exec2, 1000)
    }
    if(length(rule.exec3) > 700){
      rule.exec3 <- sample(rule.exec3, 700)
    }
    rule.exec <- c(rule.exec1,rule.exec2,rule.exec3)
    remove(rule.exec1, rule.exec2, rule.exec3)
    ## select 2000 rules randomly from all the rules extracted (simply for convenience (NOTE))
    if(length(rule.exec) > 2000){
      rule.exec <- sample(rule.exec, 2000, replace = FALSE)
      print('A random subset of 2000 rules was selected')
    }

    # rdata <- generateRuleData(rule.exec, newdata, family)

    ## information about variables in the rules
    if(family == "surv"){
      var.ruleinfo <- 2:(ncol(newdata)-1)
      names(var.ruleinfo) <- colnames(newdata)[3:ncol(newdata)]
    }
  }
  ## what should be returned: rdata, family, groups, var.sparse, rules, var.ruleinfo, nfolds
  res <- list()
  res$rules <- rule.exec
  res$data <- newdata
  res$family <- family
  res$var.sparse <- var.sparse
  res$var.ruleinfo <- var.ruleinfo
  res$nvar <- nvar
  res$mycall <- my.call
  res
}

# rules.obj <- get_rules(formula, newdata, ntree = 200)


