parseFormula <- function(f,data){
  ## confirm coherency of the formula
  if (!inherits(f, "formula")) {
    stop("'formula' is not a formula object.")
  }
  if (is.null(data)) {
    stop("'data' is missing.")
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  ## pull the family and y-variable names
  fmly <- all.names(f, max.names = 1e7)[2]
  all.names <- all.vars(f, max.names = 1e7)
  yvar.names <- all.vars(formula(paste(as.character(f)[2], "~ .")), max.names = 1e7)
  yvar.names <- yvar.names[-length(yvar.names)]
  ## survival forests
  if ((fmly == "Surv")) {
    if (sum(is.element(yvar.names, names(data))) != 2) {
      stop("Survival formula incorrectly specified.")
    }
    family <- "surv"
  }else {
    ## must be a (univariate) regresssion or classification
    if (sum(is.element(yvar.names, names(data))) != 1) {
      stop("formula is incorrectly specified.")
    }
    Y <- data[, yvar.names]
    ## logicals are treated as 0/1 real (bug reported by John Ehrlinger)
    if (is.logical(Y)) {
      Y <- as.numeric(Y)
    }
    ## check whether we have a factor or a continuous variable
    if (!(is.factor(Y) | is.numeric(Y))) {
      stop("the y-outcome must be either real or a factor.")
    }
    if (is.factor(Y)) { #|| length(coerce.factor$yvar.names) == 1) {
      family <- "class"
    } else {
      family <- "regr"
    }
  }
  ## done: return the goodies
  return (list(all.names=all.names, family=family, yvar.names=yvar.names))
}


finalizeFormula <- function(formula.obj, data) {
  ## parse the formula object
  yvar.names <- formula.obj$yvar.names
  all.names  <- formula.obj$all.names
  index      <- length(yvar.names)
  fmly       <- formula.obj$family
  ## total number of variables should exceed number of yvars
  if (length(all.names) <= index) {
    stop("formula is misspecified: total number of variables does not exceed total number of y-variables")
  }
  ## extract the xvar names
  if (all.names[index + 1] == ".") {
    if(index == 0) {
      xvar.names <- names(data)
    }
    else {
      xvar.names <- names(data)[!is.element(names(data), all.names[1:index])]
    }
  }else {
    if(index == 0) {
      xvar.names <- all.names
    }
    else {
      xvar.names <- all.names[-c(1:index)]
    }
    not.specified <- !is.element(xvar.names, names(data))
    if (sum(not.specified) > 0) {
      stop("formula is misspecified, object ", xvar.names[not.specified], " not found")
    }
  }
  ## return the goodies
  return (list(family=fmly, yvar.names=yvar.names, xvar.names=xvar.names))
}


parseMissingData <- function(data) {
  ## if impute, use imputation techniques to fill in the missing values
  return(na.omit(data))
}

extractgroups <- function(rule.exec, m, family = "surv"){
  n = length(rule.exec)
  groups <- list()
  for(i in 1:m){
    groups[[i]] <- rep(0,n)
  }
  for(i in 1:n){
    rule = rule.exec[i]
    r <- strsplit(rule, '&')[[1]]
    rn <- length(r)

    vars <- c()
    for(j in 1:rn){
      r. <- r[j]
      z = strsplit(r., "]")[[1]][1]
      vars <- c(vars,as.numeric(substr(trimws(z),4,10)))
    }
    vars <- unique(vars)

    for(v in vars){
      if(family == "surv"){
        groups[[v-1]][i] = 1
      }else{
        groups[[v]][i] = 1
      }
    }
  }
  groups
}

# extractgroups(rule.exec[1:5],4,"surv")
l1norm <- function(a,b){
  sum(abs(a-b))
}

sae.error <- function(beta, rdata){
  pred <- as.matrix(rdata[,3:ncol(rdata)]) %*% beta
  actl <- rdata[,1]
  error <- l1norm(pred,actl)/length(pred)/max(actl)
  error
}

sum.squared.error <- function(x,y){

  sqrt(sum((x-y)**2)/length(x))
}




