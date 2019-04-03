generateRuleData <-function(rule_exec, data, family){
  'This function generates a dataframe of rules for all data instance'
  RuleData <-data.frame()
  # datacol <- ncol(data)
  nrule <- length(rule_exec)
  for(i in 1:nrow(data)){
    ruleX <- genRuleX(rule_exec, data[i,], nrule)
    RuleData <- rbind(RuleData, ruleX)
  }
  colnames_list <- c('intercept')
  # if(family != 'surv'){
    # colnames_list <- c('intercept')
  # }else{
    # RuleData <- RuleData[2:(nrule+1)]
    # colnames_list <- c()
  # }

  for(i in 1:nrule){
    rule_label <- paste('rule', i, sep = "")
    colnames_list <-c(colnames_list, rule_label)
  }
  # intercept <- matrix
  # RuleData <- cbind(intercept, RuleData)
  if(family == 'surv'){
    colnames(RuleData) <- colnames_list
    # RuleData <- cbind(time = data[,1], status = as.numeric(data[,2]), RuleData)
    RuleData <- cbind(time = data[,1], status = as.numeric(data[,2]), RuleData)
  }else{
    colnames(RuleData) <- colnames_list
    RuleData <- cbind(yvar = data[,1], RuleData[,2:ncol(RuleData)])
  }
}

evaluateCondition <- function(condition, X){
  cond = 0
  X = X
  if(eval(parse(text = condition))){
    cond = 1
  }
  cond
}

evaluateRule <- function(rule, x){
  ' This function evaluates a rule at an observation'
  X <- x[2:ncol(x)]
  rul = 1
  conditions = unlist(strsplit(rule, '& '))
  for(i in 1:length(conditions)){
    cond = evaluateCondition(conditions[i], X)
    rul = rul*cond
    if(rul == 0) break
  }
  rul
}

genRuleX<-function(rule_exec, x, nrule){
  xdf <- c(1)
  for(j in 1:nrule){
    res <- evaluateRule(rule_exec[j], x)
    xdf <- c(xdf, res)
  }
  xdf
}



