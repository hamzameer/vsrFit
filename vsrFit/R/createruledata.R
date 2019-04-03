create_ruledata <- function(rules.obj){

  newdata <- rules.obj$data
  family = rules.obj$family
  rule.exec <- rules.obj$rules
  rdata <- generateRuleData(rule.exec, newdata, family)

  x <- t(rdata[,4:ncol(rdata)])
  dups <- which(duplicated(x))

  ## get rid of duplicate columns and corresponding rules
  # tables=apply(rdata[,4:ncol(rdata)],2,table)
  # dups=which(duplicated(tables))
  print(paste(length(dups),'duplicate rules and corresponding columns were removed'))
  rdata.n <- rdata[,-(dups+3)]
  rules.n <- rule.exec[-dups]
  colnames(rdata.n)[4:ncol(rdata.n)] <- paste('rule', c(1:length(rules.n)), sep = "")
  ## what should be returned: rdata, family, groups, var.sparse, rules, var.ruleinfo, nfolds
  res <- list()
  res$rdata <- rdata.n
  res$rules.obj <- rules.obj
  res$rules.obj$rules <- rules.n
  res
}

# rules.obj <- get_rules(formula, newdata, ntree = 200)
# rdata.obj <- create_ruledata(rules.obj)
# rdata.obj$rdata
