#### Home-made functions ####


# mmodelstats: extracts relevent informaiton from the model: coefficient estimate, SE, and DF.
# It also retrieves the difference-based test's t and p values.
modelstats <- function(dat, beta){
  V1 <- dat[,1]
  V2 <- dat[,2]
  V3 <- dat[,3]
  V4 <- dat[,4]
  V5 <- dat[,5]
  y <- dat[,6]
  model <- lm(y~V1+V2+V3+V4+V5, dat)
  modsum <- summary(model)
  b <- model$coefficients[beta+1] #Beta weights estimates extraction
  se <-  modsum$coefficients[beta+1,"Std. Error"] #Standard error extraction per predictor
  se
  df <- model$df.residual
  t <- modsum$coefficients[beta+1,"t value"]
  p <- modsum$coefficients[beta+1,"Pr(>|t|)"]
  ## traditional difference-based test 
  ret <- data.frame(b=b,
                    se=se,
                    df=df,
                    t=t,
                    p=p)
  ret
}

# TOST: performs Schuirmann's Two One-Sided Test on the predictor and provides the results (the largest p value) 
TOST <- function(dat, beta){
  b <- modelstats(dat, beta)$b
  se <- modelstats(dat, beta)$se
  df <- modelstats(dat, beta)$df
  t.value.1 <- (b - l.delta)/se
  t.value.2 <- (b-u.delta)/se
  p.value.1 <-stats::pt(t.value.1, df, lower.tail=FALSE)
  p.value.2 <-stats::pt(t.value.2, df, lower.tail=TRUE)
  
  ifelse(abs(t.value.1) <= abs(t.value.2), t.value <- t.value.1, t.value <- t.value.2) # finding the smaller t to present
  ifelse(p.value.1 >= p.value.2, p.value <- p.value.1, p.value <- p.value.2) # finding the larger p to present
  ret <- data.frame(p=p.value)
  ret
  
}

# AH: performs the Anderson-Hauck test on the predcitor and provides the p value
AH <- function(dat, beta){
  b <- modelstats(dat, beta)$b
  se <- modelstats(dat, beta)$se
  df <- modelstats(dat, beta)$df
  t.value <- (b - (l.delta+u.delta)/2)/se
  H.A.del <- ((u.delta-l.delta)/2)/se #this is the delta as defined in Hauck and Anderson (1986)
  p.value <- stats::pt(abs(t.value)-H.A.del,df) - stats::pt(-abs(t.value)-H.A.del, df)
  ret <- data.frame(p=p.value)
  ret
  
}


Sigma <- function(cors, predictors){
  mat <- matrix(data=cors, nrow = predictors, ncol=predictors)
  mat[1,1] <- 1L
  for (i in 1L:predictors) {
    mat[i,i] <- 1L
  }
  mat
}

