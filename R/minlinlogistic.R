sigmoid <- function(x){
  1/(1+exp(-x))
}


logsumexp <- function(x){
  xstar <- max(x)
  xstar + log(sum(exp(x-xstar)))
}

softmin <- function(x, a = 10){
  prob <- exp(-a*x - logsumexp(-a*x) )
  sum(prob * x)
}



findmin_minlin <- function(par, # parameters, for optim, betas first then intercept
                            Design, # design matrices, List, w/o intercept
                            ps, # number of predictors in each class, should be sapply(Design,ncol)
                            n_class # number of classes, should be length(ps)
){
  # par: regression coefficients, then offsets
  betas <- par[1:sum(ps)]
  #offsets <- -log( par[-(1:sum(ps))])
  offsets <-  par[-(1:sum(ps))]
  
  Xbeta <- lapply(1:n_class, function(i, betas, Design, ps, offsets){
    betai <- betas[1:ps[i]+(i!=1)*sum(ps[1:(i-1)])]
    Design[[i]] %*% betai + offsets[i]
  }, betas, Design, ps, offsets)
  
  Xbeta <- Reduce(cbind, Xbeta)
  apply(Xbeta,1,function(w){
    if(is.na(sum(w))){
      return(NA)
    }
    else{
     return(which.min(w))
    }
    })
  
}

getp <- function(par, # parameters, for optim, betas first then intercept
                    Design, # design matrices, List, w/o intercept
                    ps, # number of predictors in each class, should be sapply(Design,ncol)
                    n_class, # number of classes, should be length(ps)
                 soft = TRUE, a = 10
){
  # par: regression coefficients, then offsets
  betas <- par[1:sum(ps)]
  offsets <-  par[-(1:sum(ps))]
  
  Xbeta <- lapply(1:n_class, function(i, betas, Design, ps, offsets){
    betai <- betas[1:ps[i]+(i!=1)*sum(ps[1:(i-1)])]
    Design[[i]] %*% betai + offsets[i]
  }, betas, Design, ps, offsets)
  
  Xbeta <- Reduce(cbind, Xbeta)
  if(soft){
    Xbeta <- apply(Xbeta,1,softmin, a = a)
  }
  else{
    Xbeta <- apply(Xbeta,1,min)
  }
  
  
  sigmoid(Xbeta)
}


# useful in bootstrap
simudata_minlin <- function(par, # parameters, for optim, betas first then intercept
                          Design, # design matrices, List, w/o intercept
                          ps, # number of predictors in each class, should be sapply(Design,ncol)
                          n_class, # number of classes, should be length(ps)
                          soft = FALSE, a = 10
){
  p_exis <- getp(par, # parameters, for optim, betas first then intercept
                 Design, # design matrices, List, w/o intercept
                 ps, # number of predictors in each class, should be sapply(Design,ncol)
                 n_class, soft, a)
  
  1*(runif(length(p_exis))<=p_exis)
  
}


loglik_minlin <- function(par, # parameters, for optim, betas first then intercept
                          Y, # response
                          Design, # design matrices, List, w/o intercept
                          ps, # number of predictors in each class, should be sapply(Design,ncol)
                          n_class, # number of classes, should be length(ps)
                          soft = TRUE,
                          a = 10
                          ){
  p_exis <- getp(par, # parameters, for optim, betas first then intercept
                 Design, # design matrices, List, w/o intercept
                 ps, # number of predictors in each class, should be sapply(Design,ncol)
                 n_class)
  
  -sum(Y*log(p_exis)+(1-Y)*log(1-p_exis))
  
}


minlinlogistic <- function(Y, Design, soft = TRUE, a = 10,boot = 50,...){
  ps <- sapply(Design, ncol)
  n_class <- length(Design)
  par_init <- c( rnorm(sum(ps)), rep(1,n_class))
  cat("Initial fitting\n")
  
  #lower <- c(rep(-Inf, sum(ps)), rep(1e-5, n_class))
  #lower <- c(rep(-Inf, sum(ps)), rep(-Inf, n_class))
  
  #optres <- optim(par = par_init, loglik_minlin, Y=Y, Design = Design, ps = ps,n_class = n_class,lower = lower,method = "L-BFGS-B",...)
  optres <- optim(par = par_init, loglik_minlin, Y=Y, Design = Design, ps = ps,n_class = n_class,soft = soft, a = a,...)
  init_res <- list(opt = optres, 
                   findmin = findmin_minlin(optres$par, Design, ps,n_class),
                   predicted = getp(optres$par, Design, ps,n_class))
  
  bootres <- list()
  if(boot>0){
    cat("bootstrap\n")
    pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                         max = boot, # Maximum value of the progress bar
                         style = 3,    # Progress bar style (also available style = 1 and style = 2)
                         char = "=")
  
    for( i in 1:boot){
      Yboot <- simudata_minlin(optres$par, Design, ps, n_class)
      #optboot <- optim(par = optres$par, loglik_minlin, Y=Yboot, Design = Design, ps = ps,n_class = n_class,lower = lower,method = "L-BFGS-B",...)
      optboot <- optim(par = optres$par, loglik_minlin, Y=Yboot, Design = Design, ps = ps,n_class = n_class,...)
      bootres[[i]] <- optboot
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  return(list(fit = init_res,boot = bootres))
  
}

predict_minlin <- function(par, X, soft = FALSE, a = 10){
  ps <- sapply(X, ncol)
  n_class <- length(X)
  
  predicted <- getp(par,X,ps,n_class, soft, a)
  whichmin <- Reduce(c ,findmin_minlin(par,X,ps,n_class))
  
  return(list(predicted = predicted, limiting = whichmin))
  
}
