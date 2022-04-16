library(sp)
library(raster)
library(caret)
library(pROC)
library(randomForest)

set.seed(43)
rasters15 <- list.files("./data/Raster", full.names = T)
rasters15 <- lapply(rasters15, raster)
mystack <- stack(rasters15)
template <- rasters15[[1]]
source("./R/minlinlogistic.R")


## load points
PA <- read.csv("./data/liuetal2019PA.csv")
envs1 <- extract(mystack, PA[,1:2])
envs <- envs1
envs[,2:5] <- apply(envs[,2:5], 2,function(w){ (w-mean(w,na.rm = T))/sd(w,na.rm = T)})

full <- cbind(PA,envs)
full <- na.omit(full)

Folds <- createFolds(1:nrow(full), k=5) # 5 fold CV

Y <- full$R

X <- list(full[,4], full[,5:6], full[,7], full[,8])
X <- lapply(X, as.matrix)

minlinres <- lapply(1:5,function(i, Y, X, Folds){
  Y_train <- Y[-Folds[[i]]]
  Y_test <- Y[Folds[[i]]]
  
  X_train <- lapply(X, function(xx, fold){
    as.matrix(xx[-fold,])
  }, Folds[[i]])
  
  X_test <- lapply(X, function(xx, fold){
    as.matrix( xx[fold,])
  }, Folds[[i]])
  
  fit_minlin <- minlinlogistic(Y_train,X_train, boot = 0, 
                               a=100, method = "SANN",
                               control = list(maxit = 15000))
  #fitts <- lapply(1:10, function(i, Y,X){
    #minlinlogistic(Y,X, boot = 0, method = "SANN")
  #  minlinlogistic(Y,X, boot = 0, control = list(maxit = 5000))
  #}, Y,X)
  
  #parss <- lapply(fitts, function(w){w$fit$opt$par})
  #predict_minlinr <- predict_minlin(colMeans( Reduce(rbind,parss)),X_test)
  predict_minlinr <- predict_minlin(fit_minlin$fit$opt$par,X_test)
  roc_minlin <- roc(Y_test, predict_minlinr$predicted)
  return(list(fit = fit_minlin, predict = predict_minlinr, roc = roc_minlin))
}, Y,X,Folds)


rfres <- lapply(1:5, function(i, full, Folds){
  train_rf <- full[-Folds[[i]],]
  test_rf <- full[Folds[[i]],]
  fit_rf <- randomForest(R~., data = train_rf[,-c(1:2)])
  predict_rf <- predict(fit_rf, newdata = test_rf)
  roc_rf <- roc(test_rf$R, predict_rf)
  
  return(list(fit = fit_rf, predict = predict_rf, roc = roc_rf))
  
}, full, Folds)

glmres <- lapply(1:5, function(i, full, Folds){
  train_glm <- full[-Folds[[i]],]
  test_glm <- full[Folds[[i]],]
  fit_glm <- glm(R~., data = train_glm[,-c(1:2)],family = binomial())
  predict_glm <- predict(fit_glm, newdata = test_glm)
  roc_glm <- roc(test_glm$R, predict_glm)
  
  return(list(fit = fit_glm, predict = predict_glm, roc = roc_glm))
  
}, full,Folds)


auc_minlin <- sapply(minlinres, function(w){
  w$roc$auc
})

auc_rf <- sapply(rfres, function(w){
  w$roc$auc
})

auc_glm <- sapply(glmres, function(w){
  w$roc$auc
})

aucs <- data.frame(minlin = auc_minlin, rf = auc_rf, glm = auc_glm)

library(ggplot2)
library(reshape2)
aucs_plot <- melt(aucs)

ggplot(data = aucs_plot, mapping = aes(x = variable, y = value)) + 
  geom_boxplot() + 
  geom_point()


pdf("./Res/AUC.pdf", width = 9,height=3)
par(mfrow = c(1,3))
plot(minlinres[[1]]$roc, main = "min-linear")
for(i in 2:5){
  plot(minlinres[[i]]$roc, add = TRUE, main = "min-linear")
}
text(x=0.2, y = 0.1, paste0("auc=", 
                           signif( mean(aucs$minlin),3),
                           "(",signif( sd(aucs$minlin),3),")"))

plot(rfres[[1]]$roc, main = "random forest")
for(i in 2:5){
  plot(rfres[[i]]$roc, add = TRUE, main = "random forest")
}

text(x=0.2, y = 0.1, paste0("auc=", 
                            signif( mean(aucs$rf),3),
                            "(",signif( sd(aucs$rf),3),")"))

plot(glmres[[1]]$roc, main = "GLM")
for(i in 2:5){
  plot(glmres[[i]]$roc, add = TRUE, main = "GLM")
}
text(x=0.2, y = 0.1, paste0("auc=", 
                            signif( mean(aucs$glm),3),
                            "(",signif( sd(aucs$glm),3),")"))
dev.off()
