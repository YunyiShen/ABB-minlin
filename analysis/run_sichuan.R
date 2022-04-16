library(sp)
library(raster)


rasters15 <- list.files("./data/Raster", full.names = T)
rasters15 <- lapply(rasters15, raster)
mystack <- stack(rasters15)
template <- rasters15[[1]]



## load points
PA <- read.csv("./data/liuetal2019PA.csv")
envs1 <- extract(mystack, PA[,1:2])
envs <- envs1
envs[,2:5] <- apply(envs[,2:5], 2,function(w){ (w-mean(w,na.rm = T))/sd(w,na.rm = T)})

full <- cbind(PA,envs)
full <- na.omit(full)



Y <- full$R

X <- list(full[,4], full[,5:6], full[,7], full[,8])
X <- lapply(X, as.matrix)


# make predictions used
all_env <- as.matrix( getValues(mystack))

mean_env <- colMeans(envs1[,2:5], na.rm = T)
sd_env <- apply(envs1[,2:5],2,sd, na.rm = T)

allones <- rep(1,nrow(all_env))

# standardize
all_env[,2:5] <- (all_env[,2:5]-(allones %*% t(mean_env)))/(allones %*% t(sd_env))
Xpred <- list(all_env[,1], all_env[,2:3], all_env[,4], all_env[,5])
Xpred <- lapply(Xpred, as.matrix)


# try minlinear
source("./R/minlinlogistic.R")

set.seed(12345)
#fit <- minlinlogistic(Y,X, boot = 0, soft = FALSE,method = "SANN")
fit <- minlinlogistic(Y,X, boot = 0, a = 100,method = "SANN",control = list(maxit = 15000))
fitts <- lapply(1:10, function(i, Y,X){
  #minlinlogistic(Y,X, boot = 0, soft = FALSE, method = "SANN")
  minlinlogistic(Y,X, boot = 0, a = 100,method = "SANN", control = list(maxit = 15000))
}, Y,X)

parss <- lapply(fitts, function(w){w$fit$opt$par})

fit$fit$findmin
plot(roc(Y, fit$fit$predicted))

# random forest
library(randomForest)
rffit <- randomForest(R~., data = full[,-c(1:2)])
plot(roc(Y, rffit$predicted))

# logistic

glmfit <- glm(R~., data = full[,-c(1:2)], family = binomial())
plot(roc(Y, glmfit$fitted.values))


# see the differences between three methods
plot(glmfit$fitted.values, fit$fit$predicted)
plot(glmfit$fitted.values, rffit$predicted)
plot(fit$fit$predicted, rffit$predicted)

hist(glmfit$fitted.values-fit$fit$predicted)
hist(glmfit$fitted.values-rffit$predicted)
hist(fit$fit$predicted-rffit$predicted)

# make predictions, min-linear
predicted_sichuan <- predict_minlin(fit$fit$opt$par, Xpred)
#predicted_sichuan <- predict_minlin(colMeans( Reduce(rbind,parss)), Xpred)

par(mfrow = c(1,2))
occup <- template
values(occup) <- predicted_sichuan$predicted
writeRaster(occup, "./Res/bear_minlin_pred.tif")
plot(occup)
#points(PA$XX[PA$R==1],PA$YY[PA$R==1], col = "red")
#points(PA$XX[PA$R==0],PA$YY[PA$R==0], col = "blue")

# which is the limiting factor
values(occup) <- predicted_sichuan$limiting
plot(occup)
writeRaster(occup, "./Res/bear_minlin_limiting.tif")
#points(PA$XX[PA$R==1],PA$YY[PA$R==1], col = "red")
#points(PA$XX[PA$R==0],PA$YY[PA$R==0], col = "blue")

# prediction of random forest
predicted_rf <- predict(rffit, newdata = all_env)
occuprf <- template
values(occuprf) <- predicted_rf
plot(occuprf)
writeRaster(occuprf, "./Res/bear_rf_pred.tif")

# glm predicting
predicted_glm <- predict(glmfit, newdata = as.data.frame(all_env), type = "response")
occupglm <- template
values(occupglm) <- predicted_glm
plot(occupglm)
writeRaster(occupglm, "./Res/bear_glm_pred.tif")
