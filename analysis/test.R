source("./R/minlinlogistic.R")

n_site <- 300
n_class <- 5
ps <- rep(1,n_class)
Design <- lapply(1:n_class, function(i,n_site){
  matrix(rnorm(n_site,0,3))
},n_site)
offsets <- rep(1,n_class)

par <- c(rnorm(sum(ps)),offsets)

limiting <- findmin_minlin(par, Design, ps, n_class)
p_exit <- getp(par, Design, ps, n_class)

Y <- simudata_minlin(par, Design, ps, n_class)
fit <- minlinlogistic(Y,Design, boot = 200, control = list(maxit = 5000))


boot_beta <- lapply(fit$boot,`[[`, "par")
boot_beta <- Reduce(rbind, boot_beta)
par(mfrow = c(2,5))
for(i in 1:10){
  (boxplot(boot_beta[,i], outline = F))
  abline(h = par[i], col = "red")
}

sum(fit$fit$findmin==limiting)

library(randomForest)
X_plain <- Reduce(cbind, Design)

my.rf <- randomForest(x=X_plain, y = Y)
plot(p_exit, my.rf$predicted)
points(p_exit, fit$fit$predicted, col = "red")

plot(my.rf$predicted, fit$fit$predicted)
abline(0,1,col = "red")
