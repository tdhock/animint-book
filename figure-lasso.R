source("packages.R")

data(prostate,package="ElemStatLearn")
pros <- subset(prostate,select=-train,train==TRUE)
ycol <- which(names(pros)=="lpsa")
x <- as.matrix(pros[-ycol])
y <- pros[[ycol]]
fit <- lars(x,y,type="lasso")
pred.nox <- predict(fit)
fraction <- sort(unique(c(pred.nox$fraction, seq(0, 1, l=100))))
pred.list <- predict(
  fit, prostate[! names(prostate) %in% c("train", "lpsa")],
  mode="fraction", s=fraction)
residual.mat <- pred.list$fit - prostate$lpsa
squares.mat <- residual.mat * residual.mat

error.list <- list()
for(set in c("train", "validation")){
  train.val <- if(set=="train")TRUE else FALSE
  is.set <- prostate$train == train.val
  mse <- colMeans(squares.mat[is.set, ])
  error.list[[set]] <- data.frame(
    set, mse, fraction, what="mean squared error")
}
error <- do.call(rbind, error.list)
selected <- subset(error.list$validation, mse==min(mse))

coef.mat <- coef(fit, s=fraction, mode="fraction")
beta <- scale(coef.mat,FALSE,1/fit$normx)
arclength <- rowSums(abs(beta))
path.list <- list()
for(variable in colnames(beta)){
  standardized.coef <- beta[, variable]
  path.list[[variable]] <- data.frame(
    step=seq_along(standardized.coef),
    variable,
    standardized.coef,
    fraction,
    arclength)
}
path <- do.call(rbind, path.list)

zeros.df <- data.frame(zeros=rowSums(beta==0), fraction)

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(what ~ ., scales="free")+
  geom_point(aes(fraction, mse),
             shape=1,
             data=selected)+
  geom_line(aes(fraction, mse, linetype=set, group=set),
            data=error)+
  geom_line(aes(fraction, zeros),
            data=data.frame(zeros.df, what="zeros"))+
  geom_line(aes(fraction, standardized.coef, color=variable, group=variable),
            data=data.frame(path, what="weights"))+
  ylab("")+
  ggtitle("LASSO path for prostate cancer data calculated using the LARS")

