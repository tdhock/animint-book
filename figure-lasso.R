source("packages.R")

data(prostate,package="ElemStatLearn")
input.cols <- c(
  "lcavol", "lweight", "age", "lbph", "svi", "lcp", "gleason", 
  "pgg45")

## 3-fold cross-validation with one of the folds equivalent to the
## test set in Hastie et al.
set.seed(1)
prostate$fold <- NA
prostate$fold[prostate$train==FALSE] <- 1
prostate$fold[prostate$train==TRUE] <- sample(rep(2:3, l=sum(prostate$train)))
table(prostate$fold)

error.list <- list()
selected.list <- list()
path.list <- list()
zeros.list <- list()
for(validation.fold in 0:3){
  is.validation <- prostate$fold == validation.fold
  train.df <- prostate[!is.validation, ]
  x <- as.matrix(train.df[input.cols])
  y <- train.df$lpsa
  fit <- lars(x,y,type="lasso")
  pred.nox <- predict(fit)
  fraction <- sort(unique(c(pred.nox$fraction, seq(0, 1, l=100))))
  pred.list <- predict(
    fit, prostate[input.cols],
    mode="fraction", s=fraction)
  residual.mat <- pred.list$fit - prostate$lpsa
  squares.mat <- residual.mat * residual.mat
  for(set in c("train", "validation")){
    val <- if(set=="validation")TRUE else FALSE
    is.set <- is.validation == val
    mse <- colMeans(squares.mat[is.set, ])
    error.list[[paste(validation.fold, set)]] <- error.df <- data.frame(
      validation.fold, set, mse, fraction, what="mean squared error")
  }
  selected.list[[paste(validation.fold)]] <-
    subset(error.df, mse==min(mse))
  coef.mat <- coef(fit, s=fraction, mode="fraction")
  beta <- scale(coef.mat,FALSE,1/fit$normx)
  arclength <- rowSums(abs(beta))
  for(variable in colnames(beta)){
    standardized.coef <- beta[, variable]
    path.list[[paste(validation.fold, variable)]] <- data.frame(
      validation.fold,
      step=seq_along(standardized.coef),
      variable,
      standardized.coef,
      fraction,
      arclength)
  }
  zeros.list[[paste(validation.fold)]] <-
    data.frame(validation.fold, zeros=rowSums(beta==0), fraction)
}
path <- do.call(rbind, path.list)
error <- do.call(rbind, error.list)
zeros <- do.call(rbind, zeros.list)
selected <- do.call(rbind, selected.list)

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(what ~ validation.fold, scales="free")+
  geom_line(aes(fraction, standardized.coef, color=variable, group=variable),
            data=data.frame(path, what="weights"))+
  geom_point(aes(fraction, mse),
             shape=1,
             data=selected)+
  geom_line(aes(fraction, mse, linetype=set, group=set),
            data=error)+
  geom_line(aes(fraction, zeros),
            data=data.frame(zeros, what="zeros"))+
  ylab("")+
  ggtitle("LASSO path for prostate cancer data calculated using the LARS")

