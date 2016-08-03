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
options(warn=1)

mean.error.list <- list()
obs.error.list <- list()
selected.list <- list()
path.list <- list()
zeros.list <- list()
var.info.list <- list()
for(validation.fold in 0:3){
  is.validation <- prostate$fold == validation.fold
  train.df <- prostate[!is.validation, ]
  x <- as.matrix(train.df[input.cols])
  y <- train.df$lpsa
  fit <- lars(x,y,type="lasso")
  pred.nox <- predict(fit, type="coef")
  enters.path <- apply(pred.nox$coefficients, 2, function(coef.vec){
    pred.nox$fraction[min(which(coef.vec != 0))-1]
  })
  var.info.list[[paste(validation.fold)]] <-
    data.frame(validation.fold,
               enters.path,
               variable=names(enters.path))
  fraction <- sort(unique(c(
    ##pred.nox$fraction, #include exact breakpoints? (uneven animation)
    seq(0, 1, l=21))))
  pred.list <- predict(
    fit, prostate[input.cols],
    mode="fraction", s=fraction)
  residual.mat <- pred.list$fit - prostate$lpsa
  for(fraction.i in seq_along(fraction)){
    f <- fraction[fraction.i]
    obs.error.list[[paste(validation.fold, fraction.i)]] <- 
    data.frame(
      validation.fold,
      observation.i=1:nrow(residual.mat),
      set=ifelse(is.validation, "validation", "train"),
      fraction.i,
      fraction.fac=factor(f, fraction),
      fraction=f,
      residual=residual.mat[, fraction.i],
      predicted=pred.list$fit[, fraction.i],
      true=prostate$lpsa)
  }
  squares.mat <- residual.mat * residual.mat
  for(set in c("train", "validation")){
    val <- if(set=="validation")TRUE else FALSE
    is.set <- is.validation == val
    mse <- colMeans(squares.mat[is.set, ])
    mean.error.list[[paste(validation.fold, set)]] <- error.df <- data.frame(
      validation.fold, set, mse, fraction, what="mean squared error")
  }
  selected.list[[paste(validation.fold)]] <-
    subset(error.df, mse==min(mse))
  beta <- scale(pred.nox$coefficients, FALSE, 1/fit$normx)
  arclength <- rowSums(abs(beta))
  for(variable in colnames(beta)){
    standardized.coef <- beta[, variable]
    path.list[[paste(validation.fold, variable)]] <- data.frame(
      validation.fold,
      step=seq_along(standardized.coef),
      variable,
      standardized.coef,
      fraction=pred.nox$fraction,
      arclength)
  }
  zeros.list[[paste(validation.fold)]] <-
    data.frame(validation.fold,
               zeros=rowSums(beta==0),
               fraction=pred.nox$fraction)
}

path <- do.call(rbind, path.list)
mean.error <- do.call(rbind, mean.error.list)
zeros <- do.call(rbind, zeros.list)
selected <- do.call(rbind, selected.list)
obs.error <- do.call(rbind, obs.error.list)
var.info <- do.call(rbind, var.info.list)
info.by.var <- split(var.info, var.info$variable)
mean.enters.vec <- sort(
  sapply(info.by.var, with, mean(enters.path)), decreasing=TRUE)
mean.enters.df <- data.frame(
  variable=names(mean.enters.vec),
  enters.path=mean.enters.vec)
var.info$variable <- factor(var.info$variable, mean.enters.df$variable)

dput(RColorBrewer::brewer.pal(Inf, "Set1"))
variable.colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
  "#A65628", "#F781BF", "#999999")
names(variable.colors) <- mean.enters.df$variable

ggplot()+
  scale_color_manual(values=variable.colors)+
  geom_point(aes(enters.path, variable, color=variable),
             data=var.info)

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_color_manual(values=variable.colors)+
  facet_grid(what ~ validation.fold, scales="free")+
  geom_line(aes(fraction, standardized.coef, color=variable, group=variable),
            data=data.frame(path, what="weights"))+
  geom_point(aes(fraction, mse),
             shape=1,
             data=selected)+
  geom_line(aes(fraction, mse, linetype=set, group=set),
            data=mean.error)+
  scale_linetype_manual(values=c(train=1, validation=2))+
  geom_line(aes(fraction, zeros),
            data=data.frame(zeros, what="zeros"))+
  ylab("")+
  ggtitle("LASSO path for prostate cancer data calculated using the LARS")

abline.df <- data.frame(slope=1, intercept=0)

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(fraction ~ validation.fold)+
  geom_abline(aes(slope=slope, intercept=intercept),
              data=abline.df,
              color="grey")+
  geom_point(aes(true, predicted, color=set),
             shape=1,
             data=subset(obs.error, fraction %in% seq(0, 1, by=0.25)))+
  coord_equal()
  
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ set)+
  geom_abline(aes(slope=slope, intercept=intercept),
              data=abline.df,
              color="grey")+
  geom_point(aes(true, predicted),
             shape=1,
             data=subset(obs.error, fraction==1 & validation.fold==1))+
  coord_equal()

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(fraction ~ .)+
  geom_tile(aes(observation.i, validation.fold, fill=residual, linetype=set),
            color="black",
            size=0.3,
            data=subset(obs.error, fraction %in% seq(0, 1, by=0.25)))+
  scale_linetype_manual(values=c(train=0, validation=2))+
  guides(linetype=guide_legend(override.aes=list(fill="white")))+
  scale_fill_gradient2()

wide.df <- data.frame(
  validation.fold=0:3)
wide.df$min <- wide.df$validation.fold-0.5
wide.df$max <- wide.df$validation.fold+0.5

viz <- list(
  title="LASSO path for prostate cancer data calculated using the LARS",
  path=ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(what ~ ., scales="free")+
  geom_line(aes(fraction, standardized.coef,
                showSelected=validation.fold,
                key=variable,
                color=variable, group=variable),
            data=data.frame(path, what="weights"))+
  geom_point(aes(fraction, standardized.coef,
                 showSelected=validation.fold,
                 key=paste(step, variable),
                 color=variable),
            data=data.frame(subset(path, standardized.coef!=0), what="weights"))+
    geom_point(aes(fraction, mse,
                   showSelected=validation.fold),
               shape=1,
               size=4,
               data=selected)+
  geom_line(aes(fraction, mse, linetype=set, group=set,
                key=set,
                showSelected=validation.fold),
            data=mean.error)+
  scale_linetype_manual(values=c(train=1, validation=2))+
  ## geom_line(aes(fraction, zeros,
  ##               showSelected=validation.fold),
  ##           data=data.frame(zeros, what="zeros"))+
  make_tallrect(mean.error, "fraction")+
  ylab(""),
  ## residuals=ggplot()+
  ##   geom_widerect(aes(ymin=min, ymax=max,
  ##                     clickSelects=validation.fold),
  ##                 fill="transparent",
  ##                 data=wide.df,
  ##                 alpha=0.5)+
  ##   theme_bw()+
  ##   geom_tile(aes(observation.i, validation.fold,
  ##                 showSelected=fraction,
  ##                 fill=residual, linetype=set),
  ##             color="black",
  ##             size=1,
  ##             data=obs.error)+
  ##   scale_linetype_manual(values=c(train=0, validation=2))+
  ##   guides(linetype=guide_legend(override.aes=list(fill="white")))+
  ##   scale_fill_gradient2(),
  importance=ggplot()+
    theme_animint(width=200, height=200)+
    guides(color="none")+
    scale_color_manual(values=variable.colors)+
    geom_point(aes(enters.path, variable, color=variable,
                   clickSelects=validation.fold),
               size=6,
               alpha=0.7,
               data=var.info),
  scatter=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(. ~ set)+
    geom_abline(aes(slope=slope, intercept=intercept),
                data=abline.df,
                color="grey")+
    geom_point(aes(true, predicted,
                   key=observation.i,
                   showSelected=fraction,
                   showSelected2=validation.fold),
               shape=1,
               data=obs.error)+
    coord_equal(),  
  duration=list(fraction=500, validation.fold=1000),
  time=list(variable="fraction", ms=500)
)
animint2dir(viz, "figure-lasso")
