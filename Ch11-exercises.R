library(animint)
library(data.table)
library(lars)

data(prostate, package="ElemStatLearn")
input.cols <- c(
  "lcavol", "lweight", "age", "lbph", "svi", "lcp", "gleason", 
  "pgg45")
is.validation <- prostate$train == FALSE
train.df <- prostate[!is.validation, ]
x <- as.matrix(train.df[input.cols])
y <- train.df$lpsa
fit <- lars(x,y,type="lasso")
pred.nox <- predict(fit, type="coef")
beta <- scale(pred.nox$coefficients, FALSE, 1/fit$normx)
arclength <- rowSums(abs(beta))
path.list <- list()
for(variable in colnames(beta)){
  standardized.coef <- beta[, variable]
  path.list[[variable]] <- data.table(
    step=seq_along(standardized.coef),
    lambda=c(fit$lambda, 0),
    variable,
    standardized.coef,
    fraction=pred.nox$fraction,
    arclength)
}
path <- do.call(rbind, path.list)
variable.colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
  "#A65628", "#F781BF", "#999999")
fraction <- sort(unique(c(
  seq(0, 1, l=21))))
pred.fraction <- predict(
  fit, prostate[input.cols],
  type="coef", mode="fraction", s=fraction)
coef.grid.list <- list()
coef.grid.mat <- scale(pred.fraction$coefficients, FALSE, 1/fit$normx)
for(fraction.i in seq_along(fraction)){
  standardized.coef <- coef.grid.mat[fraction.i,]
  coef.grid.list[[fraction.i]] <- data.table(
    fraction=fraction[[fraction.i]],
    variable=colnames(x),
    standardized.coef,
    arclength=sum(abs(standardized.coef)))
}
coef.grid <- do.call(rbind, coef.grid.list)
pred.list <- predict(
  fit, prostate[input.cols],
  mode="fraction", s=fraction)
residual.mat <- pred.list$fit - prostate$lpsa
squares.mat <- residual.mat * residual.mat
mean.error.list <- list()
for(set in c("train", "validation")){
  val <- if(set=="validation")TRUE else FALSE
  is.set <- is.validation == val
  mse <- colMeans(squares.mat[is.set, ])
  mean.error.list[[paste(set)]] <- data.table(
    set, mse, fraction,
    arclength=rowSums(abs(coef.grid.mat)))
}
mean.error <- do.call(rbind, mean.error.list)
rect.width <- diff(mean.error$arclength[1:2])/2
addY <- function(dt, y){
  data.table(dt, y.var=factor(y, c("error", "weights")))
}
tallrect.dt <- coef.grid[variable==variable[1],]
lasso.res.list <- list()
for(fraction.i in seq_along(fraction)){
  lasso.res.list[[fraction.i]] <- data.table(
    observation.i=1:nrow(prostate),
    fraction=fraction[[fraction.i]],
    residual=residual.mat[, fraction.i],
    response=prostate$lpsa,
    arclength=sum(abs(coef.grid.mat[fraction.i,])),
    set=ifelse(prostate$train, "train","validation"))
}
lasso.res <- do.call(rbind, lasso.res.list)
hline.dt <- data.table(residual=0)
tallrect.all <- expand.grid(
  arclength.click=tallrect.dt$arclength,
  arclength.show=tallrect.dt$arclength)
viz.one.rect <- list(
  title="both .variable .value aesthetics",
  path=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(y.var ~ ., scales="free")+
    ylab("")+
    scale_color_manual(values=variable.colors)+
    geom_line(aes(arclength, standardized.coef, color=variable, group=variable),
              data=addY(path, "weights"))+
    geom_line(aes(arclength, mse, linetype=set, group=set),
              data=addY(mean.error, "error"))+
    geom_tallrect(aes(
      xmin=arclength.click-rect.width,
      xmax=arclength.click+rect.width,
      clickSelects.variable="arclength",
      clickSelects.value=arclength.click,
      showSelected.variable="arclength",
      showSelected.value=arclength.show,
      key=ifelse(
        arclength.click==arclength.show, 1,
        paste(arclength.click, arclength.show))),
      alpha=0.5,
      data=tallrect.all),
  res=ggplot()+
    geom_hline(aes(yintercept=residual),
               data=hline.dt,
               color="grey")+
    guides(linetype="none")+
    geom_point(aes(response, residual, 
                   key=observation.i,
                   showSelected=arclength),
               shape=21,
               fill=NA,
               color="black",
               data=lasso.res)+
    geom_segment(aes(response, residual,
                     xend=response, yend=0,
                     linetype=set,
                     key=observation.i,
                     showSelected=arclength),
               data=lasso.res),
  time=list(variable="arclength", ms=2000),
  duration=list(arclength=2000))
animint2dir(viz.one.rect, "Ch11-viz-one-rect")

tallrect.other <- expand.grid(
  arclength=tallrect.dt$arclength,
  arclength.other=tallrect.dt$arclength)
viz.other <- list(
  title="clickSelects=arclength",
  path=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(y.var ~ ., scales="free")+
    ylab("")+
    scale_color_manual(values=variable.colors)+
    geom_line(aes(arclength, standardized.coef, color=variable, group=variable),
              data=addY(path, "weights"))+
    geom_line(aes(arclength, mse, linetype=set, group=set),
              data=addY(mean.error, "error"))+
    geom_tallrect(aes(
      xmin=arclength-rect.width,
      xmax=arclength+rect.width,
      clickSelects=arclength,
      showSelected.variable="arclength",
      showSelected.value=arclength.other,
      key=ifelse(
        arclength==arclength.other, 1,
        paste(arclength, arclength.other))),
      alpha=0.5,
      data=tallrect.other),
  res=ggplot()+
    geom_hline(aes(yintercept=residual),
               data=hline.dt,
               color="grey")+
    guides(linetype="none")+
    geom_point(aes(response, residual, 
                   key=observation.i,
                   showSelected=arclength),
               shape=21,
               fill=NA,
               color="black",
               data=lasso.res)+
    geom_segment(aes(response, residual,
                     xend=response, yend=0,
                     linetype=set,
                     key=observation.i,
                     showSelected=arclength),
               data=lasso.res),
  time=list(variable="arclength", ms=2000),
  duration=list(arclength=2000))
animint2dir(viz.other, "Ch11-viz-other")

viz.other2 <- list(
  title="showSelected=arclength",
  path=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(y.var ~ ., scales="free")+
    ylab("")+
    scale_color_manual(values=variable.colors)+
    geom_line(aes(arclength, standardized.coef, color=variable, group=variable),
              data=addY(path, "weights"))+
    geom_line(aes(arclength, mse, linetype=set, group=set),
              data=addY(mean.error, "error"))+
    geom_tallrect(aes(
      xmin=arclength.other-rect.width,
      xmax=arclength.other+rect.width,
      showSelected=arclength,
      clickSelects.variable="arclength",
      clickSelects.value=arclength.other,
      key=ifelse(
        arclength==arclength.other, 1,
        paste(arclength, arclength.other))),
      alpha=0.5,
      data=tallrect.other),
  res=ggplot()+
    geom_hline(aes(yintercept=residual),
               data=hline.dt,
               color="grey")+
    guides(linetype="none")+
    geom_point(aes(response, residual, 
                   key=observation.i,
                   showSelected=arclength),
               shape=21,
               fill=NA,
               color="black",
               data=lasso.res)+
    geom_segment(aes(response, residual,
                     xend=response, yend=0,
                     linetype=set,
                     key=observation.i,
                     showSelected=arclength),
               data=lasso.res),
  time=list(variable="arclength", ms=2000),
  duration=list(arclength=2000))
animint2dir(viz.other2, "Ch11-viz-other2")

viz.make <- list(
  title="make_tallrect",
  path=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(y.var ~ ., scales="free")+
    ylab("")+
    scale_color_manual(values=variable.colors)+
    geom_line(aes(arclength, standardized.coef, color=variable, group=variable),
              data=addY(path, "weights"))+
    geom_line(aes(arclength, mse, linetype=set, group=set),
              data=addY(mean.error, "error"))+
    make_tallrect(mean.error, "arclength"),
  res=ggplot()+
    geom_hline(aes(yintercept=residual),
               data=hline.dt,
               color="grey")+
    guides(linetype="none")+
    geom_point(aes(response, residual, 
                   key=observation.i,
                   showSelected=arclength),
               shape=21,
               fill=NA,
               color="black",
               data=lasso.res)+
    geom_segment(aes(response, residual,
                     xend=response, yend=0,
                     linetype=set,
                     key=observation.i,
                     showSelected=arclength),
               data=lasso.res),
  time=list(variable="arclength", ms=2000),
  duration=list(arclength=2000))
animint2dir(viz.make, "Ch11-viz-make")

## prostateLasso <- list(
##   path=data.frame(path),
##   residuals=data.frame(lasso.res),
##   models=data.frame(tallrect.dt),
##   error=data.frame(mean.error))
## save(prostateLasso, file="~/R/animint/data/prostateLasso.RData")
## prompt(prostateLasso, file="~/R/animint/man/prostateLasso.Rd")
