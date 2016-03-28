library(animint)
library(data.table)
library(glmnet)

N <- 100
y <- 1:N
X <- cbind(
  pos=rnorm(N) + 2 * y,
  neg=rnorm(N) - y,
  noise1=rnorm(N),
  noise2=rnorm(N))
fit <- cv.glmnet(X, y, family="poisson")
link.vec <- predict(fit, X, type="link")
## link is the real valued linear predictor.
response.vec <- predict(fit, X, type="response")
## response is the positive Poisson mean.
cbind(exp(link.vec), response.vec)
##class.vec <- predict(fit, X, type="class")

df <- data.frame(X, label=y)
gfit <- glm(label ~ pos + neg, df, family="poisson")
glink.vec <- predict(gfit, type="link")
gresp.vec <- predict(gfit, type="response")

expected.loss.list <- list(
  L0=function(pred, mean){
    prob <- dpois(pred, mean)
    1 - prob
  },
  L1=function(pred, mean){
    stopifnot(is.integer(pred))
    stopifnot(length(pred)==1)
    y <- 0:pred
    prob <- dpois(y, mean)
    expected.loss <- (pred-y)*prob
    mean-pred+2*sum(expected.loss)
  },
  L2=function(pred, mean){
    residual <- pred-mean
    residual * residual + mean
  })
loss.fun.list <- list(
  L0=function(pred, label)ifelse(pred==label, 0, 1),
  L1=function(pred, label)abs(pred-label),
  L2=function(pred, label){
    residual <- pred - label
    residual * residual
  })
pred.fun.list <- list(
  L0=function(mean){
    if(mean==0){
      0
    }else{
      unique(c(ceiling(mean-1), floor(mean)))
    }
  },
  L1=function(mean){
    count <- qpois(0.5, mean)
    prob <- dpois(count, mean)
    if(prob==0.5) c(count, count+1) else count
  }, L2=function(mean)unique(c(ceiling(mean-0.5), floor(mean+0.5)))
)
expected.list <- list()
loss.seq.list <- list()
vline.list <- list()
prob.dt.list <- list()
seq.vec <- 0:7
best.pred.list <- list()
poisson.mean.vec <- c(seq(0, max(seq.vec)-1, by=0.5), log(2))
for(poisson.mean in poisson.mean.vec){
  prob.vec <- dpois(seq.vec, poisson.mean)
  for(loss.name in names(loss.fun.list)){
    loss.fun <- loss.fun.list[[loss.name]]
    pred.fun <- pred.fun.list[[loss.name]]
    best.pred.list[[paste(poisson.mean, loss.name)]] <- data.table(
      poisson.mean, loss.name, count=pred.fun(poisson.mean))
    expected.fun <- expected.loss.list[[loss.name]]
    for(pred.value in seq.vec){
      expected.value <- expected.fun(pred.value, poisson.mean)
      loss.vec <- loss.fun(pred.value, seq.vec)
      one.term <- loss.vec * prob.vec
      series <- cumsum(one.term)
      expected.list[[paste(poisson.mean, pred.value, loss.name)]] <- data.table(
        poisson.mean, loss.name, pred.value, loss=expected.value)
      loss.seq.list[[paste(poisson.mean, pred.value, loss.name)]] <- data.table(
        poisson.mean, loss.name, pred.value, count=seq.vec, series)
    }
  }
  prob.dt.list[[paste(poisson.mean)]] <- data.table(
    poisson.mean,
    probability=prob.vec,
    cum.prob=cumsum(prob.vec),
    pred.value=seq.vec)
  vline.list[[paste(poisson.mean)]] <- data.table(poisson.mean)
}
loss.seq <- do.call(rbind, loss.seq.list)
expected <- do.call(rbind, expected.list)
vline <- do.call(rbind, vline.list)
expected[, min.loss := min(loss), by=.(poisson.mean, loss.name)]
expected[, error.type := ifelse(abs(loss-min.loss)<1e-6, "min loss", "bigger loss")]
prob.dt <- do.call(rbind, prob.dt.list)
best.pred <- do.call(rbind, best.pred.list)
hline.dt <- expected
hline.dt$error.type <- "series"

ggplot()+
  geom_vline(aes(xintercept=poisson.mean), data=vline)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(loss.name ~ poisson.mean, scales="free")+
  geom_point(aes(pred.value, loss, color=error.type),
             data=expected)

viz.wide <- list(
  loss=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=1000)+
    facet_grid(loss.name ~ error.type, scales="free")+
    geom_hline(aes(yintercept=loss,
                   showSelected=pred.value,
                   showSelected2=poisson.mean,
                   showSelected3=error.type),
               color="grey",
               data=hline.dt)+
    geom_vline(aes(xintercept=poisson.mean, clickSelects=poisson.mean),
               size=5,
               alpha=0.7,
               data=data.table(vline, error.type="expected"))+
    geom_point(aes(pred.value, loss, color=error.type,
                   showSelected=poisson.mean,
                   clickSelects=pred.value),
               size=5,
               data=data.table(expected, error.type="expected"))+
    geom_text(aes(7, 0, label=sprintf("Poisson mean = %.2f", poisson.mean),
                  showSelected=poisson.mean),
              hjust=1,
              data=data.table(error.type="expected", loss.name="L0", vline))+
    geom_text(aes(max(seq.vec), 0, label=paste(
      "Series for expected loss, predicted value =", pred.value),
      showSelected=pred.value,
      showSelected2=poisson.mean),
      hjust=1,
      data=data.table(expected[loss.name=="L0",], error.type="series"))+
    geom_point(aes(count, series,
                   showSelected=pred.value,
                   showSelected2=poisson.mean),
               data=data.table(loss.seq, error.type="series"))
)
animint2dir(viz.wide, "figure-poisson-series-wide")

## No general closed form expression for the L1 decision boundary
## exists! Instead, we can use the numerical root finding method
## implemented in the R uniroot function.
L1Boundary <- function(lower.int){
  stopifnot(is.integer(lower.int))
  stopifnot(length(lower.int)==1)
  stopifnot(0 <= lower.int)
  zero.fun <- function(x){
    ppois(lower.int, x) - 0.5
  }
  uniroot(zero.fun, c(lower.int, lower.int+1))
}
## between 0 and 1 it is log(2)
L1Boundary(0L)
log(2)
boundary.fun.list <- list(
  L0=function(i)i+1,
  L1=function(i)L1Boundary(i)$root,
  L2=function(i)i+0.5
  )
median.dt <- data.table(
  count=max(seq.vec),
  cum.prob=0.5)
viz <- list(
  loss=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(height=600)+
    facet_grid(loss.name ~ ., scales="free")+
    geom_vline(aes(xintercept=poisson.mean, showSelected=poisson.mean),
               data=vline,
               color="grey",
               size=1)+
    geom_hline(aes(yintercept=loss,
                   showSelected=pred.value,
                   showSelected2=poisson.mean,
                   showSelected3=error.type),
               color="grey",
               data=hline.dt)+
    geom_hline(aes(yintercept=cum.prob),
               color="grey",
               data=data.table(median.dt, loss.name="cumulative prob"))+
    geom_vline(aes(xintercept=count, color=error.type,
                   key=paste(poisson.mean, count),
                   showSelected=poisson.mean),
               data=data.table(best.pred, error.type="min loss"))+
    geom_vline(aes(xintercept=poisson.mean, clickSelects=poisson.mean),
               size=5,
               alpha=0.6,
               data=data.table(
                 vline,
                 loss.name="probability"))+
    geom_point(aes(count, series, color=error.type,
                   showSelected=pred.value,
                   showSelected2=poisson.mean),
               data=data.table(loss.seq, error.type="series"))+
    geom_point(aes(pred.value, probability, 
                   showSelected=poisson.mean),
               size=4,
               data=data.table(
                 prob.dt, loss.name="probability"))+
    geom_point(aes(pred.value, cum.prob, 
                   showSelected=poisson.mean),
               size=4,
               data=data.table(
                 prob.dt, loss.name="cumulative prob"))+
    geom_point(aes(pred.value, loss, color=error.type,
                   showSelected=poisson.mean,
                   key=pred.value,
                   clickSelects=pred.value),
               size=5,
               alpha=0.8,
               data=expected)+
    geom_text(aes(max(seq.vec), 0,
                  label=sprintf("Poisson mean = %.2f", poisson.mean),
                  showSelected=poisson.mean),
              hjust=1,
              data=data.table(loss.name="L0", vline))+
    scale_x_continuous("predicted value", breaks=seq.vec),
  duration=list(poisson.mean=1000, pred.value=1000)
)
animint2dir(viz, "figure-poisson-series")

PoissonLoss <- function(count, seg.mean, weight = 1){
  stopifnot(is.numeric(count))
  stopifnot(is.numeric(seg.mean))
  stopifnot(is.numeric(weight))
  if (any(weight < 0)) {
    stop("PoissonLoss undefined for negative weight")
  }
  if (any(seg.mean < 0)) {
    stop("PoissonLoss undefined for negative segment mean")
  }
  not.integer <- round(count) != count
  not.positive <- count < 0
  stopifnot(length(seg.mean) == length(count))
  loss <- ifelse(
    not.integer | not.positive, Inf,
    ifelse(seg.mean == 0, ifelse(count == 0, 0, Inf),
           seg.mean - count * log(seg.mean)
           ## This term makes all the minima zero.
           -ifelse(count == 0, 0, count - count*log(count))))
  loss * weight
}

exp.linear.predictor <- seq(0, max(seq.vec), by=0.1)
linear.predictor <- log(exp.linear.predictor)
finite.predictor <- linear.predictor[is.finite(linear.predictor)]
finite.range <- range(finite.predictor)
linear.predictor.grid <-
  seq(finite.range[1], finite.range[2], l=length(exp.linear.predictor))
exp.vec <- sort(unique(c(exp.linear.predictor, exp(linear.predictor.grid))))
loss.list <- list()
seg.list <- list()
minima.list <- list()
one.pred.list <- list()
one.pred.fun.list <- list(
  L0=function(mean)floor(mean),
  L1=function(mean)qpois(0.5, mean),
  L2=function(mean)floor(mean+0.5))
for(label.value in seq.vec){
  for(loss.name in names(boundary.fun.list)){
    boundary.fun <- boundary.fun.list[[loss.name]]
    loss.fun <- loss.fun.list[[loss.name]]
    prev.boundary <- 0
    one.pred.fun <- one.pred.fun.list[[loss.name]]
    count <- one.pred.fun(poisson.mean.vec)
    loss.fun <- loss.fun.list[[loss.name]]
    one.pred.list[[paste(loss.name, label.value)]] <- data.table(
      loss.name, label.value,
      poisson.mean=poisson.mean.vec,
      count,
      loss=loss.fun(count, label.value))
    for(pred.value in seq.vec){
      this.boundary <- boundary.fun(pred.value)
      seg.list[[paste(label.value, pred.value, loss.name)]] <- data.table(
        label.value, pred.value, loss.name,
        min.exp=prev.boundary,
        max.exp=this.boundary,
        loss=loss.fun(label.value, pred.value))
      prev.boundary <- this.boundary
    }
  }
  label.vec <- rep(label.value, length(exp.vec))
  loss.value <- PoissonLoss(label.vec, exp.vec)
  loss.list[[paste(label.value)]] <- data.table(
    exp.linear.predictor=exp.vec,
    linear.predictor=log(exp.vec),
    label.value, loss.value)
  minima.list[[paste(label.value)]] <- data.table(
    label.value)
}
minima <- do.call(rbind, minima.list)
loss <- do.call(rbind, loss.list)
segs <- do.call(rbind, seg.list)
one.pred <- do.call(rbind, one.pred.list)

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(loss.name ~ label.value, labeller=function(var, val){
    paste(var, val)
  }, scales="free")+
  ylab("")+
  geom_point(aes(label.value, 0, color=error.type),
             shape=1,
             data=data.table(minima, error.type="Poisson loss"))+
  xlab("predicted Poisson mean parameter")+
  geom_segment(aes(min.exp, loss,
                   xend=max.exp, yend=loss,
                   color=error.type),
               data=data.table(segs, error.type="prediction error"))+
  geom_line(aes(exp.linear.predictor, loss.value,
                group=error.type, color=error.type),
            data=data.table(loss[is.finite(loss.value),],
                            error.type="Poisson loss"))

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(loss.name ~ label.value, labeller=function(var, val){
    paste("label =", val)
  }, scales="free")+
  ylab("")+
  geom_segment(aes(log(min.exp), loss,
                   xend=log(max.exp), yend=loss,
                   color=error.type),
               data=data.table(segs, error.type="prediction error"))+
  geom_point(aes(log(label.value), 0, color=error.type),
             shape=1,
             data=data.table( 
              minima[is.finite(log(label.value)),], error.type="Poisson loss"))+
  xlab("linear predictor = log(Poisson mean)")+
  geom_line(aes(linear.predictor, loss.value,
                group=error.type, color=error.type),
            data=data.table(
              loss[is.finite(linear.predictor),], error.type="Poisson loss"))

prob.list <- list()
prob.label.list <- list()
count.mid <- mean(range(seq.vec))
for(poisson.mean in exp.linear.predictor){
  probability <- dpois(seq.vec, poisson.mean)
  prob.list[[paste(poisson.mean)]] <- data.table(
    poisson.mean, count=seq.vec, probability)
  prob.label.list[[paste(poisson.mean)]] <- data.table(
    poisson.mean, count=count.mid, probability=1)
}
prob <- do.call(rbind, prob.list)
prob.label <- do.call(rbind, prob.label.list)

some <- function(dt){
  dt[poisson.mean %in% c(0, 0.5, 1, 1.5, 2),]
}
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_wrap("poisson.mean")+
  geom_text(aes(count, probability, label=sprintf(
    "Poisson mean = %.1f", poisson.mean)),
    data=some(prob.label))+
  geom_point(aes(count, probability),
             data=some(prob))+
  geom_tallrect(aes(xmin=label.value-0.5, xmax=label.value+0.5),
                alpha=0.5,
                data=minima)

prob.label[, min := ifelse(poisson.mean==0, 0, poisson.mean-0.05)]
prob.label[, max := poisson.mean+0.05]
real.mid <- mean(finite.range)
positive.range <- range(exp.linear.predictor)
positive.mid <- mean(positive.range)
loss.max <- loss[is.finite(loss.value), max(loss.value)]

viz.orig <- list(
  probability=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    geom_text(aes(count, probability, showSelected=poisson.mean, label=sprintf(
    "Poisson mean = %.1f", poisson.mean)),
    data=prob.label)+
    geom_point(aes(count, probability, showSelected=poisson.mean),
               data=prob)+
    geom_tallrect(aes(xmin=label.value-0.5, xmax=label.value+0.5,
                      clickSelects=label.value),
                  alpha=0.5,
                  data=minima),
  loss=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=500)+
    facet_grid(loss.name ~ x, scales="free")+
    ylab("")+
    geom_segment(aes(log(min.exp), loss,
                     xend=log(max.exp), yend=loss,
                     showSelected=label.value,
                     color=error.type),
                 data=data.table(
                   x="log(Poisson mean)",
                   segs, error.type="prediction error"))+
    geom_point(aes(log(label.value), 0, color=error.type,
                   showSelected=label.value),
               shape=1,
               data=data.table( 
                 x="log(Poisson mean)",
                 minima[is.finite(log(label.value)),],
                 error.type="Poisson loss"))+
    xlab("")+
    geom_line(aes(linear.predictor, loss.value,
                  showSelected=label.value,
                  group=error.type, color=error.type),
              data=data.table(
                x="log(Poisson mean)",
                loss[is.finite(linear.predictor),], error.type="Poisson loss"))+
    geom_text(aes(real.mid, loss.max, label=paste("label =", label.value),
                  showSelected=label.value),
              data=data.table(
                x="log(Poisson mean)",
                minima))+
    geom_tallrect(aes(xmin=log(min), xmax=log(max),
                      clickSelects=poisson.mean),
                  alpha=0.5,
                  data=data.table(prob.label, x="log(Poisson mean)"))+
    ## Below for non-log plot:
    geom_point(aes(label.value, 0, color=error.type,
                   showSelected=label.value),
               shape=1,
               data=data.table(
                 x="Poisson mean",
                 minima, error.type="Poisson loss"))+
    geom_segment(aes(min.exp, loss,
                     showSelected=label.value,
                     clickSelects=pred.value,
                     xend=max.exp, yend=loss,
                     color=error.type),
                 data=data.table(
                 x="Poisson mean",
                   segs, error.type="prediction error"))+
    geom_line(aes(exp.linear.predictor, loss.value,
                  showSelected=label.value,
                  group=error.type, color=error.type),
              data=data.table(
                 x="Poisson mean",
                 loss[is.finite(loss.value),], error.type="Poisson loss"))+
    geom_text(aes(positive.mid, loss.max, label=paste("label =", label.value),
                  showSelected=label.value),
              data=data.table(
                x="Poisson mean",
                minima))+
    geom_tallrect(aes(xmin=min, xmax=max,
                      clickSelects=poisson.mean),
                  alpha=0.5,
                  data=data.table(
                    x="Poisson mean",
                    prob.label)))
animint2dir(viz.orig, "figure-poisson-orig")

##dput(RColorBrewer::brewer.pal(Inf, "Set1"))
loss.colors <- 
  c("Poisson"="#E41A1C",
    "prediction"="#377EB8",
    series="#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
    "#A65628", "#F781BF", "#999999")
new.expected <- expected
new.expected$error.type <- "prediction"
prob.dt$error.type <- "Poisson"
viz <- list(
  mean=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(height=600, width=500)+
    facet_grid(loss.name ~ x, scales="free")+
    scale_color_manual(values=loss.colors)+
    guides(color="none")+
    ylab("")+
    xlab("")+
    geom_point(aes(log(poisson.mean), loss,
                   color=error.type,
                   clickSelects=poisson.mean,
                   showSelected=label.value,
                   showSelected2=error.type),
               size=3,
               data=data.table(
                 x="log(Poisson mean)",
                 one.pred, error.type="prediction"))+
    geom_point(aes(log(label.value), 0,
                   showSelected=label.value,
                   showSelected2=error.type),
               size=4,
               data=data.table( 
                 x="log(Poisson mean)",
                 minima[is.finite(log(label.value)),],
                 error.type="Poisson"))+
    geom_line(aes(linear.predictor, loss.value,
                  showSelected=label.value,
                  group=error.type,
                  color=error.type,
                  showSelected2=error.type),
              data=data.table(
                x="log(Poisson mean)",
                loss[is.finite(linear.predictor),], error.type="Poisson"))+
    geom_vline(aes(xintercept=poisson.mean,
                   clickSelects=poisson.mean),
               alpha=0.6,
               size=5,
               data=data.table(
                 x="Poisson mean",
                 vline))+
    geom_segment(aes(log(min.exp), loss,
                     xend=log(max.exp), yend=loss,
                     showSelected=label.value,
                     clickSelects=pred.value,
                     color=error.type,
                     showSelected2=error.type),
                 size=3,
                 data=data.table(
                   x="log(Poisson mean)",
                   segs, error.type="prediction"))+
    geom_text(aes(-2, 6, label=paste("label =", label.value),
                  showSelected=label.value),
              hjust=0,
              data=data.table(
                x="log(Poisson mean)",
                loss.name="L0",
                minima))+
    ## Below for non-log plot:
    geom_point(aes(poisson.mean, loss,
                   color=error.type,
                   clickSelects=poisson.mean,
                   showSelected=label.value,
                   showSelected2=error.type),
               size=3,
               data=data.table(
                 x="Poisson mean",
                 one.pred, error.type="prediction"))+
    geom_point(aes(label.value, 0,
                   showSelected=label.value,
                   showSelected2=error.type),
               size=4,
               data=data.table(
                 x="Poisson mean",
                 minima, error.type="Poisson"))+
    geom_line(aes(exp.linear.predictor, loss.value,
                  showSelected=label.value,
                  group=error.type,
                  color=error.type,
                  showSelected2=error.type),
              data=data.table(
                 x="Poisson mean",
                 loss[is.finite(loss.value),], error.type="Poisson"))+
    geom_vline(aes(xintercept=log(poisson.mean),
                   clickSelects=poisson.mean),
               alpha=0.6,
               size=5,
               data=data.table(vline, x="log(Poisson mean)"))+
    geom_segment(aes(min.exp, loss,
                     showSelected=label.value,
                     clickSelects=pred.value,
                     xend=max.exp, yend=loss,
                     color=error.type,
                     showSelected2=error.type),
                 size=4,
                 data=data.table(
                 x="Poisson mean",
                 segs, error.type="prediction")),
  count=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(height=600, width=300)+
    scale_color_manual(values=loss.colors)+
    facet_grid(loss.name ~ ., scales="free")+
    geom_vline(aes(xintercept=poisson.mean, showSelected=poisson.mean),
               data=vline,
               size=3,
               linetype="dashed")+
    geom_hline(aes(yintercept=loss,
                   showSelected=pred.value,
                   showSelected2=poisson.mean,
                   showSelected3=error.type),
               color="grey",
               data=hline.dt)+
    geom_hline(aes(yintercept=cum.prob),
               color="grey",
               data=data.table(median.dt, loss.name="cumulative prob"))+
    geom_vline(aes(xintercept=count, color=error.type,
                   showSelected=poisson.mean),
               data=data.table(one.pred, error.type="prediction"))+
    geom_point(aes(count, series, color=error.type,
                   showSelected=pred.value,
                   showSelected2=poisson.mean),
               data=data.table(loss.seq, error.type="series"))+
    geom_tallrect(aes(
      xmin=ifelse(label.value==0, -Inf, label.value-0.5),
      xmax=label.value+0.5,
      clickSelects=label.value),
      alpha=0.5,
      data=data.table(minima, loss.name="probability"))+
    geom_point(aes(pred.value, probability, color=error.type,
                   showSelected=poisson.mean),
               size=4,
               data=data.table(
                 prob.dt, loss.name="probability"))+
    geom_point(aes(pred.value, cum.prob, color=error.type,
                   showSelected=poisson.mean),
               size=4,
               data=data.table(
                 prob.dt, loss.name="cumulative prob"))+
    geom_point(aes(pred.value, loss, color=error.type,
                   showSelected=poisson.mean,
                   key=pred.value,
                   clickSelects=pred.value),
               size=5,
               alpha=0.8,
               data=new.expected)+
    geom_text(aes(max(seq.vec), 0,
                  label=sprintf("Poisson mean = %.2f", poisson.mean),
                  showSelected=poisson.mean),
              hjust=1,
              data=data.table(loss.name="L0", vline))+
    scale_x_continuous("predicted value", breaks=seq.vec)+
    ylab(""),
  duration=list(poisson.mean=1000, pred.value=1000, label.value=1000))
animint2dir(viz, "figure-poisson")
