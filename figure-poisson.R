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
response.vec <- predict(fit, X, type="response")
class.vec <- predict(fit, X, type="class")

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
seq.vec <- 0:10
best.pred.list <- list()
for(pmean in c(seq(0, 9, by=0.5), log(2))){
  prob.vec <- dpois(seq.vec, pmean)
  for(loss.name in names(loss.fun.list)){
    loss.fun <- loss.fun.list[[loss.name]]
    pred.fun <- pred.fun.list[[loss.name]]
    best.pred.list[[paste(pmean, loss.name)]] <- data.table(
      pmean, loss.name, count=pred.fun(pmean))
    expected.fun <- expected.loss.list[[loss.name]]
    for(pred.value in seq.vec){
      expected.value <- expected.fun(pred.value, pmean)
      loss.vec <- loss.fun(pred.value, seq.vec)
      one.term <- loss.vec * prob.vec
      series <- cumsum(one.term)
      expected.list[[paste(pmean, pred.value, loss.name)]] <- data.table(
        pmean, loss.name, pred.value, loss=expected.value)
      loss.seq.list[[paste(pmean, pred.value, loss.name)]] <- data.table(
        pmean, loss.name, pred.value, count=seq.vec, series)
    }
  }
  prob.dt.list[[paste(pmean)]] <- data.table(
    pmean,
    probability=prob.vec,
    cum.prob=cumsum(prob.vec),
    pred.value=seq.vec)
  vline.list[[paste(pmean)]] <- data.table(pmean)
}
loss.seq <- do.call(rbind, loss.seq.list)
expected <- do.call(rbind, expected.list)
vline <- do.call(rbind, vline.list)
expected[, min.loss := min(loss), by=.(pmean, loss.name)]
expected[, status := ifelse(loss==min.loss, "min loss", "bigger loss")]
prob.dt <- do.call(rbind, prob.dt.list)
best.pred <- do.call(rbind, best.pred.list)
hline.dt <- expected
hline.dt$status <- "series"

ggplot()+
  geom_vline(aes(xintercept=pmean), data=vline)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(loss.name ~ pmean, scales="free")+
  geom_point(aes(pred.value, loss, color=status),
             data=expected)

viz.wide <- list(
  loss=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=1000)+
    facet_grid(loss.name ~ what, scales="free")+
    geom_hline(aes(yintercept=loss,
                   showSelected=pred.value,
                   showSelected2=pmean,
                   showSelected3=status),
               color="grey",
               data=hline.dt)+
    geom_vline(aes(xintercept=pmean, clickSelects=pmean),
               size=5,
               alpha=0.7,
               data=data.table(vline, what="expected"))+
    geom_point(aes(pred.value, loss, color=status,
                   showSelected=pmean,
                   clickSelects=pred.value),
               size=5,
               data=data.table(expected, what="expected"))+
    geom_text(aes(7, 0, label=sprintf("Poisson mean = %.2f", pmean),
                  showSelected=pmean),
              data=data.table(what="expected", loss.name="L0", vline))+
    geom_text(aes(max(seq.vec), 0, label=paste(
      "Series for expected loss, predicted value =", pred.value),
      showSelected=pred.value,
      showSelected2=pmean),
      hjust=1,
      data=data.table(expected[loss.name=="L0",], what="series"))+
    geom_point(aes(count, series,
                   showSelected=pred.value,
                   showSelected2=pmean),
               data=data.table(loss.seq, what="series"))
)
animint2dir(viz.wide, "figure-poisson-series-wide")

median.dt <- data.table(
  count=max(seq.vec),
  cum.prob=0.5)
viz <- list(
  loss=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(height=600)+
    facet_grid(loss.name ~ ., scales="free")+
    geom_vline(aes(xintercept=pmean, showSelected=pmean),
               data=vline,
               color="grey",
               size=1)+
    geom_hline(aes(yintercept=loss,
                   showSelected=pred.value,
                   showSelected2=pmean,
                   showSelected3=status),
               color="grey",
               data=hline.dt)+
    geom_hline(aes(yintercept=cum.prob),
               color="grey",
               data=data.table(median.dt, loss.name="cumulative prob"))+
    geom_vline(aes(xintercept=count, color=status,
                   key=paste(pmean, count),
                   showSelected=pmean),
               data=data.table(best.pred, status="min loss"))+
    geom_vline(aes(xintercept=pmean, clickSelects=pmean),
               size=5,
               alpha=0.6,
               data=data.table(
                 vline,
                 loss.name="probability"))+
    geom_point(aes(count, series, color=status,
                   showSelected=pred.value,
                   showSelected2=pmean),
               data=data.table(loss.seq, status="series"))+
    geom_point(aes(pred.value, probability, 
                   showSelected=pmean),
               size=4,
               data=data.table(
                 prob.dt, loss.name="probability"))+
    geom_point(aes(pred.value, cum.prob, 
                   showSelected=pmean),
               size=4,
               data=data.table(
                 prob.dt, loss.name="cumulative prob"))+
    geom_point(aes(pred.value, loss, color=status,
                   showSelected=pmean,
                   key=pred.value,
                   clickSelects=pred.value),
               size=5,
               alpha=0.8,
               data=expected)+
    geom_text(aes(max(seq.vec), 0, label=sprintf("Poisson mean = %.2f", pmean),
                  showSelected=pmean),
              hjust=1,
              data=data.table(loss.name="L0", vline))+
    scale_x_continuous("predicted value", breaks=seq.vec),
  duration=list(pmean=1000, pred.value=1000)
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

exp.linear.predictor <- seq(0, 7, by=0.1)
linear.predictor <- log(exp.linear.predictor)
finite.predictor <- linear.predictor[is.finite(linear.predictor)]
finite.range <- range(finite.predictor)
linear.predictor.grid <-
  seq(finite.range[1], finite.range[2], l=length(exp.linear.predictor))
exp.vec <- sort(unique(c(exp.linear.predictor, exp(linear.predictor.grid))))
loss.list <- list()
seg.list <- list()
minima.list <- list()
for(label.value in 0:3){
  seg.list[[paste(label.value)]] <- if(0 == label.value){
    data.table(
      label.value,
      min.exp=c(0, 0.5),
      max.exp=c(0.5, Inf),
      is.error=c(0, 1))
  }else{
    data.table(
      label.value,
      min.exp=c(0, label.value-0.5, label.value+0.5),
      max.exp=c(label.value-0.5, label.value+0.5, Inf),
      is.error=c(1, 0, 1))
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


ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(label.value ~ ., labeller=function(var, val){
    paste("label =", val)
  })+
  ylab("")+
  ## geom_line(aes(exp.linear.predictor, is.error,
  ##               group=what, color=what),
  ##           data=data.table(loss, what="prediction error"))+
  geom_point(aes(label.value, 0, color=what),
             shape=1,
             data=data.table(minima, what="Poisson loss"))+
  xlab("predicted Poisson mean parameter")+
  geom_segment(aes(min.exp, is.error,
                   xend=max.exp, yend=is.error,
                   color=what),
               data=data.table(segs, what="prediction error"))+
  geom_line(aes(exp.linear.predictor, loss.value,
                group=what, color=what),
            data=data.table(loss[is.finite(loss.value),], what="Poisson loss"))

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(label.value ~ ., labeller=function(var, val){
    paste("label =", val)
  })+
  ylab("")+
  geom_segment(aes(log(min.exp), is.error,
                   xend=log(max.exp), yend=is.error,
                   color=what),
               data=data.table(segs, what="prediction error"))+
  ## geom_line(aes(linear.predictor, is.error,
  ##               group=what, color=what),
  ##           data=data.table(loss, what="prediction error"))+
  geom_point(aes(log(label.value), 0, color=what),
             shape=1,
             data=data.table( 
              minima[is.finite(log(label.value)),], what="Poisson loss"))+
  xlab("linear predictor = log(Poisson mean)")+
  geom_line(aes(linear.predictor, loss.value,
                group=what, color=what),
            data=data.table(
              loss[is.finite(linear.predictor),], what="Poisson loss"))

prob.list <- list()
prob.label.list <- list()
count <- 0:7
count.mid <- mean(range(count))
for(poisson.mean in exp.linear.predictor){
  probability <- dpois(count, poisson.mean)
  prob.list[[paste(poisson.mean)]] <- data.table(
    poisson.mean, count, probability)
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
viz <- list(
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
    facet_grid(. ~ x, scales="free")+
    ylab("")+
    geom_segment(aes(log(min.exp), is.error,
                     xend=log(max.exp), yend=is.error,
                     showSelected=label.value,
                     color=what),
                 data=data.table(
                   x="log(Poisson mean)",
                   segs, what="prediction error"))+
    geom_point(aes(log(label.value), 0, color=what,
                   showSelected=label.value),
               shape=1,
               data=data.table( 
                 x="log(Poisson mean)",
                 minima[is.finite(log(label.value)),], what="Poisson loss"))+
    xlab("")+
    geom_line(aes(linear.predictor, loss.value,
                  showSelected=label.value,
                  group=what, color=what),
              data=data.table(
                x="log(Poisson mean)",
                loss[is.finite(linear.predictor),], what="Poisson loss"))+
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
    geom_point(aes(label.value, 0, color=what,
                   showSelected=label.value),
               shape=1,
               data=data.table(
                 x="Poisson mean",
                 minima, what="Poisson loss"))+
    geom_segment(aes(min.exp, is.error,
                     showSelected=label.value,
                     xend=max.exp, yend=is.error,
                     color=what),
                 data=data.table(
                 x="Poisson mean",
                   segs, what="prediction error"))+
    geom_line(aes(exp.linear.predictor, loss.value,
                  showSelected=label.value,
                  group=what, color=what),
              data=data.table(
                 x="Poisson mean",
                 loss[is.finite(loss.value),], what="Poisson loss"))+
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
animint2dir(viz, "figure-poisson")
