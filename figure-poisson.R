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
predicted.vec <- floor(exp.vec)
loss.list <- list()
seg.list <- list()
minima.list <- list()
for(label.value in 0:3){
  if(0 < label.value){
    seg.list[[paste(label.value, "left")]] <- data.table(
      label.value,
      min.exp=0, max.exp=label.value, is.error=1)
  }
  seg.list[[paste(label.value, "middle, right")]] <- data.table(
    label.value,
    min.exp=label.value + c(0, 1),
    max.exp=c(label.value + 1, Inf),
    is.error=c(0, 1))
  label.vec <- rep(label.value, length(exp.vec))
  loss.value <- PoissonLoss(label.vec, exp.vec)
  is.error <- as.numeric(predicted.vec != label.value)
  loss.list[[paste(label.value)]] <- data.table(
    exp.linear.predictor=exp.vec,
    linear.predictor=log(exp.vec),
    label.value, loss.value, predicted.vec, is.error)
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
