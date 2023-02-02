set.seed(1)
sim.col <- 2
sim.row <- 100
features.hidden <- matrix(runif(sim.row*sim.col), sim.row, sim.col)
library(data.table)
hidden.dt <- data.table(type="hidden", features.hidden)
bayes <- function(DT)DT[, (V1>0.2 & V2<0.8)]
label.vec <- ifelse(bayes(hidden.dt), 1, -1)
features.noisy <- features.hidden+rnorm(sim.row*sim.col, sd=0.05)
label.fac <- factor(label.vec)
table(label.vec)
is.set.list <- list(
  validation=rep(c(TRUE,FALSE), l=nrow(features.noisy)))
set.vec <- ifelse(is.set.list$validation, "validation", "subtrain")
is.set.list$subtrain <- !is.set.list$validation
n.subtrain <- sum(is.set.list$subtrain)
sim.dt <- data.table(
  rbind(
    data.table(type="noisy", features.noisy),
    hidden.dt),
  set=set.vec, 
  label.fac)
library(animint2)
ggplot()+
  facet_grid(set ~ type, labeller=label_both)+
  geom_point(aes(
    V1, V2, color=label.fac),
    data=sim.dt)+
  coord_equal()
n.grid <- 30
V1.seq <- sim.dt[, seq(min(V1), max(V1), l=n.grid)]
V2.seq <- sim.dt[, seq(min(V2), max(V2), l=n.grid)]
grid.dt <- CJ(V1=V1.seq, V2=V2.seq)
grid.dt[, bayes.num := ifelse(bayes(grid.dt), 1, -1)]
grid.dt[, bayes.fac := factor(bayes.num)]
ggplot()+
  facet_grid(. ~ type)+
  geom_tile(aes(
    V1, V2, fill=bayes.fac),
    data=grid.dt)+
  geom_point(aes(
    V1, V2, fill=label.fac),
    color="black",
    shape=21,
    data=sim.dt)+
  coord_equal()
get_contours <- function(score){
  contour.list <- contourLines(
    V1.seq, V2.seq, 
    matrix(score, length(V1.seq), length(V2.seq), byrow=TRUE),
    levels=0)
  if(length(contour.list)){
    data.table(contour.i=seq_along(contour.list))[, {
      with(contour.list[[contour.i]], data.table(level, x, y))
    }, by=contour.i]
  }
}
bayes.contour.dt <- get_contours(grid.dt$bayes.num)
ggplot()+
  facet_grid(. ~ type)+
  geom_path(aes(
    x, y, group=contour.i),
    data=bayes.contour.dt)+
  geom_point(aes(
    V1, V2, fill=label.fac),
    color="black",
    shape=21,
    data=sim.dt)+
  coord_equal()
new_node <- function(value, gradient=NULL, ...){
  node <- new.env()
  node$value <- value
  node$parent.list <- list(...)
  node$backward <- function(){
    grad.list <- gradient(node)
    for(parent.name in names(grad.list)){
      parent.node <- node$parent.list[[parent.name]]
      parent.node$grad <- grad.list[[parent.name]]
      parent.node$backward()
    }
  }
  node
}
initial_node <- function(mat){
  new_node(mat, gradient=function(...)list())
}
mm <- function(feature.node, weight.node)new_node(
  cbind(1, feature.node$value) %*% weight.node$value,
  features=feature.node, 
  weights=weight.node,
  gradient=function(node)list(
    features=node$grad %*% t(weight.node$value),
    weights=t(cbind(1, feature.node$value)) %*% node$grad))
relu <- function(before.node)new_node(
  ifelse(before.node$value < 0, 0, before.node$value),
  before=before.node,
  gradient=function(node)list(
    before=ifelse(before.node$value < 0, 0, node$grad)))
sigmoid <- function(before.node)new_node(
  1/(1+exp(before.node$value)),
  before=before.node,
  gradient=function(node)list(
    before=node$value*(1-node$value)))
log_loss <- function(pred.node, label.node)new_node(
  mean(log(1+exp(-label.node$value*pred.node$value))),
  pred=pred.node,
  label=label.node,
  gradient=function(...)list(
    pred=-label.node$value/(
      1+exp(label.node$value*pred.node$value)
    )/length(label.node$value)))
pred_node <- function(set.features){
  feature.node <- initial_node(set.features)
  for(layer.i in seq_along(weight.node.list)){
    weight.node <- weight.node.list[[layer.i]]
    before.node <- mm(feature.node, weight.node)
    feature.node <- if(layer.i < length(weight.node.list)){
      relu(before.node)
    }else{
      before.node
    }
  }
  feature.node
}
step.size <- 0.5
max.iterations <- 1000
units.per.layer.list <- list(
  neural.network=c(ncol(features.noisy), 50, 1),
  linear.model=c(ncol(features.noisy), 1))
grid.mat <- grid.dt[, cbind(V1,V2)]
loss.dt.list <- list()
err.dt.list <- list()
pred.dt.list <- list()
for(model.name in names(units.per.layer.list)){
  units.per.layer <- units.per.layer.list[[model.name]]
  weight.node.list <- list()
  set.seed(10)
  for(layer.i in seq(1, length(units.per.layer)-1)){
    input.units <- units.per.layer[[layer.i]]+1
    output.units <- units.per.layer[[layer.i+1]]
    weight.mat <- matrix(
      rnorm(input.units*output.units), input.units, output.units)
    weight.node.list[[layer.i]] <- initial_node(weight.mat)
  }
  for(iteration in 1:max.iterations){
    loss.node.list <- list()
    for(set.name in names(is.set.list)){
      is.set <- is.set.list[[set.name]]
      set.label.node <- initial_node(label.vec[is.set])
      set.features <- features.noisy[is.set,]
      set.pred.node <- pred_node(set.features)
      set.loss.node <- log_loss(set.pred.node, set.label.node)
      loss.node.list[[set.name]] <- set.loss.node
      set.pred.num <- ifelse(set.pred.node$value<0, -1, 1)
      is.error <- set.pred.num != set.label.node$value
      err.dt.list[[paste(model.name, iteration, set.name)]] <- data.table(
        model.name, 
        iteration, set=set.name,
        set.features,
        label=set.label.node$value,
        pred.num=as.numeric(set.pred.num))
      loss.dt.list[[paste(model.name, iteration, set.name)]] <- data.table(
        model.name, 
        iteration, set.name, 
        mean.log.loss=set.loss.node$value,
        error.percent=100*mean(is.error))
    }
    grid.node <- pred_node(grid.mat)
    pred.dt.list[[paste(model.name, iteration)]] <- data.table(
      model.name, 
      iteration,
      grid.dt,
      pred=as.numeric(grid.node$value))
    loss.node.list$subtrain$backward()
    for(layer.i in seq_along(weight.node.list)){
      weight.node <- weight.node.list[[layer.i]]
      weight.node$value <- weight.node$value-step.size*weight.node$grad
    }  
  }
}
(loss.dt <- rbindlist(loss.dt.list))
(err.dt <- rbindlist(err.dt.list))
(pred.dt <- rbindlist(pred.dt.list))
loss.tall <- melt(loss.dt, measure=c("mean.log.loss", "error.percent"))
loss.tall[, log10.iteration := log10(iteration)]
min.dt <- loss.tall[
, .SD[which.min(value)], by=.(model.name, set.name, variable)]
ggplot()+
  facet_grid(variable ~ model.name, scales="free")+
  scale_y_continuous("")+
  geom_line(aes(
    iteration, value, color=set.name),
    data=loss.tall)+
  geom_point(aes(
    iteration, value, fill=set.name),
    shape=21,
    color="black",
    data=min.dt)
iteration.contours <- pred.dt[
, get_contours(pred), by=.(model.name, iteration)]

some <- function(DT)DT[iteration%in%c(1,5,10,50,100, 500, 1000)]
some.loss <- some(loss.dt)
pred.dt[, norm.pred := pred/max(abs(pred)), by=.(model.name, iteration)]
some.pred <- some(pred.dt)
some.err <- some(err.dt)
some.contours <- some.pred[
, get_contours(pred), by=.(model.name, iteration)]
ggplot()+
  facet_grid(model.name + set ~ iteration, labeller="label_both")+
  geom_tile(aes(
    V1, V2, fill=norm.pred),
    data=some.pred)+
  geom_path(aes(
    x, y, group=contour.i),
    color="grey50",
    data=some.contours)+
  scale_fill_gradient2()+
  geom_point(aes(
    V1, V2),
    size=2,
    data=some.err[label!=pred.num])+
  geom_point(aes(
    V1, V2, color=label.fac),
    size=1,
    data=sim.dt[type=="noisy"])+
  coord_equal()
loss.tall[, model.iteration := paste(model.name, iteration)]
err.dt[, model.iteration := paste(model.name, iteration)]
pred.dt[, model.iteration := paste(model.name, iteration)]
iteration.contours[, model.iteration := paste(model.name, iteration)]
loss.tall[, set := set.name]
loss.dt[, set := set.name]
loss.dt[, model.iteration := paste(model.name, iteration)]
min.dt[, set := set.name]
sim.dt[, label.num := as.numeric(paste(label.fac))]
err.dt[, prediction := ifelse(label==pred.num, "correct", "error")]
loss.dt[, n.set := ifelse(set=="subtrain", n.subtrain, sim.row-n.subtrain)]
loss.dt[, error.count := n.set*error.percent/100]
some <- function(DT)DT[iteration%in%as.integer(seq(1,1000,l=100))]

animint(
  title="Neural network vs linear model",
  out.dir="Ch18-neural-networks-sim",
  loss=ggplot()+
    ggtitle("Loss/error curves, click to select model/iteration")+
    theme_bw()+
    theme_animint(width=600, height=300)+
    theme(panel.margin=grid::unit(1, "lines"))+
    facet_grid(variable ~ model.name, scales="free")+
    scale_y_continuous("")+
    scale_x_continuous(
      "Iteration/epoch of learning, with gradient computed on full subtrain set")+
    geom_line(aes(
      iteration, value, color=set, group=set),
      data=loss.tall)+
    geom_point(aes(
      iteration, value, fill=set),
      shape=21,
      color="black",
      data=min.dt)+
    geom_tallrect(aes(
      xmin=iteration-0.5, 
      xmax=iteration+0.5),
      alpha=0.5,
      clickSelects="model.iteration",
      data=some(loss.tall[set.name=="subtrain"])),
  data=ggplot()+
    ggtitle("Learned function at selected model/iteration")+
    theme_bw()+
    theme_animint(width=600)+
    facet_grid(. ~ set, labeller="label_both")+
    geom_tile(aes(
      V1, V2, fill=norm.pred),
      showSelected="model.iteration",
      data=some(pred.dt))+
    geom_text(aes(
      0.5, 1.1, label=paste0(
        "loss=", round(mean.log.loss, 4),
        ", ", error.count, "/", n.set, 
        " errors=", error.percent, "%")),
      showSelected="model.iteration",
      data=loss.dt)+
    geom_path(aes(
      x, y, group=contour.i),
      showSelected="model.iteration",
      color="grey50",
      data=some(iteration.contours))+
    geom_point(aes(
      V1, V2, fill=label/2, color=prediction),
      showSelected=c("model.iteration", "set"),
      size=4,
      data=some(err.dt))+
    scale_fill_gradient2(
      "Class/Score")+
    scale_color_manual(
      values=c(correct="white", error="black"))+
    scale_x_continuous(
      "Input/Feature 1")+
    scale_y_continuous(
      "Input/Feature 2")+
    coord_equal())
