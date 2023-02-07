library(ggplot2)
library(data.table)
library(directlabels)
data(spam, package="kernlab")
head(spam)
head(y.vec <- ifelse(spam$type=="spam", 1, -1))
table(y.vec)
head(X.mat <- as.matrix(subset(spam, select=-type)))
is.binary <- all(y.vec %in% c(1, -1))
n.folds <- 4
logistic.loss <- function(pred, y){
  log(1+exp(-y * pred))
}
sigmoid <- function(a){
  1/(1+exp(-a))
}
set.seed(2)
(unique.folds <- 1:n.folds)
fold.vec <- sample(rep(unique.folds, l=nrow(X.mat)))
n.hidden.units <- 100 # u
max.iterations <- 1000
validation.fold=1
step.size=2
is.train <- fold.vec != validation.fold
X.train <- X.mat[is.train, ]
y.train <- y.vec[is.train]
X.sc <- scale(X.train)
iteration.dt.list <- list()
for(line.search.factor in c(NA, 3)){
  iteration.vec <- 1:max.iterations
  set.seed(1)
  V <- matrix(rnorm(ncol(X.sc)*n.hidden.units), ncol(X.sc), n.hidden.units)
  w <- rnorm(n.hidden.units)
  for(iteration in iteration.vec){
    head(A <- X.sc %*% V) #1
    head(Z <- sigmoid(A)) #2
    head(b <- as.numeric(Z %*% w))
    dw <- if(is.binary){
      -y.train * sigmoid(-y.train * b)
    }else{
      b-y.train
    }
    A.deriv <- Z * (1-Z)
    dv <- dw * A.deriv * matrix(w, nrow(A.deriv), ncol(A.deriv), byrow=TRUE)
    grad.w <- t(Z) %*% dw / nrow(X.sc)
    grad.V <- t(X.sc) %*% dv / nrow(X.sc)
    cost.w.V <- function(w.vec, V.mat){
      b <- sigmoid(X.sc %*% V.mat) %*% w.vec
      mean(logistic.loss(b, y.train))
    }
    cost.step <- function(step){
      cost.w.V(w-step*grad.w, V-step*grad.V)
    }
    if(is.finite(line.search.factor)){
      while(cost.step(step.bigger <- step.size*line.search.factor) < cost.step(step.size)){
        step.size <- step.bigger
      }
      while(cost.step(step.smaller <- step.size/line.search.factor) < cost.step(step.size)){
        step.size <- step.smaller
      }
    }
    if(FALSE){
      curve(sapply(x, cost.step), 0, 300)
    }
    cat(sprintf("it=%d step=%f\n", iteration, step.size))
    w <- w - step.size * grad.w
    V <- V - step.size * grad.V
    predict.sc <- function(X.tilde){
      A.mat <- X.tilde %*% V
      sigmoid(A.mat) %*% w
    }
    predict1.orig <- function(X.unsc){
      X.tilde <- scale(
        X.unsc, attr(X.sc, "scaled:center"), attr(X.sc, "scaled:scale"))
      predict.sc(X.tilde)
    }
    V.orig <- V/attr(X.sc, "scaled:scale")
    b.orig <- - t(V/attr(X.sc, "scaled:scale")) %*% attr(X.sc, "scaled:center")
    V.with.intercept <- rbind(intercept=as.numeric(b.orig), V.orig)
    predict2.orig <- function(X.unsc){
      A.mat <- cbind(1, X.unsc) %*% V.with.intercept
      sigmoid(A.mat) %*% w
    }
    rbind(
      as.numeric(head(predict.sc(X.sc))),
      as.numeric(head(predict1.orig(X.train))),
      as.numeric(head(predict2.orig(X.train))))
    ## train/validation error.
    pred.vec <- as.numeric(predict2.orig(X.mat))
    is.error <- ifelse(pred.vec > 0, 1, -1) != y.vec
    log.loss <- logistic.loss(pred.vec, y.vec)
    square.loss <- (y.vec-pred.vec)^2
    iteration.dt.list[[paste(line.search.factor, iteration)]] <- data.table(
      line.search.factor,
      set=ifelse(is.train, "train", "validation"),
      is.error, square.loss,
      log.loss
    )[, list(
      iteration,
      validation.fold,
      mean.square.loss=mean(square.loss),
      error.percent=mean(is.error)*100,
      mean.log.loss=mean(log.loss)
    ), by=list(line.search.factor, set)]
  }
}
fold.dt <- do.call(rbind, iteration.dt.list)
fold.tall <- melt(
  fold.dt,
  measure.vars=c("error.percent", "mean.log.loss"))
min.tall <- fold.tall[, {
  .SD[which.min(value)]
}, by=list(variable, set, validation.fold)]
set.colors <- c(
  train="black",
  validation="red")
gg <- ggplot()+
  ggtitle(paste(
    "Single layer neural network (57, 100, 1) for binary classification",
    "of spam data, N_train=2300, N_validation=2301", sep="\n"))+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_color_manual(values=set.colors)+
  facet_grid(variable ~ line.search.factor, scales="free", labeller=label_both)+
  geom_line(aes(
    iteration, value, color=set),
    data=fold.tall)+
  scale_shape_manual(values=c(min=1))+
  geom_point(aes(
    iteration, value, color=set, shape=Value),
    size=3,
    data=data.table(Value="min", min.tall))+
  ylab("")+
  scale_x_continuous(limits=c(0, 1400))
dl <- direct.label(gg, "last.polygons")
png("Ch18-neural-networks-debug.png", width=7, height=7, units="in", res=200)
print(dl)
dev.off()

## Visualize spam data {#viz-spam}

```{r}
data("spam", package="kernlab")
names(spam)
dim(spam)
```

It can be seen above that the spam data have 4601 rows and 58 columns,
of which the first 57 are the inputs/features, and the last one (type)
is the output/label to predict.

```{r}
library(data.table)
spam.dt <- data.table(spam)
is.label <- names(spam.dt)=="type"
features.dt <- spam.dt[,!is.label,with=FALSE]
(features.tall <- melt(features.dt,measure=names(features.dt)))
```

The table above is the long/tall form of the spam features, which we
plot using a histogram below.

```{r fig.height=10, fig.width=10}
ggplot()+
  geom_histogram(aes(
    value),
    data=features.tall)+
  facet_wrap("variable", scales="free")
```

The histogram above shows that the min value seems to be zero, which
is verified by the code below,

```{r}
min(features.tall$value)
```

What do the histograms look like without the zeros?

```{r}
ggplot()+
  geom_histogram(aes(
    value),
    data=features.tall[value != 0])+
  facet_wrap("variable", scales="free")
```

They look skewed/asymmetric (lots of density on small values, little
density on large values).

```{r}
features.tall[, log.value := log(value+1)]
ggplot()+
  geom_histogram(aes(
    log.value),
    data=features.tall[value != 0])+
  facet_wrap("variable", scales="free")
```

The histograms above seem much less skewed, which may be beneficial
for learning (exercise for the reader, compare learning with and
without the log transform).

## Visualize neural network units {#viz-units}

A fully connected feed-forward neural network consists of one or more
matrix multiplications, each followed by a non-linear activation. The
sizes of the matrix are constrained by the first/input layer (number
of features), and the last/output layer (number of labels to
predict). In the case of the spam data, there are 57 inputs, and 1
output (for binary classification we need only one score, larger
values mean more likely to predict positive class). The other (hidden)
layers in between the inputs and the outputs may be of any
size. Typically they are chosen so that the first hidden layer is
relatively large, and the last hidden layer is relatively small, for
example:

```{r}
(units.per.layer <- c(ncol(features.dt), 20, 10, 1))
```

The neural network diagram for the network is shown below,

```{r}
node.dt.list <- list()
edge.dt.list <- list()
for(layer.i in seq_along(units.per.layer)){
  n.units <- units.per.layer[[layer.i]]
  layer.nodes <- data.table(
    layer.i, unit.i=seq(1, n.units)
  )[, y := unit.i*2-n.units-1]
  node.dt.list[[layer.i]] <- layer.nodes
  if(1 < layer.i){
    prev.i <- layer.i-1
    prev.nodes <- node.dt.list[[prev.i]]
    edge.dt.list[[layer.i]] <- CJ(
      prev.i=1:nrow(prev.nodes), 
      this.i=1:nrow(layer.nodes)
    )[, data.table(
      data.table(this=layer.nodes[this.i]),
      data.table(prev=prev.nodes[prev.i])
    )]
  }
}
node.dt <- do.call(rbind, node.dt.list)
edge.dt <- do.call(rbind, edge.dt.list)
text.dt <- node.dt[, .(
  max.y=max(y),
  units=.N
), by=layer.i]

width <- 0.02
height <- 1
ggplot()+
  scale_y_continuous(
    "", breaks=c())+
  geom_text(aes(
    layer.i, max.y+height*2, label=units),
    vjust=0,
    data=text.dt)+
  geom_segment(aes(
    this.layer.i, this.y,
    xend=prev.layer.i, yend=prev.y),
    size=0.1,
    data=edge.dt)+
  geom_rect(aes(
    xmin=layer.i-width,
    xmax=layer.i+width,
    ymin=y-height,
    ymax=y+height),
    data=node.dt,
    color="black",
    fill="grey")
```

## Visualize weight matrices {#viz-weights}

The weights in the neural network can be defined and initialized as
follows,

```{r}
weight.mat.list <- list()
for(input.layer.i in seq(1, length(units.per.layer)-1)){
  output.layer.i <- input.layer.i+1
  input.units <- units.per.layer[[input.layer.i]]
  output.units <- units.per.layer[[output.layer.i]]
  weight.mat.list[[input.layer.i]] <- matrix(
    rnorm(input.units*output.units), input.units, output.units)
}
str(weight.mat.list)
```

The list of random weight matrices above can be converted to a data
table for plotting via the code below,

```{r}
(init.weight.dt <- data.table(input.layer.i=seq_along(weight.mat.list))[, {
  mat <- weight.mat.list[[input.layer.i]]
  data.table(
    input.unit.i=as.integer(row(mat)),
    output.unit.i=as.integer(col(mat)),
    weight=as.numeric(mat))
}, by=input.layer.i])
```

They can be plotted via the code below,

```{r}
ggplot()+
  geom_tile(aes(
    output.unit.i, input.unit.i, fill=weight),
    color="grey50",
    data=init.weight.dt)+
  facet_grid(. ~ input.layer.i, labeller=label_both)+
  scale_y_reverse()+
  coord_equal()+
  scale_fill_gradient2()
```

Another way to visualize the weights would be in the same panel,

```{r}
not.first <- units.per.layer[-c(1,length(units.per.layer))]
offset.vec <- c(0,cumsum(not.first+1))
dim.dt <- data.table(
  x=offset.vec, 
  rows=sapply(weight.mat.list, nrow),
  cols=sapply(weight.mat.list, ncol))
init.weight.dt[, offset := offset.vec[input.layer.i] ]
ggplot()+
  geom_text(aes(
    x, 0, label=sprintf(
      "%dx%d",
      rows, cols)),
    hjust=0,
    data=dim.dt)+
  geom_tile(aes(
    output.unit.i+offset, input.unit.i, fill=weight),
    color="grey50",
    data=init.weight.dt)+
  scale_y_reverse()+
  coord_equal()+
  scale_fill_gradient2()
```

## Gradient descent {#grad-desc}

Forward and back propagation are implemented below,

```{r}
max.iterations <- 100
scale_vec <- function(x){
  m <- min(x)
  (x-m)/(max(x)-m)
}
scale_mat <- function(mat)apply(mat, 2, scale_vec)
features.unsc <- as.matrix(features.dt)
features.noise <- cbind(
  features.unsc,
  matrix(
    runif(length(features.unsc)),
    nrow(features.unsc),
    ncol(features.unsc)))
features.sc <- scale_mat(log(features.noise+1))
features.sc <- scale(features.unsc)
label.fac <- spam.dt[[which(is.label)]]
label.vec <- ifelse(label.fac=="spam", 1, -1)
table(label.vec)
is.set.list <- list(
  validation=rep(c(TRUE,FALSE), l=nrow(features.sc)))
is.set.list$subtrain <- !is.set.list$validation
n.subtrain <- sum(is.set.list$subtrain)
set.seed(1)
units.per.layer <- c(ncol(features.sc), 100, 1)
weight.node.list <- new_weight_node_list(units.per.layer)
loss.dt.list <- list()
step.size <- 0.2
for(iteration in 1:max.iterations){
  loss.node.list <- list()
  for(set in names(is.set.list)){
    is.set <- is.set.list[[set]]
    set.label.node <- initial_node(label.vec[is.set])
    set.pred.node <- pred_node(features.sc[is.set,])
    set.loss.node <- log_loss(set.pred.node, set.label.node)
    loss.node.list[[set]] <- set.loss.node
    set.pred.num <- ifelse(set.pred.node$value<0, -1, 1)
    is.error <- set.pred.num != set.label.node$value
    loss.dt.list[[paste(iteration, set)]] <- data.table(
      iteration, set, 
      mean.log.loss=set.loss.node$value,
      error.percent=100*mean(is.error))
  }
  loss.node.list$subtrain$backward()
  for(layer.i in seq_along(weight.node.list)){
    weight.node <- weight.node.list[[layer.i]]
    weight.node$value <- weight.node$value-step.size*weight.node$grad
  }  
}
(loss.dt <- rbindlist(loss.dt.list))
ggplot()+
  geom_line(aes(
    iteration, mean.log.loss, color=set),
    data=loss.dt)
```

## Using torch {#torch}

```{r}
##library(torch)
```
