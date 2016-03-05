works_with_R("3.2.2",
             animint="2016.3.1",
             ElemStatLearn="2015.6.26",
             ggplot2="1.0.1")

library(class)

data(mixture.example)

str(mixture.example)

mixture <- with(mixture.example, data.frame(x, label=factor(y)))

set.seed(1)
n.folds <- 5
mixture$fold <- sample(rep(1:n.folds, l=nrow(mixture)))

## model choice: k (number of neighbors).

## 5-fold CV for model selection.

## train and validation error vs k value, select fold and k.

## select: fold, k.

##dput(RColorBrewer::brewer.pal(Inf, "Set1"))
label.colors <-
  c("0"="#E41A1C", "1"="#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
    "#A65628", "#F781BF", "#999999")

ggplot()+
  coord_equal()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_wrap("fold")+
  scale_color_manual(values=label.colors)+
  geom_point(aes(X1, X2, color=label),
             shape=1,
             data=mixture)

input.cols <- c("X1", "X2")
big.grid <- data.frame(mixture.example$xnew)
names(big.grid) <- input.cols
n.grid <- 20
small.grid <- with(mixture, expand.grid(
  X1=seq(min(X1), max(X1), l=n.grid),
  X2=seq(min(X2), max(X2), l=n.grid)))
grid.df <- rbind(
  data.frame(big.grid, set="big.grid"),
  data.frame(small.grid, set="grid"))
grid.df$label <- NA
grid.df$fold <- NA

neighbors.vec <- seq(1, by=2, l=40)
pred.data.list <- list()
error.list <- list()
selected.list <- list()
pred.boundary.list <- list()
getBoundaryDF <- function(prob.vec){
  stopifnot(length(prob.vec) == 6831)
  several.paths <- with(mixture.example, contourLines(
    px1, px2,
    matrix(prob.vec, length(px1), length(px2)),
    levels=0.5))
  contour.list <- list()
  for(path.i in seq_along(several.paths)){
    contour.list[[path.i]] <- with(several.paths[[path.i]], data.frame(
      path.i, X1=x, X2=y))
  }
  do.call(rbind, contour.list)
}
for(validation.fold in 0:n.folds){
  set <- ifelse(mixture$fold == validation.fold, "validation", "train")
  fold.data <- rbind(grid.df, data.frame(mixture, set))
  fold.data$data.i <- 1:nrow(fold.data)
  train.df <- subset(fold.data, set == "train")
  fold.error.list <- list()
  for(neighbors in neighbors.vec){
    pred.label <- 
      knn(train.df[, input.cols],
          fold.data[, input.cols],
          train.df$label,
          k=neighbors,
          prob=TRUE)
    prob.winning.class <- attr(pred.label, "prob")
    fold.data$probability <- ifelse(
      pred.label=="1", prob.winning.class, 1-prob.winning.class)
    fold.data$pred <- factor(pred.label)
    fold.data$is.error <- with(fold.data, pred != label)
    pred.grid <- subset(fold.data, set=="big.grid")
    boundary.df <- getBoundaryDF(pred.grid$probability)
    pred.boundary.list[[paste(validation.fold, neighbors)]] <-
      data.frame(validation.fold, neighbors, boundary.df)
    data.by.set <- split(fold.data, fold.data$set)
    for(set in c("train", "validation")){
      if(set %in% names(data.by.set)){
        set.data <- data.by.set[[set]]
        errors <- sum(set.data$is.error)
        observations <- nrow(set.data)
        error.percent <- 100*errors/observations
        fold.error.list[[paste(neighbors, set)]] <- data.frame(
          neighbors, set, errors, observations, error.percent)
      }
    }
    pred.data.list[[paste(validation.fold, neighbors)]] <- 
      data.frame(validation.fold, neighbors, fold.data)
  }
  fold.error <- do.call(rbind, fold.error.list)
  error.list[[paste(validation.fold)]] <- data.frame(
    validation.fold, fold.error)
  validation.error <- subset(fold.error, set=="validation")
  if(nrow(validation.error)){
    min.error <- subset(validation.error, errors==min(errors))
    simplest <- subset(min.error, neighbors==max(neighbors))
    ggplot()+
      geom_point(aes(neighbors, error.percent, color=set),
                 shape=1,
                 data=simplest)+
      geom_line(aes(neighbors, error.percent, color=set),
                data=fold.error)
    selected.list[[paste(validation.fold)]] <- data.frame(
      validation.fold, simplest)
  }
}
pred.data <- do.call(rbind, pred.data.list)
pred.boundary <- do.call(rbind, pred.boundary.list)
show.data <- subset(pred.data, set %in% c("train", "validation"))
show.grid <- subset(pred.data, set == "grid")
error <- do.call(rbind, error.list)
selected <- do.call(rbind, selected.list)
best <- data.frame(
  set="validation",
  neighbors=ceiling(mean(selected$neighbors)))

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_wrap("validation.fold")+
  geom_vline(aes(xintercept=neighbors, color=set),
             data=best)+
  geom_point(aes(neighbors, error.percent, color=set),
             shape=1,
             data=selected)+
  geom_line(aes(neighbors, error.percent, color=set),
            size=1,
            data=error)

true.boundary <- getBoundaryDF(mixture.example$prob)
right.df <- subset(error, neighbors==max(neighbors))
show.data$status <- ifelse(show.data$is.error, "error", "correct")
show.grid$set <- NULL
viz <- list(
  title="training k-nearest-neighbors via 5-fold cross-validation",
  error=ggplot()+
    theme_bw()+
    geom_vline(aes(xintercept=neighbors, color=set),
               data=best)+
    make_tallrect(error, "neighbors")+
    geom_point(aes(neighbors, error.percent, color=set,
                   clickSelects=validation.fold),
               size=4,
               alpha=0.6,
               data=selected)+
    geom_text(aes(neighbors, error.percent,
                  clickSelects=validation.fold,
                  label=validation.fold, color=set),
              hjust=0,
              data=right.df)+
    geom_line(aes(neighbors, error.percent,
                  clickSelects=validation.fold,
                  group=paste(validation.fold, set),
                  color=set),
              alpha=0.6,
              size=4,
              data=error),
  data=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(. ~ set)+
    theme_animint(width=600)+
    geom_tile(aes(X1, X2, fill=probability,
                  showSelected=neighbors,
                  showSelected2=validation.fold),
               data=show.grid)+
    xlab("Input feature 1")+
    ylab("Input feature 2")+
    geom_point(aes(X1, X2, size=status,
                   key=data.i,
                   showSelected=neighbors,
                   showSelected3=label,
                   showSelected2=validation.fold),
               color="black",
               data=show.data)+
    scale_color_manual(values=label.colors)+
    scale_fill_gradient2(
      low=label.colors[1], midpoint=0.5, high=label.colors[2])+
    scale_size_manual(values=c(error=5, correct=3.5))+
    geom_point(aes(X1, X2, color=label,
                   key=data.i,
                   showSelected=neighbors,
                   showSelected3=status,
                   showSelected2=validation.fold),
               size=3,
               data=show.data)+
    geom_path(aes(X1, X2, group=path.i, linetype=boundary),
              data=data.frame(true.boundary, boundary="true"))+
    geom_path(aes(X1, X2, group=path.i, linetype=boundary,
                  showSelected=neighbors,
                  showSelected2=validation.fold),
              data=data.frame(pred.boundary, boundary="predicted")),
  first=list(validation.fold="0", neighbors=best$neighbors)
)  
animint2dir(viz, "figure-knn")
