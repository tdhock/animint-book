source("packages.R")

data(mixture.example)

str(mixture.example)

##dput(RColorBrewer::brewer.pal(Inf, "Set1"))
label.colors <-
  c("0"="#E41A1C", "1"="#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
    "#A65628", "#F781BF", "#999999")
set.colors <-
  c("0"="#E41A1C", "1"="#377EB8",
    validation="#4DAF4A",
    test="#984EA3",
    train="#FF7F00", "#FFFF33", 
    "#A65628", "#F781BF", "#999999")
input.cols <- c("X1", "X2")

## from help(mixture.example)
library(MASS)
set.seed(123)
centers <- c(sample(1:10, 5000, replace=TRUE), 
             sample(11:20, 5000, replace=TRUE))
mix.test <- mvrnorm(10000, c(0,0), 0.2*diag(2))
test.points <- data.frame(
  mix.test + mixture.example$means[centers,],
  label=factor(c(rep(0, 5000), rep(1, 5000))))

mixture <- with(mixture.example, data.frame(x, label=factor(y)))
big.grid <- data.frame(mixture.example$xnew, label=NA)
names(big.grid)[1:2] <- input.cols
n.grid <- 20
small.grid <- with(mixture, expand.grid(
  X1=seq(min(X1), max(X1), l=n.grid),
  X2=seq(min(X2), max(X2), l=n.grid)))
small.grid$label <- NA
grid.df <- rbind(
  data.frame(test.points, set="test"),
  data.frame(big.grid, set="big.grid"),
  data.frame(small.grid, set="grid"))
grid.df$fold <- NA
neighbors.vec <- seq(1, by=2, l=40)
neighbors.vec <- 1:30
neighbors.vec <- seq(1, 30, by=4)

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
for(n.folds in c(3, 5)){
  ## My 10-fold cross-validation split.
  set.seed(1)
  mixture$fold <- sample(rep(1:n.folds, l=nrow(mixture)))

  ggplot()+
    coord_equal()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_wrap("fold")+
    scale_color_manual(values=label.colors)+
    geom_point(aes(X1, X2, color=label),
               shape=1,
               data=mixture)

  big.info <- with(mixture.example, data.frame(big.grid, prob, marginal))
  ggplot()+
    scale_fill_gradient2(midpoint=0.5)+
    geom_tile(aes(X1, X2, fill=prob),
              data=big.info)
  ggplot()+
    geom_tile(aes(X1, X2, fill=marginal),
              data=big.info)
  ggplot()+
    geom_tile(aes(X1, X2, fill=label),
              data=big.info)

  for(validation.fold in 0:n.folds){
    set <- ifelse(mixture$fold == validation.fold, "validation", "train")
    fold.data <- rbind(grid.df, data.frame(mixture, set))
    fold.data$data.i <- 1:nrow(fold.data)
    train.df <- subset(fold.data, set == "train")
    fold.error.list <- list()
    for(neighbors in neighbors.vec){
      cat(sprintf("n.folds=%4d validation.fold=%d neighbors=%d\n",
                  n.folds, validation.fold, neighbors))
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
      pred.boundary.list[[paste(n.folds, validation.fold, neighbors)]] <-
        data.table(n.folds, validation.fold, neighbors, boundary.df)
      data.by.set <- split(fold.data, fold.data$set)
      for(set in c("train", "validation", "test")){
        if(set %in% names(data.by.set)){
          set.data <- data.by.set[[set]]
          errors <- sum(set.data$is.error)
          observations <- nrow(set.data)
          error.percent <- 100*errors/observations
          fold.error.list[[paste(neighbors, set)]] <- data.table(
            neighbors, set, errors, observations, error.percent)
        }
      }#for(set
      pred.data.list[[paste(n.folds, validation.fold, neighbors)]] <- 
        data.table(n.folds, validation.fold, neighbors, fold.data)
    }
    fold.error <- do.call(rbind, fold.error.list)
    ggplot()+
      geom_line(aes(neighbors, error.percent, color=set),
                data=fold.error)
    error.list[[paste(n.folds, validation.fold)]] <- data.table(
      n.folds, validation.fold, fold.error)
    validation.error <- subset(fold.error, set=="validation")
    if(nrow(validation.error)){
      min.error <- subset(validation.error, errors==min(errors))
      simplest <- subset(min.error, neighbors==max(neighbors))
      fewest <- subset(min.error, neighbors==min(neighbors))
      mean.neighbors <- mean(min.error$neighbors)
      average <- data.table(
        neighbors=mean.neighbors,
        set="validation",
        errors=NA,
        observations=NA,
        error.percent=with(validation.error, {
          approx(neighbors, error.percent, mean.neighbors)$y
        }))
      both <- rbind(
        data.table(average, rule="mean.neighbors"),
        data.frame(simplest, rule="max.neighbors"),
        data.frame(fewest, rule="min.neighbors"))
      ggplot()+
        geom_point(aes(neighbors, error.percent, color=set),
                   shape=1,
                   data=simplest)+
        geom_line(aes(neighbors, error.percent, color=set),
                  data=fold.error)
      selected.list[[paste(n.folds, validation.fold)]] <- data.table(
        n.folds, validation.fold, both)
    }#nrow(validation.error
  }#for(validation.fold
}#for(n.folds
pred.data <- do.call(rbind, pred.data.list)
pred.boundary <- do.call(rbind, pred.boundary.list)
show.data <- subset(pred.data, set %in% c("train", "validation"))
show.grid <- subset(pred.data, set == "grid")
error <- do.call(rbind, error.list)
error.stats <- error[, list(
  mean=mean(error.percent), sd=sd(error.percent), error="knn"
  ), by=.(n.folds, set, neighbors)]
validation.stats <- error.stats[set=="validation",]
error.by.neighbors <- split(validation.error, validation.error$neighbors)
selected <- do.call(rbind, selected.list)
best <- selected[, list(
  neighbors.combined=as.integer(mean(neighbors))
  ), by=.(set, n.folds, rule)]
full.error <- subset(error, validation.fold==0)
full.error$error <- "knn"
bayes.error <- data.frame(error.percent=21, set="test", error="Bayes")
validation.stats.best <- validation.stats[, .SD[which.min(mean),], by=n.folds]
one.full.error <- full.error[n.folds==min(n.folds), ]
test.curve <- one.full.error[set=="test",]
min.test.error <- test.curve[which.min(errors), ]

ggplot()+
  geom_hline(aes(yintercept=neighbors),
             data=min.test.error)+
  geom_point(aes(n.folds, neighbors, color="min.mean.error"),
             data=validation.stats.best)+
  geom_point(aes(n.folds, neighbors.combined, color=rule),
             data=best)

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))+
  facet_grid(n.folds ~ .)+
  geom_point(aes(neighbors, validation.fold),
             data=selected)

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))+
  facet_grid(n.folds ~ .)+
  scale_color_manual(values=set.colors,
                     breaks=c("test", "validation", "train"))+
  scale_fill_manual(values=set.colors)+
  guides(fill="none")+
  scale_linetype_manual(values=c(Bayes="dashed", knn="solid"))+
  geom_hline(aes(yintercept=error.percent, linetype=error, color=set),
             data=bayes.error)+
  geom_line(aes(neighbors, error.percent, color=set, linetype=error),
            data=full.error)+
  geom_ribbon(aes(neighbors,
                  ymin=mean-sd/sqrt(n.folds),
                  ymax=mean+sd/sqrt(n.folds),
                  fill=set),
              alpha=0.5,
              data=validation.stats)+
  geom_point(aes(neighbors, mean, color=set),
             data=validation.stats.best)+
  geom_line(aes(neighbors, mean, color=set, linetype=error),
            data=validation.stats)

ggplot()+
  scale_color_manual(values=set.colors,
                     breaks=c("test", "validation", "train"))+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(n.folds ~ validation.fold)+
  geom_vline(aes(xintercept=neighbors.combined, color=set),
             data=best)+
  geom_point(aes(neighbors, error.percent, color=set),
             shape=1,
             data=selected)+
  geom_line(aes(neighbors, error.percent, color=set),
            size=1,
            data=error)

Bayes.boundary <- getBoundaryDF(mixture.example$prob)
right.df <- subset(error, neighbors==max(neighbors))
show.data$status <- ifelse(show.data$is.error, "error", "correct")
show.grid$set <- NULL
min.test.error$n.folds <- NULL
viz <- list(
  title=paste0("training k-nearest-neighbors via ",
    n.folds, "-fold cross-validation"),
  error=ggplot()+
    geom_vline(aes(xintercept=neighbors, color=set),
               size=1,
               linetype="dashed",
               data=min.test.error)+
    geom_point(aes(neighbors, n.folds, color=set),
               data=data.frame(validation.stats.best, facet="n.folds"))+
    geom_point(aes(neighbors.combined, n.folds, color=set,
                   key=n.folds,
                   showSelected=rule),
               data=data.frame(best, facet="n.folds"))+
    theme_bw()+
    geom_ribbon(aes(neighbors, ymin=mean-sd, ymax=mean+sd, fill=set,
                    showSelected2=set,
                    showSelected=n.folds),
                alpha=0.5,
                data=data.frame(validation.stats, facet="details"))+
    geom_line(aes(neighbors, mean, color=set,
                  linetype=fold,
                  showSelected=n.folds),
              data=data.frame(validation.stats, fold="mean", facet="details"))+
    geom_point(aes(neighbors, mean, color=set,
                   showSelected=n.folds),
               data=data.frame(validation.stats.best, facet="details"))+
    theme(panel.margin=grid::unit(0, "cm"))+
    geom_point(aes(neighbors, validation.fold, color=set,
                   showSelected2=rule,
                   showSelected=n.folds),
               data=data.frame(selected, facet="validation.fold"))+
    geom_vline(aes(xintercept=neighbors.combined, color=set,
                   showSelected2=rule,
                   showSelected=n.folds),
               data=data.frame(best, facet="validation.fold"))+
    facet_grid(facet ~ ., scales="free")+
    scale_fill_manual(values=set.colors)+
    scale_color_manual(values=set.colors,
                       breaks=c("test", "validation", "train"))+
    guides(fill="none")+
    scale_linetype_manual(values=c(mean="dashed", selected="solid"))+
    geom_line(aes(neighbors, error.percent,
                  linetype=fold,
                  showSelected=validation.fold,
                  showSelected2=n.folds,
                  group=set,
                  key=set,
                  color=set),
              data=data.frame(error, fold="selected", facet="details"))+
    make_tallrect(error, "neighbors")+
    geom_widerect(aes(ymin=validation.fold-0.5, ymax=validation.fold+0.5,
                      showSelected=n.folds,
                      clickSelects=validation.fold),
                  alpha=0.5,
                  data=data.frame(
                    selected[rule==rule[1],], facet="validation.fold"))+
    geom_widerect(aes(ymin=n.folds-0.5, ymax=n.folds+0.5,
                      clickSelects=n.folds),
                  alpha=0.5,
                  data=data.frame(validation.stats.best, facet="n.folds"))+
    ylab("")+
    geom_point(aes(neighbors, error.percent, color=set,
                   key=rule,
                   clickSelects=rule,
                   showSelected=validation.fold,
                   showSelected2=n.folds),
               size=4,
               data=data.frame(selected, facet="details"))+
    geom_segment(aes(0, error.percent,
                     xend=30, yend=error.percent,
                     color=set),
                 size=1,
                 data=data.frame(bayes.error, facet="details"))+
    geom_text(aes(0, error.percent, color=set, label="Bayes"),
              hjust=1,
              data=data.frame(bayes.error, facet="details"))+
    geom_text(aes(neighbors, 33, color=set,
                  clickSelects=validation.fold,
                  label=" best test error"),
              hjust=0,
              data=data.frame(min.test.error, facet="details")),
  ## error=ggplot()+
  ##   theme_bw()+
  ##   geom_vline(aes(xintercept=neighbors.before, color=set),
  ##              data=best)+
  ##   make_tallrect(error, "neighbors")+
  ##   geom_point(aes(neighbors, error.percent, color=set,
  ##                  clickSelects=validation.fold),
  ##              size=4,
  ##              alpha=0.6,
  ##              data=selected)+
  ##   geom_text(aes(neighbors, error.percent,
  ##                 clickSelects=validation.fold,
  ##                 label=validation.fold, color=set),
  ##             hjust=0,
  ##             data=right.df)+
  ##   geom_line(aes(neighbors, error.percent,
  ##                 clickSelects=validation.fold,
  ##                 group=paste(validation.fold, set),
  ##                 color=set),
  ##             alpha=0.6,
  ##             size=4,
  ##             data=error),
  data=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(. ~ set)+
    theme_animint(width=600)+
    geom_tile(aes(X1, X2, fill=probability,
                  showSelected=neighbors,
                  showSelected3=n.folds,
                  showSelected2=validation.fold),
               data=show.grid)+
    xlab("Input feature 1")+
    ylab("Input feature 2")+
    geom_point(aes(X1, X2, size=status,
                   key=data.i,
                   showSelected=neighbors,
                   showSelected3=label,
                   showSelected4=n.folds,
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
                   showSelected2=validation.fold,
                   showSelected4=n.folds),
               data=show.data)+
    geom_path(aes(X1, X2, group=path.i, linetype=boundary),
              data=data.frame(Bayes.boundary, boundary="Bayes"))+
    geom_path(aes(X1, X2, group=path.i, linetype=boundary,
                  key=paste(path.i, neighbors, n.folds, validation.fold),
                  showSelected=neighbors,
                  showSelected3=n.folds,
                  showSelected2=validation.fold),
              data=data.frame(pred.boundary, boundary="predicted")),
  first=list(
    n.folds=3,
    validation.fold="0",
    neighbors=min.test.error$neighbors
    ),
  duration=list(
    n.folds=1000,
    validation.fold=1000,
    neighbors=1000,
    rule=1000,
    )
)  
viz$error + facet_grid(facet ~ n.folds, scales="free")

animint2dir(viz, "figure-knn")
