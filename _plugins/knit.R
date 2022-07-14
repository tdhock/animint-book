works_with_R(
  "4.0.2",
  penaltyLearning="2020.5.13",
  future="1.18.0",
  future.apply="1.6.0",
  htmltools="0.5.0",
  RJSONIO="1.3.1.4",
  maps="3.3.0",
  lars="1.2",
  LambertW="0.6.5", #depends/attaches ggplot2
  kernlab="0.9.29",
  caTools="1.18.0",
  data.table="1.13.0",
  "rstudio/rmarkdown@c97053384dae1c0b254f73a354801bbb73176660",
  "hadley/bookdown@12ed348231f26d86e478b9462561cc6c8517992d",
  "tdhock/animint2@0a2988ff02f73aab7ec6c3fb70d14439092bee27")
             
library(methods)

args <- commandArgs(trailingOnly = TRUE)
path <- args[1]

if (!file.exists(path)) {
  stop("Can't find path ", path, call. = FALSE)
}

if (file.access(path, 4) != 0) {
  stop("Can't read path ", path, call. = FALSE)
}

path.dir <- dirname(path)
setwd(path.dir)
path.base <- basename(path)
html_path <- render(
  path.base,
  html_chapter(raw = TRUE, toc = "toc.rds"),
  quiet = TRUE)

read_file <- function(path) {
  size <- file.info(path)$size
  readChar(path, size, useBytes = TRUE)
}
cat(read_file(html_path))
