works_with_R(
  "3.5.1",
  htmltools="0.3.6",
  RJSONIO="1.3.0",
  maps="3.3.0",
  LambertW="0.6.4", #depends/attaches ggplot2
  kernlab="0.9.27",
  "rstudio/rmarkdown@c97053384dae1c0b254f73a354801bbb73176660",
  "hadley/bookdown@12ed348231f26d86e478b9462561cc6c8517992d",
  "tdhock/animint2@7ac92dcc31ea35af6a6cd016e352e7c980e08198")
             
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
