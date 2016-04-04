works_with_R("3.2.3",
             "rstudio/rmarkdown@c97053384dae1c0b254f73a354801bbb73176660",
             "hadley/bookdown@12ed348231f26d86e478b9462561cc6c8517992d")
             
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
