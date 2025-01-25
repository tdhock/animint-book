### Write down what package versions work with your R code, and
### attempt to download and load those packages. The first argument is
### the version of R that you used, e.g. "3.0.2" and then the rest of
### the arguments are package versions. For
### CRAN/Bioconductor/R-Forge/etc packages, write
### e.g. RColorBrewer="1.0.5" and if RColorBrewer is not installed
### then we use install.packages to get the most recent version, and
### warn if the installed version is not the indicated version. For
### GitHub packages, write "user/repo@commit"
### e.g. "tdhock/animint@f877163cd181f390de3ef9a38bb8bdd0396d08a4" and
### we use install_github to get it, if necessary.
works_with_R <- function(Rvers,...){
  local.lib <- file.path(getwd(), "library")
  dir.create(local.lib, showWarnings=FALSE, recursive=TRUE)
  .libPaths(c(local.lib, .libPaths()))
  message(paste(c("R .libPaths() is",.libPaths()),collapse=":"))
  pkg_ok_have <- function(pkg,ok,have){
    stopifnot(is.character(ok))
    if(!as.character(have) %in% ok){
      warning("works with ",pkg," version ",
              paste(ok,collapse=" or "),
              ", have ",have)
    }
  }
  pkg_ok_have("R",Rvers,getRversion())
  pkg.vers <- list(...)
  for(pkg.i in seq_along(pkg.vers)){
    vers <- pkg.vers[[pkg.i]]
    pkg <- if(is.null(names(pkg.vers))){
      ""
    }else{
      names(pkg.vers)[[pkg.i]]
    }
    if(pkg == ""){# Then it is from GitHub.
      ## suppressWarnings is quieter than quiet.
      if(!suppressWarnings(require(requireGitHub))){
        ## If requireGitHub is not available, then install it using
        ## devtools.
        if(!suppressWarnings(require(devtools))){
          install.packages("devtools")
          require(devtools)
        }
        install_github("tdhock/requireGitHub")
        require(requireGitHub)
      }
      requireGitHub(vers)
    }else{# it is from a CRAN-like repos.
      if(!suppressWarnings(require(pkg, character.only=TRUE))){
        install.packages(pkg)
      }
      pkg_ok_have(pkg, vers, packageVersion(pkg))
      library(pkg, character.only=TRUE)
    }
  }
}
works_with_R(
  "4.5.0",
  penaltyLearning="2024.9.3",
  future="1.34.0",
  future.apply="1.11.2",
  htmltools="0.5.7",
  RJSONIO="1.3.1.9",
  maps="3.4.2",
  lars="1.3",
  LambertW="0.6.9.1", #depends/attaches ggplot2
  kernlab="0.9.32",
  caTools="1.18.2",
  data.table="1.16.99",
  "rstudio/rmarkdown@c97053384dae1c0b254f73a354801bbb73176660",
  "tdhock/bookdown@12ed348231f26d86e478b9462561cc6c8517992d",
  "animint/animint2@f638615162e1dd6864844c7d92dd96f06a5eca9d")

library(methods)
args <- commandArgs(trailingOnly = TRUE)
##print(args)##NO: that will appear on top of web page!
message(paste(c("R trailing commandArgs:", args), collapse=" "))
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
