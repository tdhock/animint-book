comment_blank_lines <- function(string){
  gsub("\n\n", "\n\n<!-- comment -->\n\n", string)
}
glob <- "Ch*.Rmd"
Rmd <- "Ch10-nearest-neighbors.Rmd"
for(Rmd in Sys.glob(glob)){
  non.greedy.lines <- list(
    list(".*\n"), "*?")
  code.start <- "```\\{r"
  code.pattern <- list(
    code_header=list(
      code.start,
      ".*\\}\n"),
    code=non.greedy.lines,
    "```")
  Rmd.dt <- nc::capture_all_str(
    Rmd,
    before=non.greedy.lines,
    code_chunk=code.pattern,
    after=list(
      nc::quantifier(".*\n(?!", code.start, ")", "*"),
      ".*")
  )
  out.Rmd <- file.path("commented", Rmd)
  cat(sprintf(
    "output %s with %d code chunks with blank lines\n",
    out.Rmd, sum(grepl("\n\n", Rmd.dt$code))))
  dir.create("commented", showWarnings = FALSE)
  out.text <- Rmd.dt[, paste0(
    comment_blank_lines(before), code_chunk, comment_blank_lines(after)
  )]
  cat(out.text, sep="\n", file=out.Rmd)
}  

