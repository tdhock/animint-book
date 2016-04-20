03-showSelected.html: 03-showSelected.Rmd
	Rscript -e 'rmarkdown::render("03-showSelected.Rmd")'
XX-nearest-neighbors-example.html: XX-nearest-neighbors-example.Rmd
	Rscript -e 'rmarkdown::render("XX-nearest-neighbors-example.Rmd")'
02-ggplot2.html: 02-ggplot2.Rmd
	Rscript -e 'rmarkdown::render("02-ggplot2.Rmd")'
pngs:
	for p in *.png; do convert $p -resize 500 $p;done
