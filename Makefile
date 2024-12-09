README.md : README.Rmd
	R -e "library(rmarkdown); render('README.Rmd')"

submission/manuscript.pdf submission/manuscript.docx submission/manuscript.pptx: submission/manuscript.Rmd
	R -e 'library(rmarkdown); render("submission/manuscript.Rmd", output_format ="all")'

submission/outline.pdf submission/outline.docx : submission/outline.Rmd
	R -e 'library(rmarkdown); render("submission/outline.Rmd", output_format ="all")'

visuals/ppt_presentation.pdf visuals/ppt_presentation.pptx : visuals/ppt_presentation.Rmd
	R -e 'library(rmarkdown); render("visuals/ppt_presentation.Rmd", output_format ="all")'