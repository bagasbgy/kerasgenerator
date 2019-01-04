#!/bin/bash

# update README
#--------------------

# printf
printf '\n\nUpdating README...'
printf '\n--------------------\n\n'

# remove existing README files
rm -rf man/figures/README-*

# render README
Rscript -e 'rmarkdown::render("README.Rmd")'

# remove existing README.html
rm README.html

# build site
#--------------------

# printf
printf '\n\nBuilding site...'
printf '\n--------------------\n\n'

# remove existing documentations
rm -rf docs

# build documentation
Rscript -e 'pkgdown::build_site()'
