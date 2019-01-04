#!/bin/bash

# build site
#--------------------

# printf
printf '\n\nBuilding site...'
printf '\n--------------------\n\n'

# remove existing documentations
rm -rf docs

# build documentation
Rscript -e 'pkgdown::build_site()'
