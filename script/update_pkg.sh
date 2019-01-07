#!/bin/bash

# documentation
#--------------------

# printf
printf '\n\nUpdating docs...'
printf '\n--------------------\n\n'

# remove existing documentations
rm -rf man/*.Rd

# build documentation
Rscript -e 'devtools::document()'

# documentation
#--------------------

# printf
printf '\n\nBuilding...'
printf '\n--------------------\n\n'

# remove existing inst/doc
rm -rf inst/doc

# build the package
Rscript -e 'devtools::build()'

# update installation
#--------------------

# printf
printf '\n\nUpdating...'
printf '\n--------------------\n\n'

# install the package
Rscript -e 'devtools::install()'

# check
#--------------------

# printf
printf '\n\nRunning check...'
printf '\n--------------------\n\n'

# run check
Rscript -e 'devtools::check()'

# end
#--------------------

# printf
printf '\n> Package update finished!\n\n'
