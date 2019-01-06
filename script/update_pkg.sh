#!/bin/bash

# documentation
#--------------------

# printf
printf '\n\nBuilding documentation...'
printf '\n--------------------\n\n'

# remove existing documentations
rm -rf man/*.Rd

# build documentation
Rscript -e 'devtools::document()'

# update installation
#--------------------

# printf
printf '\n\nUpdating the package installation...'
printf '\n--------------------\n\n'

# install the package
Rscript -e 'devtools::install()'

# testing
#--------------------

# printf
printf '\n\nRunning tests...'
printf '\n--------------------\n\n'

# run test
Rscript -e 'devtools::test()'

# end
#--------------------

# printf
printf '\n> Updating progress finished!\n\n'
