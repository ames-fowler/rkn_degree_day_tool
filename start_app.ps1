Set-Location -Path $PSScriptRoot
Rscript -e "shiny::runApp('.', host='127.0.0.1', port=3839, launch.browser=TRUE)"
