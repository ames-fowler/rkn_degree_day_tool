@echo off
cd /d "%~dp0"
Rscript -e "shiny::runApp('.', host='127.0.0.1', port=3839, launch.browser=TRUE)"
pause
