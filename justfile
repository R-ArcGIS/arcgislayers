default:
    just --list

readme:
  R -q -e "rmarkdown::render('README.Rmd')"

fmt:
    air format R/*.R

prune:
  git branch | grep -v "main" | xargs git branch -D
