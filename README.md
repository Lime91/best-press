# best-press

A small university project to perform variable selection in `R` based on [Allen's PRESS statistic](https://en.wikipedia.org/wiki/PRESS_statistic).

Developed on `ubuntu` 20.04 with `R` 3.6.3 (2020-02-29).

## Dependencies

The following `R`-packages are used:
* `dplyr` (1.0.2)
* `kableExtra` (1.3.1)
* `leaps` (3.1)
* `MPV` (1.57)
* `rmarkdown` (2.5)
* `stringr` (1.4.0)

`pandoc` 2.5 is used in the `rmarkdown` backend.

To compile pdfs, `pdfTeX` 3.14159265-2.6-1.40.20 (TeX Live 2019/Debian) is used.

## Run

Open a terminal and type

`Rscript -e "rmarkdown::render('Seminar_tutorial.Rmd')"`



