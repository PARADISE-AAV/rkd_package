# rkd_package

Repo for development of the RKD analysis package by the PARADISE pre-processing team

## Installation

You can install the latest version of the package directly from GitHub from within an R session using one line of code.

```r
# install.packages('remotes')
remotes::install_github('PARADISE-AAV/rkd_package', ref = 'pkg')
```

As it's a private repository, you'll need to authenticate with GitHub somehow.
More details [here](https://stackoverflow.com/questions/21171142/how-to-install-r-package-from-private-repo-using-devtools-install-github).

Another way (if the above doesn't work) is to download and extract the `.zip` file of the repository using your web browser, or to `git clone` via SSH.
Then, with the package `devtools` installed, you can navigate to the folder containing the package and run
```r
# install.packages('devtools')
devtools::install()
```
or, in RStudio, hit `Ctrl + Shift B` (build) and it should build the package from source.

Once either of the above are complete, you should be able to load the package with

```r
library(rkdpipeline)
```

## Contributors

This package is designed to follow the standard structure of R Packages like submitted to CRAN or BioConductoR.
For more information on best practices, see the free guide [*R Packages* by Hadley Wickham](https://r-pkgs.org/).

If you are a contributor (i.e. you'd like to add some code, tests, documentation, vignettes or examples), then add your name to the `DESCRIPTION` file if you have not done so already.

Make sure to run any changes past `R CMD CHECK` before committing, to avoid issues.
That is, run `Ctrl + Shift + E` from RStudio, or `devtools::check()` and it will check that you don't have any undeclared dependencies, invalid function names and so on.

## The pipeline discussed

![GetImage](https://user-images.githubusercontent.com/120493801/214896546-7de93959-ad72-4696-83ab-dfb446137d0f.png)
