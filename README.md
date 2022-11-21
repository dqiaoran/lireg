### Package "lireg": fitting linear regression models

This is meant to be a practice of building a package from scratch. The package partially reproduces the output of R base function **lm()** and generic functions related to **lm()**. The source code reference can be found in the github website:

<https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/lm.R>

Here are the related S3 classes/generic functions in **lm()** function and their corresponding S3 classes/generic functions in the **lireg** package.

| R base library     | Package "lireg"       |
|--------------------|-----------------------|
| "lm"               | "lireg"               |
| "summary.lm"       | "summary.lireg"       |
| "print.summary.lm" | "print.summary.lireg" |
