## RandomForest2

This package implements random forests [(Breiman, L. Random Forests. Machine Learning 45, 5–32, 2001)](https://doi.org/10.1023/A:1010933404324) in R, adding new functionality and split functions to encompass additional probability distributions. It is based on a modified version of the [rpart](https://cran.r-project.org/web/packages/rpart/index.html) R package that can be found [here](https://github.com/MNLR/rpart).

The package's new methodologies were proposed and thoroughly analyzed — in the context of statistical downscaling of climate — in the article: [MISSING]; and can be summarized as:

* The inclussion of several new split functions intended for predictand variables that are non-normally distributed. We mainly focus on the gamma two-parameter distribution (Deviation and Log Likelihood), although other distributions can be easily added through the [modified rpart package](https://github.com/MNLR/rpart).

* A new approach to regression and classification based on random forests, which employs them as a first step to predict the whole probability distribution of the predictands Y given the predictors X. It has proven to accurately capture Y | X, thus making the approach very useful both for estocastic simulations as well as to characterize the uncertainty of the predictions. We call this approach "a posteriori".


### Installation
The installation process is as follows.

The [modified version of rpart](https://github.com/MNLR/rpart) has to be installed first, as RandomForest2 will not work without it. Use the command 

```
devtools::install_github("MNLR/rpart")
```
Once the dependency is installed, install the additional dependencies from CRAN and the package itself with

```
install.packages(c("progressr", "qmap", "fitdistrplus"))
devtools::install_github("MNLR/RandomForest2")
```

Note that, if `devtools` is not already installed, it can be installed from CRAN using the command `install.packages("devtools")`. 
