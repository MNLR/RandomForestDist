## RandomForest2

Building on a modified version of [rpart](https://cran.r-project.org/web/packages/rpart/index.html) which can be found [here](https://github.com/MNLR/rpart), this package implements advanced functionalities for random forests [(Breiman, L. Random Forests. Machine Learning 45, 5–32, 2001)](https://doi.org/10.1023/A:1010933404324) which make this technique suitable for statistical downscaling of precipitation, as analyzed in *Legasa et al. 2021* (submitted to *Water Resources Research*). The key elements of [RandomForest2](https://github.com/MNLR/RandomForest2) are:

* The inclussion of several split functions intended for predictand variables that are non-normally distributed. We mainly focus on the gamma two-parameter distribution (Deviation and Log Likelihood), although other distributions can be easily added through the [modified rpart package](https://github.com/MNLR/rpart).

* A new approach called "a priori" which has proven to accurately capture the whole probability distribution of the predictands Y given the predictors X, allowing thus for the generation of reliable stochastic predictions. 

Please refer to the [notebook](https://github.com/MNLR/RandomForest2/blob/master/WorkedExample.ipynb) included with this package for examples of use. 

### Installation
The installation process is as follows.

The [modified version of rpart](https://github.com/MNLR/rpart) has to be installed first, as RandomForest2 will not work without it. Use the command 

```
devtools::install_github("MNLR/rpart")
```
Once this dependency is installed, install the additional dependencies from CRAN and the package itself with

```
install.packages(c("progressr", "qmap", "fitdistrplus"))
devtools::install_github("MNLR/RandomForest2")
```

Note that, if `devtools` is not already installed, it can be installed from CRAN using the command `install.packages("devtools")`. 
