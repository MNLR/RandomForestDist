## RandomForest2

Building on a modified version of [rpart](https://cran.r-project.org/web/packages/rpart/index.html) which can be found [here](https://github.com/MNLR/rpart), this package implements advanced functionalities for random forests [(Breiman, L. Random Forests. Machine Learning 45, 5–32, 2001)](https://doi.org/10.1023/A:1010933404324) which make this technique suitable for statistical downscaling of precipitation, as analyzed in *Legasa et al. 2021* (submitted to *Water Resources Research*). The key elements of [RandomForest2](https://github.com/MNLR/RandomForest2) are:

* The inclussion of several split functions intended for predictand variables that are non-normally distributed. In *Legasa et al. 2021* , we focus on the two-parameter gamma distribution (Deviation and Log Likelihood). However, other distributions can be easily added through the [modified rpart package](https://github.com/MNLR/rpart).

* A new approach called "a posteriori" which has proven to accurately capture the whole probability distribution of the predictand *Y* given the predictors *X*, allowing thus for the generation of reliable stochastic predictions. 

Please refer to the [notebook](https://github.com/MNLR/RandomForest2/blob/master/WorkedExample.ipynb) included in the package for examples of use. 

### Installation
The installation process is as follows:

The [modified version of rpart](https://github.com/MNLR/rpart) has to be installed first, as [RandomForest2](https://github.com/MNLR/RandomForest2) will not work without it 

```
devtools::install_github("MNLR/rpart")
```
Afterwards, install the additional dependencies from CRAN and the package itself with

```
install.packages(c("progressr", "qmap", "fitdistrplus"))
devtools::install_github("MNLR/RandomForest2")
```

Note that, in case `devtools` is not already available, it can be installed from CRAN using the command `install.packages("devtools")`. 
