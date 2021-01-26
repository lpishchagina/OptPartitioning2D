<a id="top"></a>


#OptPartitioning2D Vignette
### Liudmila Pishchagina
### January  26, 2021


> [Quick Start](#qs)

<a id="qs"></a>
## Quick Start

We present a basic use of the main functions of the `OptPartitioning2D` package. 

We install the package from Github:

```r
#devtools::install_github("lpishchagina/OptPartitioning2D")
library(OptPartitioning2D)
```
The `GenData2D` function simulates a bivariate time series with the arguments:

`n`  is a time series length.

`changepoints` is a changepoint vector that gives the last index of each segment. The last element of `changepoints` always equals to the length of time series `Data`.

`means1` is a vector of means for the first univariate time series. The length of this vector is equal to the length of `changepoints`.

`means2` is a vector of means for the second univariate time series. The length of this vector is equal to the length of `changepoints`.

`noise1` is a variance of the first univariate time series(by default it is equal to `1`).

`noise2` is a variance of the second univariate time series(by default it is equal to `1`).

```r
n <- 100
Data <- GenData2D(n, changepoints = c(20, 40, 60, 80, 100), means1 = c(0, 1, 0, 1, 0), means2 = c(1, 2, 3, 4, 5), noise1 = 1,  noise2 = 1)
```


The `OptPart2D` function returns the result of the segmentation using the parameters:

`data1` is the first univariate time series.

`data2` is the second univariate time series.

`penalty` is a value of penalty (a non-negative real number). The `penalty` here equals to a classic `2*(noise^2)*log(n)`. 

`type` is a parameter defining the algorithm of segmentation. The `type` must be either `"null"` or `"pruning"`.
 
We use Optimal Partitioning algorithm when `type = "null"` and  PELT-algorithm when `type = "pruning"`.

We choose a gaussian cost.

```r
OptPart <- OptPart2D(Data[1,], Data[2,], penalty = 2 * log(n) , type="null")

OptPELT <- OptPart2D(Data[1,], Data[2,], penalty = 2 * log(n) , type="pruning")
```

```
## OptPart$changepoints
## 
## OptPart$means1
## 
## OptPart$means2
##
## OptPart$globalCost
##
```

```
## OptPELT$changepoints
## 
## OptPELT$means1
## 
## OptPELT$means2
##
## OptPELT$globalCost
##
```

`changepoints` is a  vector of the inferred changepoints.

`means1`  is a vector of the inferred means for the first univariate time series. 

`means2`  is a vector of the inferred means for the second univariate time series. 
  
`globalCost` is the overall gaussian cost of the segmented data. 


The function `PlotOptPart2D` plots a graph for each univariate time series of the bivariate series.

The graph shows the univariate time series, the estimated values of means and the inferred segments. 

The function `PlotOptPart2D` uses the parameters:

`data` is the bivariate time series.

`changepoints` is a  vector of the inferred changepoints.

`means1`  is a vector of the inferred means for the first univariate time series. 

`means2`  is a vector of the inferred means for the second univariate time series.

```r
PlotOptPart2D(data = Data, changepoints = c(20, 40, 60, 80, 100), means1 = c(0, 1, 0, 1, 0), means2 = c(1, 2, 3, 4, 5))
```
[Back to Top](#top)