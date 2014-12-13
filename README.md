blowtorch v1.0.2
=========
### Constrained optimization via stochastic gradient descent.
_Optimize a (smooth) function with constraints via a transformation of the Lagrangian._

### Installation
This package is available on [CRAN](http://cran.r-project.org/web/packages/blowtorch/index.html) however you can download the latest version from here with 

```
devtools::install_github("blowtorch", username = "stevenpollack")
```

Note that `blowtorch` has a companion package, [`mixtureModel`](https://github.com/stevenpollack/mixtureModel) (not on CRAN) which is used for the various files in `inst/`. The easiest way to pick that up is, again

```
devtools::install_github("mixtureModel", username = "stevenpollack")
```

### Usage
The workhorse of this package is `blowtorch::SGD`, read the man page for more details.
