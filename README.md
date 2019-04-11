Replication materials for 'Ensemble Forecasting of Irregular Leadership Change'
================

**[Ensemble Forecasting of Irregular Leadership Change](http://rap.sagepub.com/content/1/3/2053168014557511)**

For questions contact the corresponding author [Michael Ward](mailto:michael.don.ward@gmail.com) or [Andreas Beger](mailto:adbeger@gmail.com).

The complete [original PITF report](http://arxiv.org/abs/1409.7105) is available on arXiv.org, and contains a large amount of additional information on the method we used for forecasting, accuracy assessments, etc.

**Citation:**

Beger, Andreas, Cassy L. Dorff, and Michael D. Ward, 2014, "Ensemble Forecasting of Irregular Regime Change," Research & Politics.

``` bibtex
@article{beger2014ensemble,
  title={Ensemble Forecasting of Irregular Leadership Changes},
  author={Beger, Andreas, Dorff, Cassy L., Ward, Michael D.},
  journal={Research \& Politics},
  year={2014},
  volume={1},
  issue={3},
  pages={1-7}
}    
```

Getting the code and data
-------------------------

The easiest way to get the replication code is to [download a zip](https://github.com/andybega/rap-ensemble-forecasting/archive/master.zip). Alternatively, you can clone the repository through the Github GUI client ([OS X](https://mac.github.com/), [Windows](https://windows.github.com/)).

The data, including several intermediate results, are available on dataverse: <http://dx.doi.org/10.7910/DVN/27482>.

Running the replication
-----------------------

1.  [Download](https://github.com/andybega/rap-ensemble-forecasting/archive/master.zip) or [clone](github-mac://openRepo/https://github.com/andybega/rap-ensemble-forecasting) this repository.

2.  Download the 3 data sets on [Dataverse](http://dx.doi.org/10.7910/DVN/27482) and place them in `replication/data`.

3.  In `runme.R`, change the working directory path on line 33.

4.  Source or run the code in `runme.R`. We recommend running through the code block by block rather than sourcing. The original analysis was run on OS X using R 3.0.2 and 3.1.1.

The script relies on two packages, `EBMAforecastbeta` and `spduration` that are not available on CRAN. They are included in `replication/R/packages` with both OS X and Windows versions. The replication script will attempt to install them if they are not already present, but you may have to do so manually if this fails.

See `replication.pdf` for a list of included files and scripts.

2019-04-11 Update
-----------------

Checked replication and updated several issues. See `runme.R` for more details in the notes at the top. To replicate the exact results, use the saved fitted models and predictions.

``` r
sessionInfo()
```

    ## R version 3.5.2 (2018-12-20)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Mojave 10.14.4
    ## 
    ## Matrix products: default
    ## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_3.5.2  magrittr_1.5    tools_3.5.2     htmltools_0.3.6
    ##  [5] yaml_2.2.0      Rcpp_1.0.1      stringi_1.4.3   rmarkdown_1.12 
    ##  [9] knitr_1.22      stringr_1.4.0   xfun_0.6        digest_0.6.18  
    ## [13] evaluate_0.13
