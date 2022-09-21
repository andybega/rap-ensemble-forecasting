Replication materials for ‘Ensemble Forecasting of Irregular Leadership
Change’
================

This repo contains replication materials for:

Beger, Andreas, Cassy L. Dorff, and Michael D. Ward, 2014, “Ensemble
Forecasting of Irregular Regime Change,” Research & Politics 1(3): .

-   Journal link: <https://doi.org/10.1177/2053168014557511>
-   The complete [original PITF report](http://arxiv.org/abs/1409.7105)
    is available on arXiv.org, and contains a large amount of additional
    information on the method we used for forecasting, accuracy
    assessments, etc.
-   Dataverse for data: <http://dx.doi.org/10.7910/DVN/27482>
-   For questions, contact [Andreas Beger
    (adbeger@gmail.com)](mailto:adbeger@gmail.com).

------------------------------------------------------------------------

*Note:*

*Last updated on 2022-09-21*

*The replication now uses the current versions of {spduration} and
{EBMAforecast} (which replaced the original {EBMAforecastbeta} package)
from CRAN. As a result the original paper results do not exactly match
the current output. See below for more details.*

------------------------------------------------------------------------

**Bibtex citation:**

``` bibtex
@article{beger2014ensemble,
  title={Ensemble Forecasting of Irregular Leadership Changes},
  author={Beger, Andreas, Dorff, Cassy L., Ward, Michael D.},
  journal={Research \& Politics},
  year={2014},
  volume={1},
  issue={3},
  pages={1-7},
  doi={https://doi.org/10.1177/2053168014557511}
}    
```

## Getting the code and data

The easiest way to get the replication code is to [download a
zip](https://github.com/andybega/rap-ensemble-forecasting/archive/master.zip).
Alternatively, you can clone the repository through the Github GUI
client ([OS X](https://mac.github.com/),
[Windows](https://windows.github.com/)).

The data, including several intermediate results, are available on
dataverse: <http://dx.doi.org/10.7910/DVN/27482>.

## Running the replication

1.  [Download](https://github.com/andybega/rap-ensemble-forecasting/archive/master.zip)
    or
    [clone](github-mac://openRepo/https://github.com/andybega/rap-ensemble-forecasting)
    this repository.

2.  Download the 3 data sets on
    [Dataverse](http://dx.doi.org/10.7910/DVN/27482) and place them in
    `replication/data`.

3.  In `runme.R`, change the working directory path on line 33.

4.  Source or run the code in `runme.R`. We recommend running through
    the code block by block rather than sourcing. The original analysis
    was run on OS X using R 3.0.2 and 3.1.1.

The script relies on two packages, `EBMAforecastbeta` and `spduration`
that are not available on CRAN. They are included in
`replication/R/packages` with both OS X and Windows versions. The
replication script will attempt to install them if they are not already
present, but you may have to do so manually if this fails.

See `replication.pdf` for a list of included files and scripts.

## 2019-04-11 Update

Checked replication and updated several issues. See `runme.R` for more
details in the notes at the top.

To replicate the exact results, use the saved fitted models and
predictions.

## 2022-09-21 Update

I was unable to used the saved models to exactly replicate the forecasts
and other materials in the 2014 paper. However, the code does run with
the current CRAN versions of {spduration} and {EBMAforecast} (which
replicated {EBMAforecastbeta}).

There are some changes, e.g. Table 1 in the paper now looks like this:

| Model          | AUC_is |  F_is | AUC_oos | F_oos |
|:---------------|-------:|------:|--------:|------:|
| Ensemble       |  0.870 | 0.079 |   0.850 | 0.148 |
| Logit          |  0.951 | 0.286 |   0.776 | 0.059 |
| Split-duration |  0.742 | 0.085 |   0.782 | 0.121 |

The forecasts are also different. Most consequentially, since it had an
ILC, Thailand now has the 3rd highest forecast. In the original results
it had the 5th highest.

The original versions of the {EBMAforecastbeta} and {spduration}
packages that were used back in 2014 are still included under
[`/replication/R/packages/`](/replication/R/packages/). However, I could
not get them to work with the most recent version of R.

------------------------------------------------------------------------

``` r
sessionInfo()
```

    ## R version 4.1.2 (2021-11-01)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Big Sur 10.16
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_4.1.2  magrittr_2.0.3  fastmap_1.1.0   cli_3.3.0      
    ##  [5] tools_4.1.2     htmltools_0.5.2 rstudioapi_0.13 yaml_2.3.5     
    ##  [9] stringi_1.7.8   rmarkdown_2.14  knitr_1.39      stringr_1.4.0  
    ## [13] xfun_0.30       digest_0.6.29   rlang_1.0.4     evaluate_0.16
