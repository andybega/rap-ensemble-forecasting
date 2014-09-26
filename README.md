Replication materials for "Ensemble Forecasting of Irregular Leadership Change"
=====

Citation:

"blah blah"

    @article{citekey,
      title={},
      author={},
      journal={},
      year={},
      volume={},
      issue={},
      pages={}
    }

Getting the code and data
-----

The easiest way to get the replication code is to download a zip. Alternatively, you can clone the repository through the Github GUI client.

The data, including several intermediate results, are available on dataverse.


Running the replication
-----

Source or run the code in `runme.R`. We recommend running through the code block by block rather than sourcing. The original analysis was run on OS X using R 3.0.2 and 3.1.1.

The script relies on two packages, `EBMAforecastbeta` and `spduration` that are not available on CRAN. They are included in `replication/R/packages` with both OS X and Windows versions. The replication script will attempt to install them if they are not already present, but you may have to do so manually if this fails.

See `replication.pdf` for a list of included files and scripts.