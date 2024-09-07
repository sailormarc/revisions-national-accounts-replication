# Revisions to German and Swiss national accounts: are initial releases good nowcasts?

This project is a partial replication of T. Strohsal, E. Wolf: "Data revisions to German national accounts: are initial releases good nowcasts?", *International Journal of Forecasting*, vol. 36, issue 4, pp. 1252-1259, 2020 ([DOI](https://doi.org/10.1016/j.ijforecast.2019.12.006)).

In the script `replication.R`, we replicate Tables 2 and 3 from the article, showing that German national accounts are biased, large, and predictable. We also partially replicate Table 4, a nowcasting exercise to improve initial releases using a Kalman filter. Finally, we perform some robustness checks to partially replicate Table 5 from the article, and we extend the analysis to Swiss national accounts. 

The folder `results/` contains the replication tables, and the script `results/load_res` loads the results. 

The project is joint work with Jonas L. Schmidiger. 

## Requirements

To install the required packages to run the script, you can run the provided `packages.R` file, which will automatically install any missing packages and ensure that the correct versions are installed.

### Installation Instructions

1. Make sure you have R installed on your system.
2. Install the necessary R packages by running the following command in your R console or RStudio:

   ```r
   source("packages.R")

