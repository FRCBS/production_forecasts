# production_forecasts
This repository contains R scripts and R Markdown scripts used to generate results in the paper: "Robust autonomous method for blood demand forecasting" by Esa V. Turkulainen, Merel L. Wemelsfelder, Mart P. Janssen, and Mikko Arvas. Submitted for review: August 2021.

## Folders

**result_scripts**
Contains all scripts and functions required for creating the results in our paper, for transparency purposes. Unfortunately, no data is made public due to its confidential nature, so there is no obvious way to ensure reproducibility. The folder also contains the scripts "bug_demo.R" and "datadiff_check.R", which explore two particularly hairy obstacles encountered during development. These are included for transparency and documentation purposes.

**implementation**
Contains an R Markdown file that knits to HTML, and its accompanying function script. The script implements our method selection system as a forecast report. Will likely be updated with an example report output.
