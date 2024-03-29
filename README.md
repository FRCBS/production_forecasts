# production_forecasts
This repository contains R scripts and R Markdown scripts used to generate results in the paper: ["Robust autonomous method for blood demand forecasting"]( https://doi.org/10.1111/trf.16870) by Esa V. Turkulainen, Merel L. Wemelsfelder, Mart P. Janssen, and Mikko Arvas. Transfusion 2022.

## Folders

**result_scripts**
Contains all scripts and functions required for creating the results in our paper, for transparency purposes. Unfortunately, no data is made public due to its confidential nature, so there is no obvious way to ensure reproducibility. The folder also contains the scripts "bug_demo.R" and "datadiff_check.R", which explore two particularly hairy obstacles encountered during development. These are included for transparency and documentation purposes.

**implementation**
Contains an R Markdown file that knits to HTML, and its accompanying function script. The script implements our method selection system as a forecast report. The report is the same we currently use at the FRCBS. The data processing needs to be amended to your sources, but otherwise it is an out-of-the-box solution for your forecasting needs.
