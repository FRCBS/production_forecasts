# production_forecasts
This repository contains R notebooks modelling blood production time series for forecasting purposes. The aim is to study the previously created models semi-scientifically and to find out future avenues for improving the forecasting results.

## Notebooks
Notebooks starting with "DEP" are deprecated, which means that the reader is mostly just wasting their time due to erroneus methods or values or simply due to redundant information. These are preserved for transparency and documentation purposes.

**DEP_first_analyses.(R)md**
This R notebook was the first approach to examining FRCBS monthly product sales data and old forecasting models. The aim was to understand the underlying characteristics of the series, and to have a quick, mainly visual, look at the performance of the forecasts in use already.

**DEP_adjusted_first_analyses.(R)md**
This notebook is almost exactly the same as DEP_first_analyses, but uses an inadequate month adjustment on the series.

**DEP_platelet_analyses.(R)md**
This notebook follows the general structure of DEP_adjusted_first_analyses, but only focuses on *PLATELETS*. This notebook contains couple of additions, "deeper" analyses and some improvement to the methods and code. This is however also deprecated, as the methods are still largely incorrect and/or insufficient. For example, the month adjustment is now better, but still not ideal.

**DEP_plasma_analyses.(R)md**
See above. For *PLASMA*.

**DEP_red_analyses.(R)md**
See above. For *RED CELLS*. In addition, first naive benchmarks are added and first experiments with model saving and filters are conducted. This is also severely deprecated.

**red_benchmarks.(R)md**
Minimalistic (less visuals) benchmarking pipeline for finding the best models with cMAPE, MAPE and RMSE.
Best model for red cell series: adjusted STLF

**pla_benchmarks.(R)md**
Minimalistic (less visuals) benchmarking pipeline for finding the best models with cMAPE, MAPE and RMSE.
Best model for platelet series: adjusted dynamic regression or unadjusted STL

**ffp_benchmarks.(R)md**
Minimalistic (less visuals) benchmarking pipeline for finding the best models with cMAPE, MAPE and RMSE.
Best model for plasma series: adjusted dynamic regression (by a large margin!)

**ketju_usage_lab.(R)md**
Currently retired (but might see intermittent development). In this notebook we study how to use hospital blood product usage data to create demand predictions.

**delivery_lab.(R)md**
Currently under development. In this notebook we try weekly and monthly forecasting with delivery data and see how it compares with earlier production forecasts.

## Functions

**evalhelp.R**
This file contains helper functions for running tsCV(). The custom metric of critical MAPE (cMAPE) is also defined here.
