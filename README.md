# trinityghg

Evaluating the ability of process-based biogeochemical models to predict Ireland's agricultural soil greenhouse gas emissions, and scaling them to the national level.

## Introduction
This repository contains R scripts providing a variety of functions for interacting with biogeochemical models, specifically:
  - Reading model output,
  - Processing model output, and
  - Evaluating model output using observed data.
  
The models that are used in this project are DailyDayCent, DNDC 9.4, DNDC 9.5, and ECOSSE.

## Scripts and functions

### Reading model output
 - readDayCent.r
 - readDNDC.r (reads output from DNDC 9.4 and DNDC 9.5)
 - readecosse.r (currently only reads SUMMARY.OUT file)
 
### Processing model output
 - cumget.r: get.cumulative() function. Generates cumulative fluxes from observations and simulated data over a specified period of interest.
 - extract.dndc.r: extract.dndc() function. Extracts output for a specified variable from a DNDC object generated using readDNDC and binds simulated output to a time series
 - simobmerge.r: simobmerge() function. Creates time series template and merges to observations.
 
### Evaluating model output
 - fitplot.r: fitplot() function. Plots observations and simulated data on an x-y plot and a time series plot.
 - modeval.r: modeval() function. An R port of Jo and Pete Smith's MODEVAL2000 spreadsheet tool. Generates a range of statistics used to evaluate different aspects of model performance against observed data.