## Project overview
[![DOI](https://zenodo.org/badge/772546883.svg)](https://zenodo.org/doi/10.5281/zenodo.12655000) 

The Systems Science in Public Health and Health Economics Research (SIPHER) Consortium brings together a range of academic and policy partners to focus on developing novel approaches to support improving health and reducing health inequalities. As part of the SIPHER Consortium, a synthetic population was developed, a digital twin, for the adult (16+) population in Great Britain. An interactive dashboard has been developed to support exploration of an aggregated version of the SIPHER Synthetic Population 

This repository contains all code to enable the reproduction of the aggregated dataset as well as the dashboard. Please note that Understanding Society survey data is required to reproduce the aggregated dataset. While we cannot share these files, they can be obtained from the UK Data Service [SN6614].  

## File Structure 
The file structure follows the `golem` framework. Folders of note include:

`/R` - scripts used to build and run the dashboard

`/dev` - scripts used to prepare the data for the dashboard

`/data` - prepared data files used in the dashboard

## Running the Code  

To run the dashboard locally, use `remotes::install.github("Dylan-W-Lewis/SIPHERdashboard")`, followed by `SIPHERdashboard::run_app()` 

## Reporting Issues 

If you encounter any issues with the materials contained in this repository, please direct enquiries marked ‘SIPHER Synthetic Population – Code’ to sipher@glasgow.ac.uk 

## Additional Links  

The live version of the SIPHER Synthetic Population Dashboard is available here: https://sipherdashboard.sphsu.gla.ac.uk/  

About the SIPHER Consortium: https://sipher.ac.uk/ 

About the Synthetic Population dataset: https://www.gla.ac.uk/research/az/sipher/products/syntheticpopulation/

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
