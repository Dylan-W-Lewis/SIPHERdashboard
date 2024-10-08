## Project overview
![GitHub Release](https://img.shields.io/github/v/release/Dylan-W-Lewis/SIPHERdashboard)
[![DOI][doi-shield]][doi]
[![CC BY 4.0][cc-by-shield]][cc-by]

The Systems Science in Public Health and Health Economics Research (SIPHER) Consortium brings together a range of academic and policy partners to focus on developing novel approaches to support improving health and reducing health inequalities. As part of the SIPHER Consortium, a synthetic population was developed, a digital twin, for the adult (16+) population in Great Britain. An interactive dashboard has been developed to support exploration of an aggregated version of the SIPHER Synthetic Population 

This repository contains all code to enable the reproduction of the aggregated dataset as well as the dashboard. Please note that Understanding Society survey data is required to reproduce the aggregated dataset. While we cannot share these files, they can be obtained from the UK Data Service [SN6614].  

## File Structure 
The file structure follows the `golem` framework. Folders of note include:

`/R` - scripts used to build and run the dashboard

`/dev` - scripts used to prepare the data for the dashboard

`/data` - prepared data files used in the dashboard

## Download from GitHub and Running the App 

To run the dashboard locally, open a new R-studio session and run the following commands in order:

* `install.packages("remotes")`: Install the package required for downloading files from GitHub
* `library(remotes)`: Load the remotes package we have previously downloaded
* `remotes::install_github("Dylan-W-Lewis/SIPHERdashboard")`: Download the SIPHERdashboard package from GitHub
* `library(SIPHERdashboard)`: Load the SIPHERdashboard package we have just downloaded
* `SIPHERdashboard::run_app()`: Start the dashboard app

## Reporting Issues 

If you encounter any issues with the materials contained in this repository, please direct enquiries marked ‘SIPHER Synthetic Population – Code’ to sipher@glasgow.ac.uk 

## Additional Links  

The live version of the SIPHER Synthetic Population Dashboard is available here: https://sipherdashboard.sphsu.gla.ac.uk/  

About the SIPHER Consortium: https://sipher.ac.uk/ 

About the Synthetic Population dataset: https://www.gla.ac.uk/research/az/sipher/products/syntheticpopulation/

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[doi-shield]: https://zenodo.org/badge/772546883.svg
[doi]: https://zenodo.org/doi/10.5281/zenodo.12655000
[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
