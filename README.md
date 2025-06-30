Last edited:  January 2025

# Clarifying the trophic state concept to advance macroscale freshwater science and management 

Authors: Michael F Meyer

Point of Contact: Michael Meyer (mfmeyer@usgs.gov)

Repository Type: R scripts supporting publication

Year of Origin: 2025 (Original Publication)

Year of Version: 2025

Digital Object Identifier (DOI): doi:10.5066%2FP14KNKHD

USGS Information Product Data System (IPDS) no.: IP-180238 (internal agency tracking)

## Respository Contents

This repository is for the "Refining Lake Trophic Status Concept"
Project. Michael F Meyer (mfmeyer@usgs.gov) is the main owner of the
group, but owner permissions may change through time.

The repository is meant to contain all scripts, data, and figures
relating to the project. MFM structured the project with a few
components that he hopes will allow for members to be able to work most
efficiently, but he recognizes that this may not be realized. If you
have suggestions for improvements, please feel free to send them over
email to Michael (mfmeyer@usgs.gov).

Directory Architecture: MFM designed the main directories to be
something like this:
```
~/refining_lts
├───data
│   ├───derived_products
│   └───nla_all_years
├───figures
└───scripts
```

Ideally all scripts should be read in from the `nla_all_years`
directory, and then we can output any derived data products to the
`derived_products` directory. All figures can be output to the `figures`
directory.

As this project is more limited with respect to coding and analyses, you
are welcome to either fork or branch the repository or create a script
with your name/initials in the file name, and then commit that script to
the scripts folder.

To be aware of what others are working on, please make a Git Issue with
a short write-up of your figure ideas.

## Repository Organization

- `scripts/check_strat.R`: This script iterates through all thermal profiles from the 2017 National Lakes Assessment (NLA) and assesses whether that lake is likely stratified or mixed. This script should be run before `depth_prfile_plots_condesed.R`.
    - `Inputs`: Depth profile data from the 2017 NLA. 
    - `Outputs`: A CSV of whether a given lake was stratified or mixed. 
- `scripts/depth_prfile_plots_condesed.R`: This script creates a figure of thermal and oxygen depth profiles for 
lakes in the NLA, where lakes are grouped by the trophic state. The average Trophic State Index and Nutrient Color 
Paradigm groups are created. 
    - `Inputs`: Depth profiles form the 2017 NLA; outputs from `strat_check.R`; formatted NLA data with water quality concentrations. 
    - `Outputs`: Two pngs of figures generated from the analysis. These figures are associated with Figure 1 in the manuscript. 
- `scripts/secchi_depth_boxplots.R`: This script aggregates data from the 2007, 2012, and 2017 NLA campaigns, generates trophic state classifications, and then plots the Secchi Disk Depths for each grouping. 
    - `Inputs`: Water quality data for the 2007, 2012, and 2017 NLA. 
    - `Outputs`: A png of secchi disk depths across several TS classification schemes. 
- `scripts/lts_refinement_map.R`: This script creates summary statistics and maps of where certain trophic states are located throughout the contiguous United States. 
    - `Inputs`: Water quality data for the 2007, 2012, and 2017 NLA. 
    - `Outputs`: A png of spatial distributions and summary statistics across several TS classification schemes. 
- `scripts/rs_ts_differences.R`: This script creates boxplots and violin plots of the dominant wavelength across two different trophic state classification systems. 
    - `Inputs`: Water quality data for the 2007, 2012, and 2017 NLA; LimnoSat surface reflectance data. 
    - `Ouputs`: A png of dominant wavelength differences across two different trophic state classification schemes. 
- `Data`: All input data and derived outputs with a tabular structure should be contained here. 
- `Figures`: All figures should be output here. 
- `.gitignore`: A file specifying which files should not be tracked in the git repository. 

## Setup and Package Installation

The scripts contained herein were run using R version 4.4.2.

Necessary Packages to run this codebase include:
- [tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html)
- [scales](https://cran.r-project.org/web/packages/scales/index.html)
- [sf](https://cran.r-project.org/web/packages/sf/index.html)
- [cowplot](https://cran.r-project.org/web/packages/cowplot/index.html)
- [rLakeAnalyzer](https://cran.r-project.org/web/packages/rLakeAnalyzer/index.html)
- [ggalluvial](https://cran.r-project.org/web/packages/ggalluvial/index.html)
- [ggpubr](https://cran.r-project.org/web/packages/ggpubr/index.html)
- [ggtext](https://cran.r-project.org/web/packages/ggtext/index.html)

## Running the code

This code has been organized in piecemeal scripts. They can be run in any order, with the exception of 
`check_strat.R`. This script must be run first, as detailed above. 

## Code run time

All scripts can be run within ~15 minutes on high speed laptop. 

## Expertise required

This code assumes the end user is familiar with both R and limnological expertise. However, much of the domain expertise can be gleaned from the main associated manuscript. 

## License

This project is licensed under the Creative Commons CC0 1.0 Universal License - see the LICENSE.md file for details.

## Suggested Citation

In the spirit of open source, please cite any re-use of the source code stored in this repository. Below is the suggested citation:

Meyer, MF. 2025. Clarifying the trophic state concept to advance macroscale freshwater science and management. U.S. Geological Survey software release. Reston, VA. https://doi.org/10.5066/P14KNKHD

# Disclaimer
This software is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science. The
software has not received final approval by the U.S. Geological Survey
(USGS). No warranty, expressed or implied, is made by the USGS or the
U.S. Government as to the functionality of the software and related
material nor shall the fact of release constitute any such warranty. The
software is provided on the condition that neither the USGS nor the U.S.
Government shall be held liable for any damages resulting from the
authorized or unauthorized use of the software.
