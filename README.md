
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Estimating the uncertainty of the greenhouse gas emission accounts in Global Multi-Regional Input-Output analysis

This is the code for reproducing the results for our study on
“Estimating the uncertainty of the greenhouse gas emission accounts in
Global Multi-Regional Input-Output analysis” submitted to the Journal of
Earth Systems Science Data (ESSD).

Once published the study will be linked here.

## Data required

To reproduce the results and run the scripts you need to download the
following data:

- EXIOBASE V3.8.2 `IOT_2015_ixi.zip` and `MRSUT_2015.zip` from here:
  <https://zenodo.org/records/5589597>
- UNFCCC country-submitted greenhouse gas emissions data until
  2020-10-25 from here: <https://zenodo.org/record/4199622>
- EDGAR v5.0 time series data for CO$_2$, CH$_4$ and N$_2$O from here:
  <https://edgar.jrc.ec.europa.eu/dataset_ghg50>
- UNFCCC uncertainty data from here:
  <https://zenodo.org/record/10037714>
- Correspondence table between UNFCCC CRF and EXIOBASE industry sectors
  from here: <https://zenodo.org/record/10046372>
- Supplementary material 2 from Selin et al. (2021):
  <https://doi.org/10.1088/1748-9326/abec02> (The link to the file is a
  bit hidden below the abstract)

The uncertainty data from Solazzo et al. (2021)
(<https://acp.copernicus.org/articles/21/5655/2021/>) is already part of
this repository
(`./data/tot_GHG_2015_Tier1_sector_countrytotal_cat_.csv`).

## How to run the scripts

1.  [Clone the repository to create a local copy on you
    computer](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository).

2.  Open the project by clicking on `uncertainty_GHG_accounts.Rproj`.

3.  [renv](https://cran.r-project.org/web/packages/renv/vignettes/renv.html)
    will automatically bootstrap itself, downloading and installing the
    appropriate version of renv. It will also ask you if you want to
    download and install all the packages it needs by running
    `renv::restore()`.

4.  Once the packages are installed, open the file `__main__.qmd`.

5.  Run the code junks **one by one**.

The scripts need to be run according to the order indicated (which the
`__main__.qmd` does). The dependency graph of the individual scripts is
depicted here:

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

The code was tested with the following setup:

    platform       x86_64-pc-linux-gnu         
    arch           x86_64                      
    os             linux-gnu                   
    system         x86_64, linux-gnu           
    status                                     
    major          4                           
    minor          3.1                         
    year           2023                        
    month          06                          
    day            16                          
    svn rev        84548                       
    language       R                           
    version.string R version 4.3.1 (2023-06-16)
    nickname       Beagle Scouts 

In case of problems please raise an [issue](/issues) or [contact
me](mailto:simon.schulte@indecol.uni-freiburg.de).
