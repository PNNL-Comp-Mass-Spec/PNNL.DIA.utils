
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pnnl.diann.utils

<!-- badges: start -->
<!-- badges: end -->

The goal of pnnl.diann.utils is

1.  A set of functions to analyze DIA-NN outputs and convert them to
    `MSnSet` objects for downstream analysis with
    [`MSnSet.utils`](https://github.com/PNNL-Comp-Mass-Spec/MSnSet.utils).
2.  Assembling paths for analyzing files directly from DMS.
3.  (in-progress) General workflow for *plexDIA* analysis.

<div class="figure">

<img src="images/pnnl.diann.utils_workflow_DARK.png" alt="Overview of `pnnl.diann.utils` package workflow. Package functions are in `monospace`." width="100%" />
<p class="caption">
Overview of `pnnl.diann.utils` package workflow. Package functions are
in `monospace`.
</p>

</div>

## Installation

You can install the development version of pnnl.diann.utils from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PNNL-Comp-Mass-Spec/DIANN.utils")
```

## Example datasets

Example gene group (*gg*) and protein group (*pg*) matrices, and
precursors (*pq*) report are found in build`inst/extdata/`.

- `diann_report.tsv` is example data from
  [`diann_rpackage`](https://github.com/vdemichev/diann-rpackage)
- `QC_Mam` matrices are from
  `\\proto-8\Lumos02\2023_4\QC_Mam_19_01_b_DIA_trap_60min_19Dec23_Pippin_WBEH-23-05-18\DNN202312200344_Auto2255735`

## Converting Protein Groups Matrix to `MSnSet`

``` r
library(pnnl.diann.utils)


pg_matrix_path <- system.file("extdata",
                              "QC_Mam_19_01_b_DIA_report.pg_matrix.tsv",
                              package = "pnnl.diann.utils")


pnnl.diann.utils::pgmatrix_to_msnset(pg_matrix_path)
#> MSnSet (storageMode: lockedEnvironment)
#> assayData: 5550 features, 1 samples 
#>   element names: exprs 
#> protocolData: none
#> phenoData
#>   sampleNames:
#>     QC_Mam_19_01_b_DIA_trap_60min_19Dec23_Pippin_WBEH-23-05-18
#>   varLabels: dataset
#>   varMetadata: labelDescription
#> featureData
#>   featureNames: A0A075B5S1;A0A075B5S9;A0A0B4J1J4;A0A0B4J1J5
#>     A0A087WNP0;A0A3B2W7J0;E9PX25;E9PZY4;J3QP73;K9J7E2;L7N2C4 ... S4R1W5
#>     (5550 total)
#>   fvarLabels: Protein.Ids Protein.Names Genes First.Protein.Description
#>   fvarMetadata: labelDescription
#> experimentData: use 'experimentData(object)'
#> Annotation:  
#> - - - Processing information - - -
#>  MSnbase version: 2.26.0
```

## Assembling file paths from Data Package number

While running DIA-NN from `kaiju` or other PNNL servers, DIA-NN can pull
files directly from DMS, saving time and redundancy incurred from
copying LC-MS files over with a minimal impact on overall DIA-NN
analysis time. To this end an absolute file path needs to be provided to
DIA-NN for each LC-MS file. `dms_dataset_path` assembles the paths for
all files in a [DMS Data
Package](https://prismwiki.pnl.gov/wiki/Data_Package). The function
requires
[`PNNL>DMS.utils`](https://github.com/PNNL-Comp-Mass-Spec/PNNL.DMS.utils)
and access to PNNL network.

``` r
paths <-pnnl.diann.utils::dms_dataset_path(
  data_package_num = 5293,
  file_type = ".raw",
  diann_console = TRUE
  )


paths[1:3,]
#> [1] "--f \"\\\\proto-9\\Eclipse02\\2023_4\\KidsFirst_T-ALL_PATEAK_Prot_061_FAIMS_Merry_DI_16Oct23_WBEH-23-07-14\\KidsFirst_T-ALL_PATEAK_Prot_061_FAIMS_Merry_DI_16Oct23_WBEH-23-07-14.raw\""
#> [2] "--f \"\\\\proto-9\\Eclipse02\\2023_4\\KidsFirst_T-ALL_PATGMS_Prot_063_FAIMS_Merry_DI_16Oct23_WBEH-23-07-14\\KidsFirst_T-ALL_PATGMS_Prot_063_FAIMS_Merry_DI_16Oct23_WBEH-23-07-14.raw\""
#> [3] "--f \"\\\\proto-9\\Eclipse02\\2023_4\\KidsFirst_T-ALL_PARLST_Prot_002_FAIMS_Merry_DI_16Oct23_WBEH-23-07-14\\KidsFirst_T-ALL_PARLST_Prot_002_FAIMS_Merry_DI_16Oct23_WBEH-23-07-14.raw\""
```

Note the outputs above are already formatted for the [DIA-NN Console
Command](https://github.com/vdemichev/DiaNN?tab=readme-ov-file#command-line-reference).
They can then be saved (see below) and pasted into the *Console
Commands* box on DIA-NN. Setting `diann_console = FALSE` will return the
absolute LC-MS file paths only.

``` r

write_tsv(x = diann_raw_paths,
          escape = "none",
          col_names = FALSE, 
          file = "DATA/PATH.txt")
```
