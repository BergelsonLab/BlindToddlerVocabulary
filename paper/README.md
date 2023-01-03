This folder contains the manuscript, figures, and code for analyses.

In this folder: 
- VI_CDI_citations.bib: references for the manuscript, exported from Zotero
- VI_CDI_figures.R: code for generating the figures from the manuscript. This is sourced by the Rmd.
-  VI_CDI_manuscript_files: a folder containing the figures from the manuscript, updated automatically when the Rmd file is knit
- VI_CDI_manuscript.log: a latex-related file, which is automatically generated when the Rmd is knit
- VI_CDI_manuscript.pdf: a pdf of the manuscript, from latest knitting
- VI_CDI_manuscript.tex: a tex file of the manuscript, which is automatically generated when the Rmd is knit
- VI_CDI_preprocessing.R: code to preprocess data. if pull-new-data is set to TRUE in the Rmd, this is sourced before knitting. Note, need to be connected to pn-opus (secure Duke server) to pull new data.


Last updated: 11/29/2022
Maintained by: Erin Campbell (erin.e.campbell@duke.edu)