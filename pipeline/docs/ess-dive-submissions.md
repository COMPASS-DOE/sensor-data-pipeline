---
editor_options: 
  markdown: 
    wrap: 72
---

# Steps to update ESS-DIVE submissions

0.  Run processing pipeline to produce L1 data for synoptic and TEMPEST
    sites

1.  Move L1 data to folders labeled `vX-X` \> `vX-X_SYN` and `vX-X` \>
    `vX-X_TMP` . Each folder gets a copy of the `READMD.txt` and
    `README_vX-X.txt` files.

2.  Prep package metadata files

-   Produce flmd using `flmd-generator.R`
-   Copy over `dd.csv` from previous version (this file does not change
    unless we change or add a new L1 column)
-   Run `availability_graph.R` to produce availability graphs.
    -   Add saved graphs to quick start documents located in the FME
        Data Management folder
    -   Export Google document as a PDF and move to submission folders

3.  Prep data

-   Compress all folders into \_.zip\_ format

4.  ESS-DIVE submission

-   If a minor version change:

    -   Remove all data files

    -   Add new files

    -   Update data date range in submission metadata

    -   Change version name in title and abstract

    -   Finally, email ESS-DIVE support to start the re-review

-   If a major version change:

    -   Create new submission

    -   *More steps to come*...
