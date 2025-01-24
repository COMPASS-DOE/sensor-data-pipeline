# system-files.md

This file describes the files in the sensor data processing system.
It excludes data files, temporary Quarto output files, and folder README files.

The sections below are organized by **folder name**. Within
each is a list of the files in that folder and accompanying descriptions.

## ./pipeline

The root of the system, holding main Quarto files, helper files, and the driver script.

File | Description
---- | -------------
`data/` | Data folder that holds inputs (raw data), outputs, and logs; see below
`data_PREFLIGHT/` | Preflight data folder for testing updated data only; see below
`data_TEST/` | Test data folder that holds inputs (raw data), outputs, and logs; see below
`docs/` | Documentation folder; see below
`driver.R` | A 'driver' script that runs all the Quarto files sequentially and by default in parallel mode, which is fast but generates no logs
`flag-database.R` | Flag database helper script; not used currently
`flmd-generator.R` | File level metadata generation script, to help with ESS-DIVE submission
`helpers.R` | Various ancillary ('helper') functions and test code. This is sourced by all the Quarto files. For interactive use, this contains the `reset()` and `list_directories()` functions
`L0.qmd` | Quarto file for generating L0 data from raw data
`L1_normalize.qmd` | Quarto file for running the 'normalize' L1 step: unit conversion, design table matching, etc. This is the most complicated step in the pipeline
`L1.qmd` | Quarto file for generating L1 data. This is a memory-intensive step
`L2.qmd` | Quarto file for generating L2 data; not used currently
`metadata/` | Metadata folder; see below
`out-of-service.R` | description

## ./pipeline/data

This directory tree holds the data used in a release run. It typically holds
the entire sensor data, making it big and slow to process.

File | Description
---- | -------------
`L0/` | Folder of L0 data written by the `L0.qmd` processing step
`L1/` | Folder of L0 data written by the `L1.qmd` processing step
`L1_normalize/` | Folder of intermediate data written by the `L1_normalize.qmd` processing step
`L2/` | Folder of L2 data written by the `L2.qmd` processing step; not used currently
`Logs/` | Folder of all log files
`Raw/` | Folder of Raw data, copied (occasionally with edits) from the SERC Dropbox
`Raw_done/` | Folder of completely processed raw data; not used currently

## ./pipeline/data_PREFLIGHT

This directory tree is empty by default and intended as a convenience: you can
copy new data into its `Raw/`, and then run the data system to confirm there are
no problems. See `docs/making-a-new-release.md`.

## ./pipeline/data_TEST

This directory tree holds the test data used to test and verify system performance.
These data are the default for the Quarto files and driver script, and are used
by GitHub Actions when pull requests are opened. It is structured
identically to the `./data` directory (see above) but its `Raw/` folder
holds only short sample files from across all sites.

## ./pipeline/docs

Documentation folder.

File | Description
---- | -------------
`making-a-new-release.md` | Step-by-step instructions for creating a new data release
`design_table.csv` | This file

## ./pipeline/metadata

Metadata used by the processing system.

File | Description
---- | -------------
`design_table.csv` | The design table that links datalogger data with experimental subjects, measurement names, and instruments. In many ways, this is the crucial center of the entire L0-to-L1 process
`L1_metadata/` | Folder of L1-specific metadata; see below
`L2_output_templates/` | Folder of L2 template files; not used currently
`newvars_table.csv` | Table describing how to compute new variables in the L2 step; not used currently
`out-of-service` | Folder of data files used in the out-of-service step, part of L1_normalize; see below

## ./pipeline/metadata/L1_metadata

Files used by the `L1_normalize.qmd` and `L1.qmd` steps.

File | Description
---- | -------------
`CRC.txt` | Site description file for the CRC site, giving location, ecological context, contacts, and key publications; used in the `L1.qmd` metadata-generation step
`GCW.txt` | Site description file for the GCW site
`GWI.txt` | Site description file for the GWI site
`L1_metadata_columns.csv` | This specifies the names and ordering of the L1 data columns. Upon being generated, L1 files are checked against this list and the process will error if there's a discrepancy
`L1_metadata_template.txt` | Template for the various metadata files in each site-year folder. Information placeholders in square brackets are replaced in the L1 metadata-generation step
`L1_metadata_variables.csv` | Along with the design table, this is a key 'information center' for the system. Also known as the 'bounds and units table', it specifies unit conversions, expected bands, variable metadata, etc. Every output variable must have an entry here, or the L1_normalize step will error
`MSM.txt` | Site description file for the MSM site
`OWC.txt` | Site description file for the OWC site
`PTR.txt` | Site description file for the PTR site
`README_v???.txt` | Template for the overall README files
`README_v0-9.txt` | Overall README file for the v0-9 release; the release number is set in either `driver.R` or `L1.qmd` and L1 will error without a corresponding README file present here 
`README_v1-0.txt` | Overall README file for the v1-0 release
`README_v1-1.txt` | Overall README file for the v1-1 release
`SWH.txt` | Site description file for the SWH site
`TMP.txt` | Site description file for the TMP site

## ./pipeline/metadata/out-of-service

Files used by `out-of-service.R`.

File | Description
---- | -------------
`exo_log.csv` | This file tracks the "Aquatroll Calibration/Removal Log" spreadsheet on the COMPASS-FME Google Drive and must be updated by hand
`troll_maintenance.csv` | This file tracks the "EXO calibration/deployment log" sheet on the COMPASS-FME Google Drive and must be updated by hand
