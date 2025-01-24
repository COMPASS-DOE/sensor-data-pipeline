# Steps to make a new data release

## Update the raw data

1. Make sure you're up-to-date with the latest version of `main`.

2. Add new raw files from the Dropbox folder(s) into
`./pipeline/data/Raw/`. Right now, this should include files from the
TEMPEST, synoptic, and GCReW met Dropbox shares. Basically, bring your
raw data files up to date. Using the terminal can make this easy:

```
# v1-1 update copy steps for June 2024 files
# Working directory is Dropbox, and $PATH points to sensor-data-pipeline/pipeline
# Note that the folder organization in Raw/ is for user convenience only

cp TEMPEST_PNNL_Data/Loggernet_Rawdata_Archive/*202406* $PATH/data/Raw/Synoptics
cp GCREW_LOGGERNET_DATA/GCREW_MET_GCREW_MET_15min_202406* $PATH/data/Raw/GCREW\ met
cp COMPASS_PNNL_Data/COMPASS_PNNL_Rawdata_Archive/*202406* $PATH/data/Raw/Synoptics
```

If there are have lots of files to copy, you can use a regular expression and `xargs`:
```
# Copy September-December 2024 met files
ls | grep -E 'GCREW_MET_GCREW_MET_15min_2024(09|10|11|12)' | xargs -I {} cp {} $PATH/data/Raw/GCReW\ Met
```

3. **IMPORTANT NOTE:** Some of the raw data files have bad timestamps
(usually, from when a datalogger is first installed) and I have edited
those out by hand. So you do NOT want to start with the entire Dropbox
archive folder, but rather from the raw files of the previous release.
These are archived with each release. If you don't have these
raw-but-edited data used in the last release, get it from the HPC:
```
# Working directory is ./pipeline/data/Raw
rsync -av <user>@compass.pnl.gov:/compass/datasets/fme_data_release/sensor_data/Raw/v1-1/ .
```

4. You might want to do a test run with _only_ the new data. In that
case, use the `./pipline/data_PREFLIGHT`: copy the new files to its
`Raw/` folder and run the pipeline by setting the `ROOT` variable in
`driver.R`. If everything looks good, move the raw files over to the
main `data/Raw/` folder and proceed.


## Update the metadata

5. Set the release version in `driver.R`.

6. Make sure there's a README for your release number in
`./pipeline/metadata/L1_metadata/`. The L1 step will error if a file
named `README_vXXX.txt` doesn't exist there, where "XXX" is the version
number you set in step 4. Make sure that the citation and changelog
sections of this document are up to date.

7. Update the out-of-service files in
`./pipeline/metadata/out-of-service` (see the README in that folder).

8. For major (v2-0, etc) releases, check with site PIs to see if the
site-specific contact information is correct and, if needed, update the
various site files in `./pipeline/metadata/L1_metadata`.


## Run the pipeline

9. Commit all your changes. Now set the `ROOT` variable in `driver.R`
(if that's what you're using) to "./data" instead of "./data_TEST".
**This change does NOT get committed**, however, because you want GitHub
Actions to continue to use the _test_ data.

10. From the `./pipeline` folder, run `reset("data/")` (this function
should be sourced from `helpers.R`). This will clean out any previous
files.

11. Run the processing pipeline. If you use `driver.R` it will be
relatively fast, because highly parallelized, but you don't get
informative html logs, because the parallel processes can't write to
output. If you want the full, detailed logs, run without parallelism
(either by rendering the Quarto files one by one, or changing the driver
script). Of course, this is much slower.


## Check, clean up, upload

12. Double-check the final release README file.

13. You may want to clean up the resulting L1 folder; for example,
remove unwanted hidden files (`find ./ -name ".DS_Store" | xargs rm`) or
'stub' data (`find ./ -name "*202407*" | xargs rm`). (Before doing this,
use find's `-print` option to make sure you know what you're deleting!)

14. Push the data (including `L1`, `Raw`, `L0`, and `Logs`) to the
COMPASS HPC. For example:

```
rsync -av --exclude=".*" L1/ <user>@compass.pnl.gov:/compass/datasets/fme_data_release/sensor_data/Level1/v1-0/
```

15. Upload to the Google Drive, renaming the folder to the correct
version number, again for `L1`, `Raw`, `L0`, and `Logs`.

16. Make a Git release corresponding to the version number.

17. Let everyone know!
