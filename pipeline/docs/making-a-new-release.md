# Steps to make a new data release

## Update the raw data

1. Make sure you're up-to-date with the latest version of `main`.

2. Add new raw files from the Dropbox folder(s) into
`./pipeline/data/Raw/Raw_original/`. Right now, this should include files from the
TEMPEST, synoptic, and GCReW met Dropbox shares. Basically, bring your
raw data files up to date. Using the terminal can make this easy:

```
# v1-1 update copy steps for June 2024 files
# Working directory is Dropbox, and $PATH points to sensor-data-pipeline/pipeline
# Note that the folder organization in Raw/ is for user convenience only

cp TEMPEST_PNNL_Data/Loggernet_Rawdata_Archive/*202406* $PATH/data/Raw/Raw_original/Synoptics
cp GCREW_LOGGERNET_DATA/GCREW_MET_GCREW_MET_15min_202406* $PATH/data/Raw/Raw_original/GCREW\ met
cp COMPASS_PNNL_Data/COMPASS_PNNL_Rawdata_Archive/*202406* $PATH/data/Raw/Raw_original/Synoptics
```

If there are lots of files to copy, you can use `grep` and `xargs`:
```
# Copy September-December 2024 met files
ls | grep -E 'GCREW_MET_GCREW_MET_15min_2024(09|10|11|12)' | xargs -I {} cp {} $PATH/data/Raw/Raw_original/GCReW\ Met
```

3. **IMPORTANT NOTE:** Some of the raw data files have bad timestamps
(usually, from when a datalogger is first installed) and have been
edited by hand, and/or occasionally removed entirely. The `Raw_edited/`
folder and `removed_raw_files.txt` file in `./pipeline/data/Raw/` are
for handling these situations; the L0 step uses their contents to
replace or drop, respectively, the files it reads from
`./pipeline/data/Raw/Raw_original/`.

4. You might want to do a test run with _only_ the new data. In that
case, use `./pipline/data_PREFLIGHT`: copy the new files to its
`Raw/Raw_original` folder and run the pipeline by setting the `ROOT`
variable in `driver.R`. If everything looks good, move the raw files
over to the `./pipeline/data/Raw/Raw_original` folder and proceed.
**NOTE** By default, the computationally-intensive L0 step skips a raw
file if a corresponding L0 file already exists; this makes updating with
new data fast.


## Update the metadata

5. Set the release version and date in `driver.R`.

6. Make sure there's a README for your release number in
`./pipeline/metadata/L1_metadata/`. The L1 step will error if a file
named `README_vXXX.txt` doesn't exist there, where "XXX" is the version
number you set in the previous step. Make sure that the citation and
changelog sections of this document are up to date. **NOTE**: if you
copy the previous version's README template as a starting point, replace
the older "[DATESTAMP]" in the changelog with the release date of the
previous version.

7. Update the out-of-service files in
`./pipeline/metadata/out-of-service` (see the README in that folder).

8. For major (v2-0, etc) releases, check with site PIs to see if the
site-specific contact information is correct and, if needed, update the
various site files in `./pipeline/metadata/L1_metadata`. Also update the
key publications for each site.

9. Confirm that the datalogger time zone information listed in
`./metadata/L1_metadata/L1_metadata_timezones.csv` is correct, and that
the output time zone (set in `driver.R` or the individual Quarto files)
is what you want.

## Run the pipeline

10. Commit all your changes. Now set the `ROOT` variable in `driver.R`
(if that's what you're using) to "./data" instead of "./data_TEST".
**This change does NOT get committed**, however, because you want GitHub
Actions to continue to use the _test_ data.

11. From the `./pipeline` folder, run `reset("data/")` (sourced from 
`helpers.R`) to remove any previous files.

12. Run the processing pipeline. If you use `driver.R` it will be
relatively fast, because highly parallelized, but you don't get
informative html logs, because the parallel processes can't write to
output. If you want the full, detailed logs, run without parallelism
(either by rendering the Quarto files one by one, or changing the driver
script). Of course, this is much slower. 
If errors happen, they almost always will occur in the `L1_normalize` 
step, as this has multiple complex data merges and checks. 
See https://github.com/COMPASS-DOE/sensor-data-pipeline/issues/232
for examples of how to resolve these.


## Check, clean up, upload

13. Double-check the final release README file.

14. You may want to clean up the resulting L1 folder; for example,
remove unwanted hidden files (`find ./ -name ".DS_Store" | xargs rm`) or
'stub' data (`find ./ -name "*202407*" | xargs rm`). (Before doing this,
use find's `-print` option to make sure you know what you're deleting!)

15. Push the data (including `L1`, `Raw`, `L0`, and `Logs`) to the
COMPASS HPC. For example:

```
# this example is for for L1 v1-2; use similar calls this for Raw, L0, and Logs
rsync -av --chown=:compass-fme-data --perms --chmod=g+rx --exclude=".*" L1/ <user>@compass.pnl.gov:/compass/datasets/fme_data_release/sensor_data/Level1/v1-2/

# Important note: we use the -chown option to ensure that the correct
# group is associated with the uploaded files, but by default MacOS
# comes with an old (2.6.9, 2006) rsync that doesn't support this. You
# can either do the 'chown' step subsequently on the COMPASS HPC, or use
# Homebrew (https://brew.sh) to install an up-to-date version of rsync.
```

16. Upload to the Google Drive, renaming the folder to the correct
version number, for `L1` and `Raw`.

17. Make a Git release corresponding to the version number.

18. Let everyone know!
