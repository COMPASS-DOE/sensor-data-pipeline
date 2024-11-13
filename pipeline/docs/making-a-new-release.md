# Steps to make a new data release

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

3. IMPORTANT NOTE: some of the raw data files have bad timestamps
(usually, from when a datalogger is first installed) and I have edited
those out by hand. So you do NOT want to start with the entire Dropbox
archive folder, but rather from the raw files of the previous release.
These are archived with each release.

4. Set the release version in `driver.R`.

5. Make sure there's a README for your release number in
`./pipeline/metadata/L1_metadata/`. The L1 step will error if a file
named `README_vXXX.txt` doesn't exist there, where "XXX" is the version
number you set in step 4. Make sure that the citation and changelog
sections of this document are up to date.

6. Update the out-of-service files in
`./pipeline/metadata/out-of-service` (see the README in that folder).

7. Occasionally, check with site PIs to see if the site-specific contact
information should be updated, and if so, update the various site files
in `./pipeline/metadata/L1_metadata`.

8. Commit all your changes. Now set the `ROOT` variable in `driver.R`
(if that's what you're using) to "./data" instead of "./data_TEST". This
change does NOT get committed, however, because you want GitHub Actions
to continue to use the _test_ data.

9. From the `./pipeline` folder, run `reset("data/")` (this function
should be sourced from `helpers.R`). This will clean out any previous
files.

10. Run the processing pipeline. If you use `driver.R` it will be
relatively fast, because highly parallelized, but you don't get
informative html logs, because the parallel processes can't write to
output. If you want the full, detailed logs, run without parallelism
(either by rendering the Quarto files one by one, or changing the driver
script). Of course, this is much slower.

11. Double-check the final release README file.

12. You may want to clean up the resulting L1 folder; for example,
remove unwanted hidden files (`find ./ -name ".DS_Store" | xargs rm`) or
'stub' data (`find ./ -name "*202407*" | xargs rm`). (Before doing this,
use find's `-print` option to make sure you know what you're deleting!)

13. Push the data (including `L1`, `Raw`, `L0`, and `Logs`) to the
COMPASS HPC. For example:

```
rsync -av --exclude=".*" L1/ <user>@compass.pnl.gov:/compass/datasets/fme_data_release/sensor_data/Level1/v1-0/
```

14. Upload to the Google Drive, renaming the folder to the correct
version number, again for `L1`, `Raw`, `L0`, and `Logs`.

15. Make a Git release corresponding to the version number.

16. Let everyone know!
