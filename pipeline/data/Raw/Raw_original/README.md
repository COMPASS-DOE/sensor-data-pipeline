# Raw

This folder holds raw data as downloaded from dataloggers.

Note that the "Compass_PTR_Buoy_ExoTable_20220824143832.dat" file will
generate an error but in L1_normalize, but since it's listed in
`removed_raw_files.txt`, the `L0.qmd` script removes it from the
processing pipeline before this happens.
