`GLPF`
=============

We may add a `make` or `remake` configuration in the future, but currently, the process from raw data to final visualizations is done in the following order:

All files are relative to the "scripts" folder:

1. `0_download`:
  "getGoogleSampleTrackingDownload.R" grabs tracking form data from google doc. Saved data to raw_data/tracking
  
2. `1_munge`
  * "openMapData.R" grabs raw data from "raw" folder, puts it into "cache" folder. Some bigger shapefiles are only on the "Projects" directory
  * "openTrackingData.R" grabs data from "raw_data/tracking" and "raw_data/optics" saves merged bacteria results with DOC and TDN
  
3. `2_process`
  * "mergeFieldOpt.R" grabs
  * "mergeTrackingBacteriaData.R" grabs raw data from "raw_data/tracking" and "raw_data/optics" folders, merges bacteria results with DOC and TDN
  * "mergeGLPF_GLRI" merges GLPF and GLRI abs and fl data.

##Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)

