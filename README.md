[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](<Redirect-URL>)


workbc_lfs
============================

### Usage

1.  Go to https://eft-tef.statcan.gc.ca/#/ and manually upload the 22 (i'm not kidding) .sas files in folder `SAS_scripts_for_RTRA`. Note that RTRA has a usage limit, so needs to be split over two days.
2. Create a new directory in `data/archived_data` with the name of last year e.g. `2022`.
3.  Move files from directory `data` into sub-directory you created above.
4.  Return to https://eft-tef.statcan.gc.ca/#/ and download the 22 .csv files to folder `data`.
5.  Open file `01_source_me.R` and change variable `last_full_year` to the previous year.
6.  Source file `01_source_me.R`.
7.  Check to make sure dates in output file `xxxx_LFS_data_sheet(xxxx-xxx-xx).xlsx` are correct: You might need to highlight entire sheet and (in libreoffice) `data>calculate>formula to value`. 

### Project Status

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/workbc_lfs/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2022 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 
