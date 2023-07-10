[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](<Redirect-URL>)


workbc_lfs
============================

### Usage

1.  Go to https://www75.statcan.gc.ca/eft-tef/en/login-connexion and manually upload the 20 (i'm not kidding) .sas files in folder `SAS_scripts_for_RTRA`.
2.  Go to bed and wake up next morning.
3.  Clear your inbox of 120!!! email messages from Stats Can.
4.  Empty the folder `data`.
5.  Return to https://www75.statcan.gc.ca/eft-tef/en/login-connexion and manually download the 20 .csv files to folder `data`.
6.  Open file `01_source_me.R` and change variable `last_full_year` to the year you want.
7.  Source file `01_source_me.R`.
8.  Open the file `year_LFS_data_sheet.xlsx` highlight entire sheet and (in libreoffice) `data>calculate>formula to value`.  Otherwise the dates in the header will change with the year.

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
