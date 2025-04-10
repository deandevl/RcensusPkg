## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.
* URL checked with no errors
* devtools::check_win_release() performed with no fails
* Some functions will require a Census Bureau key
* Some tests and examples may require more than 5 seconds Census Bureau download
   and processing time.

* Patch 0.1.2 reflects a reworded Title and expanded Description paragraph in
    the DESCRIPTION file.

* Patch 0.1.3 has Description field starting with a capital letter.

* Patch 0.1.4 modifies example for tiger_zctas_sf() to '\dontrun' because  
    downloading and processing may extend beyond 5 sec.
    
* Patch 0.1.5 replaces httr with httr2 and adds downloader package for downloading raw json and 
    shape geometry zip files respectively.  A tryCatch surrounds all Internet request code addressing the
    Census Bureau, giving the user an informative message by the Bureau or http errors.
  All url's used in the function descriptions are working from a browser without receiving 
    a 403 status message (Forbidden).
  All tests received OK status.
