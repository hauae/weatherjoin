## Test environments

- Windows 11, R 4.5.1
- win-builder: R-devel (via devtools::check_win_devel())
- GitHub Actions (windows-latest, ubuntu-latest, macos-latest)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Comments

Version 0.2.2 is a patch update following the recent acceptance of version 0.2.0.

The previous CRAN checks reported a NOTE due to files being created in a user-level cache directory during R CMD check.
This has been resolved by redirecting all package caching to a temporary directory when running under R CMD check, preventing creation of files outside the check directory.

No user-facing functionality has changed.

The package provides tools for attaching gridded weather variables from the NASA POWER Project to event-based datasets. Data are retrieved via the 'nasapower' package and cached locally for efficient repeated use.

The package is not affiliated with or endorsed by NASA.

