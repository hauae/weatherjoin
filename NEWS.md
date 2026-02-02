# weatherjoin 0.2.2

## Bug fixes

- Prevent creation of persistent user cache files during `R CMD check` by redirecting all package caching to a temporary directory when running under check conditions.

# weatherjoin 0.2.1

- internal/not released

# weatherjoin 0.2.0

## New features

- Initial CRAN release.
- Tools for joining gridded weather variables from the NASA POWER Project to event-based datasets.
- Local caching of retrieved weather data to improve performance for repeated use.

## Notes

- The package is not affiliated with or endorsed by NASA.
