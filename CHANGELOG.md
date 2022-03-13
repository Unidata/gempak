<!-- markdownlint-configure-file {"MD024": { "siblings_only": true } } -->
# Unidata Community GEMPAK Change Log

## 7.14.0.1 (13 March 2022)

This release signifies the desire for the community to make releases at a faster
cadence than verbatim tracking NCEP NAWIPS releases (#74).  We will continue
to attempt to merge any upstream NCEP NAWIPS releases, but will attempt to add
additional functionality here.

### Bug Fixes

- The install4j Java vulnerability is "addressed" by simply removing the offending
jar file content, which is generally unused by the Unidata Community (#69).
- The release of `7.14.0` has a number of issues with included GEMPAK tables
with Unidata modifications over-written by the NCEP NAWIPS releases (#70). There
may still need to be some work done in this area, but things are much closer now
to back to normal.
- @sgdecker kindly provided fixes for a couple of edge cases (#44, #46).

### Improvements

- Developer experience improvements were made with some cleanups to the git
repository (#78) and implementation of Github Actions for CI (#65, #57).
- GEMPAK now supports the newly added "Super-Res" NEXRAD Level III products (#73).

