<!-- markdownlint-configure-file {"MD024": { "siblings_only": true } } -->
# Unidata Community GEMPAK Change Log

## 7.19.0.1 (14 Apr 2025)

Support GOES 19 (#162), patch by @vgensini

## 7.19.0 (12 Dec 2024)

This release merges upstream NCEP NAWIPS 7.19.0.

## 7.18.0 (24 May 2024)

This release merges upstream NCEP NAWIPS 7.18.0, changes are mostly table
and NWS Zone updates.

## 7.17.0 (29 Oct 2023)

This release merges upstream NCEP NAWIPS 7.17.0, changes are mostly table
and NWS Zone updates.

## 7.16.1 (21 June 2023)

This release merges upstream NCEP NAWIPS 7.16.1, whose CHANGELOG includes:

- Bug fix for upper air decoder (dcuair) where 21212 group is processed incorrectly.
- Bug fix for “white spots/specks” graphic issue on accumulated QPF.

## 7.16.0 (4 May 2023)

This release merges upstream NCEP NAWIPS 7.16.0, whose CHANGELOG includes:

- NWS zones updated to March 2023
- Small nmap2 fixes
- Volcanic Ash Advisory updates

## 7.15.2 (15 Feb 2023)

This release merges upstream NCEP NAWIPS 7.15.2, whose CHANGELOG includes:

- fix to gempak/source/bridge/ru/ruwhgt.f

This release also includes the following GEMPAK Community changes:

- Some fixes and cleanups to CI/build infrastructure.

## 7.15.1 (8 Oct 2022)

This release merges upstream NCEP NAWIPS 7.15.1, whose CHANGELOG includes:

- Build fixes on RHEL 8.
- GOES18 table additions.

This release also includes the following GEMPAK Community changes:

- Continued table cleanups.
- Build cleanups and fixes.

## 7.15.0 (25 May 2022)

This release merges upstream NCEP NAWIPS 7.15.0, whose CHANGELOG includes:

- Bug fixes and other issue resolutions.
- Cumulative revisions to NAWIPS data tables to support maps/bounds,
surface stations, tropical, and other routine periodic updates.
- Integration of compiler, and certain changes required to support the
WCOSS supercomputer and RHEL 8 Operating System transition projects.
- Updates to Volcanic Ash Advisory user interface to support the NESDIS
Satellite Analysis Branch.

This release also includes the following GEMPAK Community changes:

- More dynamic setting of `NAWIPS` environment variable on bash (#85).
- Update MRMS Grib Tables to v12.1 (#89).
- Continued cleanup of bad previous merges that overwrote Unidata
local table modifications.

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

