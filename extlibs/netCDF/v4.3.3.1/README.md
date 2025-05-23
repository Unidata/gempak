Unidata NetCDF
==============

[![Build Status](https://travis-ci.org/Unidata/netcdf-c.svg?branch=master)](https://travis-ci.org/Unidata/netcdf-c)

<a href="https://scan.coverity.com/projects/157">
  <img alt="Coverity Scan Build Status"
       src="https://scan.coverity.com/projects/157/badge.svg"/>
</a>

The Unidata network Common Data Form (netCDF) is an interface for
scientific data access and a freely-distributed software library that
provides an implementation of the interface.  The netCDF library also
defines a machine-independent format for representing scientific data.
Together, the interface, library, and format support the creation,
access, and sharing of scientific data.  The current netCDF software
provides C interfaces for applications and data.  Separate software
distributions available from Unidata provide Java, Fortran, Python,
and C++ interfaces.  They have been tested on various common
platforms.

NetCDF files are self-describing, network-transparent, directly
accessible, and extendible.  `Self-describing` means that a netCDF file
includes information about the data it contains.  `Network-transparent`
means that a netCDF file is represented in a form that can be accessed
by computers with different ways of storing integers, characters, and
floating-point numbers.  `Direct-access` means that a small subset of a
large dataset may be accessed efficiently, without first reading through
all the preceding data.  `Extendible` means that data can be appended to
a netCDF dataset without copying it or redefining its structure.

NetCDF is useful for supporting access to diverse kinds of scientific
data in heterogeneous networking environments and for writing
application software that does not depend on application-specific
formats.  For information about a variety of analysis and display
packages that have been developed to analyze and display data in
netCDF form, see

* http://www.unidata.ucar.edu/netcdf/software.html

For more information about netCDF, see the netCDF Web page at

* http://www.unidata.ucar.edu/netcdf/

You can obtain a copy of the latest released version of netCDF software
from

* http://github.com/Unidata/netcdf-c
* http://github.com/Unidata/netcdf-fortran
* http://github.com/Unidata/netcdf-cxx4
* http://github.com/Unidata/netcdf4-python

Copyright and licensing information can be found here, as well as in
the COPYRIGHT file accompanying the software

* http://www.unidata.ucar.edu/software/netcdf/copyright.html

To install this package, please see the file INSTALL in the
distribution, or the (usually more up-to-date) document:

* https://docs.unidata.ucar.edu/netcdf-c/current/netCDF-CMake.html

The netCDF FORTRAN interfaces are documented at

* https://docs.unidata.ucar.edu/netcdf-fortran/current/

User's Guides are also available in several forms from the same
location.

A mailing list, netcdfgroup@unidata.ucar.edu, exists for discussion of
the netCDF interface and announcements about netCDF bugs, fixes, and
enhancements.  For information about how to subscribe, see the URL

* http://www.unidata.ucar.edu/netcdf/mailing-lists.html

We appreciate feedback from users of this package.  Please send
comments, suggestions, and bug reports to
<support-netcdf@unidata.ucar.edu>.  Please identify the version of the
package (file VERSION).
