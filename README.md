# GEMPAK

This document is intended to guide the user through the process of downloading, configuring and installing GEMPAK v6.10 for most common systems (32 and 64-bit Linux and Solaris). Specific instructions for <a href="http://www.unidata.ucar.edu/software/gempak/doc/install_osx.html">building on OS X</a> are available as well.

Last Updated August 2013

## Download

GEMPAK can be installed by an individual user, and root / superuser permission is not required. It's assumed for this guide and throughout other online documentation that GEMPAK is installed for a gempak user account in the home directory.

* ### from Unidata

	The current release of GEMPAK is provided as a gzip/tarball archive and can found on the <a href="http://www.unidata.ucar.edu/downloads/gempak/">Unidata GEMPAK download page</a>. 
	
	Unpacking the archive in the gempak user home directory (assuming **/home/gempak**):

		tar -xvzf gempak-6.10-1.tar.gz

	will create the directory **/home/gempak/GEMPAK6.10/** which contains all of the libraries, program source and configuration files, most notably the two Gemenviron and Gemenviron.profile.

	


* ### from GitHub

	GEMPAK may be download from the <a href="https://github.com/Unidata/gempak"">GitHub repository</a> in a zip file, or via `git` on the command line.
	
	In **/home/gempak**, to clone into the directory **/home/gempak/GEMPAK6.10**:
	
		git clone git://github.com/Unidata/gempak.git GEMPAK6.10

	This will copy the GEMPAK source to the present working directory.  If downloaded from GitHub, ensure that the source is located in a directory under the user's home directory, such as **/home/gempak/GEMPAK6.10/**

## Create the NAWIPS symbolic link

Creating a symbolic link NAWIPS in your home directory will allow you to maintain a single command in **.cshrc** or **.profile** that sources the current GEMPAK installation:


	ln -s GEMPAK6.10/ NAWIPS
	
Your **gempak** home directory should then contain the following:

	/home/gempak/GEMPAK6.10
	/home/gempak/NAWIPS -> GEMPAK6.10

## Gemenviron / Gemenviron.profile

To build GEMPAK on your system, the environmental variable **$NAWIPS** must be defined at the top of **Gemenviron** [ for csh/tcsh ] or **Gemenviron.profile** [ for bash/ksh ]. These files contain definitions for the locations of various components used by GEMPAK. It is the responsibility of the GEMPAK administrator to confirm that **$NAWIPS** is defined correctly with the full path of the `GEMPAK6.10` directory. Other variables which point to the location of data directories, tables and can be tailored at your lesiure, but it's recommended that only **$NAWIPS** be modified as all other variables related to GEMPAK are dependant on **$NAWIPS**.

If you unpacked GEMPAK in **/home/gempak** and plan on compiling with gfortran, you can skip this step.

If you unpacked and plan to install GEMPAK in an uncommon directory (such as **/opt/gempak/**), you'll need to edit the NAWIPS definition at the top of the appropriate file:

	setenv NAWIPS /home/gempak/GEMPAK6.10

## Sourcing Environmental Variables

### csh/tcsh

source the full path of Gemenviron in your .cshrc

	source /home/gempak/NAWIPS/Gemenviron

### bash

add the following to .profile

	. /home/gempak/NAWIPS/Gemenviron.profile

Important environmental variable to be familiar with are:

	$NAWIPS as defined above
	$GEMPAK points to $NAWIPS/gempak
	$GEMTBL points to $GEMPAK/tables
	$OS_BIN points to $GEMPAK/os/$NA_OS/bin
	$OS_LIB points to $GEMPAK/os/$NA_OS/lib
	$GEMMAPS points to $GEMPAK/maps
	$GEMDATA points to /data/ldm/gempak
	$NA_OS is determined by Gemenviron (linux, linux64, x86, SunOS, Darwin, etc. )

The last definition, **$GEMDATA**, points to the **/data/ldm/gempak** but can be changed depending on how you <a href="http://www.unidata.ucar.edu/software/gempak/doc/configuration.html">install and configure the LDM</a>.

## Required Libraries

If your system uses standard locations for its compilers, X11/Motif libraries and make utilities, you do not needto change any settings. It's assumed both an ansi compatible C compiler and a Fortran 77 compatible compiler are available.

To build locally from source, you must have the developer environments installed for X11, Open Motif and libXp.

### X11

* libX11
* libX11-devel

### Open Motif

* openmotif
* openmotif-devel

The Open Motif 2.3.4 source tarball can be downloaded from the <a href="http://sourceforge.net/projects/motif/files/">motif sourceforge page</a>.

Binary packages are also available for <a href="http://sourceforge.net/projects/motif/files/">Red Hat, Fedora and Ubuntu</a> operating systems.

Some operating systems provide the MOTIF libraries, but some only install them as an option. 
### libXp

* libXp
* libXp-devel

### Full List of Required Libraries

Some systems, most notably recent releases of Fedora Core (fc16 and fc17) require a number of packages be installed in order to successfully build OpenMotif and GEMPAK. Below is a list of all required packages, followed by a single yum install command to take care of them in one instance: 

* gcc
* gcc-c++
* gcc-gfortran
* libX11
* libX11-devel
* libXt-devel
* libXext-devel
* libXp
* libXp-devel
* libXft-devel
* libXtst-devel
* xorg-x11-xbitmaps
* flex
* byacc
* \*fonts-ISO8859-\*

To install all at once: 

	yum install gcc gcc-c++ gcc-gfortran libX11 libX11-devel libXt-devel \
	libXext-devel libXp libXp-devel libXft-devel libXtst-devel \
	xorg-x11-xbitmaps flex byacc *fonts-ISO8859-* -y

## Build Options

### Gemenviron / Gemenviron.profile

To change the GCC compiler, edit the **USE_GFORTRAN**, **USE_PGI** and **USE_G77** definitions in **Gemenviron** or **Gemenviron.profile** (**gfortran** is the default):

	set USE_GFORTRAN=1

## Makeinc.common

For most systems, GEMPAK will be built using either g77 or gfortran[4.1+]. Files located in **$NAWIPS/config/** contain information specific to the individual platform you will be installing on (sol, x86, hpux, linux, osf, irix, aix, freebsd, darwin).

### SunOS-Specific Options

Bundled cc is not ansi compatible, so SunOS users should make sure SUNWspro/bin/cc is found before **/usr/ucb/cc** in your path. Also, you will need to ensure that /usr/ccs/bin is in your path.

### Ubuntu Linux

Ubuntu builds require the compiler flag `-fno-stack-protector` be added to **COPT** (C compiler options) and FOPT (fortran compiler options). Ubuntu-specific Makeinc files are provided in **$NAWIPS/config/** that include this compiler flag:

	$NAWIPS/config/Makeinc.linux_gfortran_ubuntu
	$NAWIPS/config/Makeinc.linux64_gfortran_ubuntu

### Mac OS X

<a href="http://www.unidata.ucar.edu/software/gempak/doc/install_osx.html">Mac OS X GEMPAK Installation Guide</a> by Kevin Tyle, University of Albany.

This document will guide users through building GEMPAK on Intel Mac OS X 10.6 (Snow Leopard) with GCC 4.4 compilers. The same procedure should also work for 10.5 (Leopard), but earlier versions/CPU's of OS X remain untested.

## Compiling GEMPAK Source

### Make

GEMPAK and various bundled libearies are compiled from source by the Make tool, using the Fortran and C compilers specified in the above configuration files. From $NAWIPS issue the command

	make all >& make.out

The redirection command `>&` sends **STDOUT** to the log file make.out. The Make process takes several minutes to complete, first compiling libraries such as netCDF, ncepBUFR, HDF5 and zlib, followed by the GEMPAK source. In another terminal you can follow the STDOUT log with the command

	tail -f make.out

or attach an ampersand to send make to the background and tail the log instantly

	make all >& make.out & tail -f make.out

This process allows you to examine the build as it runs but preserves the output in the event something goes horribly wrong. If seeking help from Unidata, attach some or all of this make.out log. One should note that with the Makefile installation, only fatal errors (errors that cause the build to end prematurely) are significant, and most "warnings" can be ignored.

## Install & Clean

	make install; make clean

Executables are installed to **$OS_BIN** and libraries should already be located in **$OS_LIB**.

The command make clean will remove compiled objects and binary executables from the source tree.

If your distribution is modified by patches or other edits and updates and you have to rebuild, the compilation time will be reduced as the libraries which took the majority of the time to build already exist.

## Cleaning a Build

In the event that core definitions change and libraries must be built from scratch, a the entire install for your system will need to be cleaned:

	make distclean

This full clean will delete all objects and executables as well as all libraries built for the current system.

### Everything

	make everything

This command will combine the steps of make all, make install and make clean to build and install the package, but will not log **STDOUT** to file. The command also runs the `make programs_nc` and `make programs_gf` targets mentioned below.

### GF & NC Programs

	make programs_nc

and

	make programs_gf

build versions of programs which link directly to device drivers (eliminating the gplt/message queue interface). These are primarily used for projects where multiple scripts are run at scheduled times, as in the cron, and don't require user interaction.

Everything Okay? Continue on to <a href="http://www.unidata.ucar.edu/software/gempak/doc/configuration.html">LDM & GEMPAK Setup</a>

## Troubleshooting

### Missing MOTIF Libraries

Any error messages involving missing Xm files (such as Xm/XmAll.h):

<pre>
> ...include/geminc.h:59:22: Xm/XmAll.h: No such file or directory
> In file included from /home/gempak/GEMPAK6.10/include/proto.h:28,
>    from /home/gempak/GEMPAK6.10/include/gemprm.h:647,
</pre>

Indicate that openmotif and openmotif-devel were not found on your system. Either the motif libraries are not installed on your system, or the correct location has not been specified to the compiler. If the latter is the case, you will need to edit **$NAWIPS/config/Makeinc.linux_gfortran** (assuming a linux system using gfortran), where $NA_OS is your system architecture as determined in the Gemenviron configuration file.

### Incorrectly sourced Gemenviron/Gemenviron.profile

The runtime error

<pre>
> [GEMPLT -101]  NOPROC - Nonexistent executable
</pre>

indicates that your environmental variables are not sourced correctly (type command `env` to see all envars and make sure the Gemenviron configuration file is edited and sourced as described above).

### Error in message queue

<pre>
> Error in message send = 22
> itype, ichan, nwords,0,65536,3
</pre>

This runtime error means the message queue is full, which typically occurs when executing GEMPAK programs in batch mode from scripts. To fix, exit GEMPAK and run

	cleanup -c

This command will remove existing message queues (the **ipcrm** command is run to remove message queues found with **ipcs**). It will also look for running **gplt** processes and kill them.

## Legal Disclaimer

GEMPAK was developed by the National Aeronautics and Space
Administration and the National Oceanic and Atmospheric Administration.
GEMPAK is provided by UCAR on an "AS IS" basis and any warranties,
either express or implied, including but not limited to implied
warranties of noninfringement, originality, merchantability and fitness
for a particular purpose, are disclaimed. UCAR will not be obligated to
provide the user with any support, consulting, training or assistance of
any kind with regard to the use, operation and performance of GEMPAK nor
to provide the user with any updates, revisions, new versions, error
corrections or "bug" fixes. In no event will UCAR be liable for any damages,
whatsoever, whether direct, indirect, consequential or special, which may
result from an action in contract, negligence or other claim that arises
out of or in connection with the access, use or performance of GEMPAK
including infringement actions.

