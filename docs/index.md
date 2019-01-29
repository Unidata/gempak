GEMPAK is an analysis, display, and product generation package for meteorological data. Originally developed by NCEP for use by the National Centers (SPC, TPC, AWC, and others), UCAR's Unidata Program Center releases and support a non-operational version of GEMPAK for use in research and education.

GEMPAK is available as a pre-built binary for 64-bit Linux systems (Red Hat, CentOS, Fedora Core). A source code tarball is available for builds on other systems.

## History 

GEMPAK was orginially developed by the Severe Storms Laboratory at NASA's Goddard Space Flight Center in the early 1980's. The Unidata Program Center release of GEMPAK/NAWIPS incorporates additions developed locally to enhance the use of realtime data aquired through the IDD, as well as instructional case studies.

As a software package, GEMPAK is made up of application programs, libraries, graphics routines, and device drivers. In order to perform data analysis the application program calls on a set of libraries to perform the given tasks. When the graphic is constructed, the application program (such as GPMAP) starts a separate graphics program (the GPLT graphics process), which handles graphical processing such as mapping, symbol drawing, and coordinate transformations. 

## Workshop Specific Instructions

This user manual assumes the use of a Linux terminal window (a shell) to run GEMPAK programs, either with bash or tcsh/csh.  You will need to edit the file `~/.cshrc` to source the GEMPAK environmental variable file `/home/gempak/NAWIPS/Gemenviron`:

Edit the file with  `vi`:

    vi .cshrc

and add the line

    source /home/gempak/NAWIPS/Gemenviron


save the file with the command `:wq`, close the terminal window and launch another one.  In this new window, check that the GEMPAK variables are sourced with the command `env | grep GEM`; it should look like this:
    
    env | grep GEM
    NAWIPS=/home/gempak/GEMPAK6.6.0
    GEM_COMPTYPE=gfortran
    GEMPAK=/home/gempak/GEMPAK6.6.0/gempak
    GEMPAKHOME=/home/gempak/GEMPAK6.6.0/gempak
    CONFIGDIR=/home/gempak/GEMPAK6.6.0/config
    OS_ROOT=/home/gempak/GEMPAK6.6.0/os/linux64
    OS_BIN=/home/gempak/GEMPAK6.6.0/os/linux64/bin
    OS_INC=/home/gempak/GEMPAK6.6.0/os/linux64/include
    OS_LIB=/home/gempak/GEMPAK6.6.0/os/linux64/lib
    GEMLIB=/home/gempak/GEMPAK6.6.0/os/linux64/lib
    GEMEXE=/home/gempak/GEMPAK6.6.0/os/linux64/bin
    GEMPDF=/home/gempak/GEMPAK6.6.0/gempak/pdf
    GEMTBL=/home/gempak/GEMPAK6.6.0/gempak/tables
    GEMERR=/home/gempak/GEMPAK6.6.0/gempak/error
    GEMHLP=/home/gempak/GEMPAK6.6.0/gempak/help
    … 


GEMPAK programs are located in the directory `~/NAWIPS/os/linux/bin/`, or `$OS_BIN` for short.


