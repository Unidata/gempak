# Gemenviron file for GEMPAK
#
# Please configure the following definitions to reflect your system:
NAWIPS=/home/gempak/NAWIPS
export EDEX_SERVER="edex-cloud.unidata.ucar.edu"
#
#		Sets environment variables used in running GEMPAK
#               Modified  3/04 for Gempak5.7/NAWIPS (Chiz)
#---------------------------------------------------------------------
#  The command
#               . Gemenviron
#
#  or the individual lines below, must be included in your .login or 
#  .cshrc file!
#---------------------------------------------------------------------
# <<CONFIGURE>>
# To build GEMPAK on your system, you should set the NAWIPS variable 
# below to the top level of the source distribution tree on your system.
#
#
# Make sure NAWIPS directory exists
#
if [ ! -d $NAWIPS ] ; then
    echo "Can not find NAWIPS distribution."
    echo 'Check Gemenviron NAWIPS definition ->' $NAWIPS
    unset NAWIPS
    exit
fi
export NAWIPS
#
# Path for Garp tables and configuration. You should only have to
# modify GARP_PATH if you have unique Garp_defaults for specifc
# data sets, such as case studies. In those cases, creating
# a ntl.case startup script which sets those local environmental variables
# is generally the best solution.
#
# Uncomment the following lines for using GCC compilers (Solaris/HP only)
# USEGCC_SOL=1
# export USEGCC_SOL
# USEGCC_HPUX=1
# export USEGCC_HPUX
#
# Linux specific: defile which compiler will be used
USE_GFORTRAN=1
export USE_GFORTRAN
#
# USE_G77=1
# export USE_G77
#
# USE_PGI=1
# export USE_PGI
#
# --------------------------------------------------------------------
#try to determine operating system: command uname must be in path
#
MAKEINC="Makeinc.common"
if [ ! $NA_OS ] ; then
   TMP_OS=`uname -s | tr '[A-Z]' '[a-z]'`
   case $TMP_OS in
        aix )
           NA_OS="aix"
	   OS_MAJOR=`uname -v`
           if [ $OS_MAJOR == '5' ] ; then
              AIX_NO_GEMPAK_SCANDIR=1
	      export AIX_NO_GEMPAK_SCANDIR
           fi
           ;;
        hp-ux )
	   OS_MAJOR=`uname -r | cut -f1,2 -d.`
           if [ $OS_MAJOR == 'B.11' ] ; then
              XCFLAGS="-DSYSLOG_RETURNS_INT"
	      export XCFLAGS
           fi
	   if [ $USEGCC_HPUX ] ; then
	      NA_OS="hpux_gcc"
           else
              NA_OS= "hpux"
           fi
           b;;
        irix64 )
           XCFLAGS="-o32"
	   export XCFLAGS
	   ;;
        irix )
           NA_OS="irix"
           ;;
        osf1 )
           NA_OS="osf"
           uac p noprint # supress warnings in xw driver for unaligned access
           ;;
        sunos )
	   MAKEINC="Makeinc.common_sol"
           OS_MAJOR=`uname -r | sed 's/\..*//'`
           HARDWARE=`uname -i | tr '[A-Z]' '[a-z]' | sed 's/\,.*//'`
           NA_OS="sunos"
           #if [ [ $OS_MAJOR == 5 ] && [ $USEGCC_SOL ] ] ; then
           if [ $OS_MAJOR == 5 ] && [ $USEGCC_SOL ] ; then
              NA_OS="sol_gcc"
	   else
              NA_OS="sol"
           fi
           #if [ [ $NA_OS == 'sol' ] && [ $HARDWARE == 'i86pc' ] ] ; then
           if [ $NA_OS == 'sol' ] && [ $HARDWARE == 'i86pc' ] ; then
              NA_OS="x86"
           fi
           ;;
        ultrix )
           NA_OS="ultrix"
           ;;
	linux )
	   NA_OS="linux"
           if [ $USE_G77 ] ; then
              GEM_COMPTYPE="g77"
           fi
           if [ $USE_GFORTRAN ] ; then
              GEM_COMPTYPE="gfortran"
           fi
           if [ $USE_PGI ] ; then
              GEM_COMPTYPE="pgi"
           fi
           export GEM_COMPTYPE
	   #
	   # See if MACHTYPE is set, otherwise set it
	   MACHTEST=${MACHTYPE:=`uname -m`}
	   # search for _64 in x86_64
           IS64=`echo $MACHTEST | grep -c "_64"`
	   if [ ${IS64} -gt 0 ] ; then
              NA_OS="linux64"
           fi
	   ;;
	freebsd )
	   NA_OS="freebsd"
	   ;;
	cygwin )
	   NA_OS="cygwin"
	   ;;
	darwin )
	   NA_OS="darwin"
	   ;;
        * )
           echo "trouble determining operating system configuration"
           echo "OS reported $TMP_OS"
           exit
   esac
   export NA_OS
fi
export MAKEINC
# --------------------------------------------------------------------
#
# GEMPAK directory:
#
   GEMPAK=$NAWIPS/gempak
   export GEMPAK
   GEMPAKHOME=$NAWIPS/gempak
   export GEMPAKHOME
#
# CONFIGURATION directory
   CONFIGDIR=$NAWIPS/config
   export CONFIGDIR
#
# System environmental variables
#

   OS_ROOT=$NAWIPS/os/$NA_OS
   export OS_ROOT
   OS_BIN=$OS_ROOT/bin
   export OS_BIN
   OS_INC=$OS_ROOT/include
   export OS_INC
   OS_LIB=$OS_ROOT/lib
   export OS_LIB

#
# Directory for storing object libraries (GEMPAK + related software):
#
   GEMLIB=$OS_LIB
   export GEMLIB
#
# Directory for executables (GEMPAK + related software):
#
   GEMEXE=$OS_BIN
   export GEMEXE
#
# Remaining directories used by GEMPAK  (leave as is):
#
   GEMPDF=$GEMPAK/pdf
   export GEMPDF
   GEMTBL=$GEMPAK/tables
   export GEMTBL
   GEMERR=$GEMPAK/error
   export GEMERR
   GEMHLP=$GEMPAK/help
   export GEMHLP
   GEMMAPS=$GEMPAK/maps
   export GEMMAPS
   GEMNTS=$GEMPAK/nts
   export GEMNTS
   GEMPARM=$GEMPAK/parm
   export GEMPARM
   GEMPTXT=$GEMPAK/txt/programs
   export GEMPTXT
   GEMGTXT=$GEMPAK/txt/gemlib
   export GEMGTXT
#
   NMAP_RESTORE=$GEMNTS/nmap/restore ; export NMAP_RESTORE
#
#  MEL_BUFR environment
   MEL_BUFR=$NAWIPS/extlibs/melBUFR/melbufr ; export MEL_BUFR
   MEL_BUFR_TABLES=$GEMPAK/tables/melbufr ; export MEL_BUFR_TABLES
#
#
   BRDGDIR=$NAWIPS/unidata/ldmbridge ; export BRDGDIR
#
# Add NAWIPS to the X applications resource path.
#
xresources=""

if [ -d $NAWIPS/resource ] ; then
        xresources="$NAWIPS/resource/%N"
        if [ $XUSERFILESEARCHPATH ] ; then
           XUSERFILESEARCHPATH=${xresources}:${XUSERFILESEARCHPATH}
        else
           XUSERFILESEARCHPATH=$xresources
        fi
	export XUSERFILESEARCHPATH
fi

#
# 
# Set PATH to include $OS_BIN
#
PATH=${PATH}:${OS_BIN}:${NAWIPS}/bin ; export PATH

#
# --------------------------------------------------------------------
# <<CONFIGURE>>
# Data directories, either real time or the Hurricane Bob sample data set:
#
#  GEMDATA=$GEMPAK/data ; export GEMDATA
   GEMDATA=/data/ldm/gempak ; export GEMDATA
   OBS=$GEMDATA ; export OBS
#
# Location of meta files (generated with NC decide driver)
   NTRANS_META=$GEMDATA/meta ; export NTRANS_META
#
# Location of text bulletins for use with NWX
   TEXT_DATA=$GEMDATA/nwx ; export TEXT_DATA
#
#   Used by Nalarm (path component /nawips is added automatically by program). 
#   Write products into $GEMDATA/alarms/nawips for display.
   AFOS_ALARMS=$GEMDATA/alarms/ ; export AFOS_ALARMS
#
#  Location of NSAT directory tree "Required!!"...must exist
#
   SAT=$GEMDATA/images/sat ; export SAT
   RAD=$GEMDATA/nexrad ; export RAD
#
#
# Below follow  variables for data locations (site specific)
# Not required for NAWIPS, but may be used in scripts etc.
#
   LDMDATA=/data/ldm ; export LDMDATA
   GOES8=$SAT/GOES-8 ; export GOES8
   GOES9=$SAT/GOES-9 ; export GOES9
   MODEL=$GEMDATA/model ; export MODEL
   HDS=$MODEL ; export HDS
   MODEL_SND=$GEMDATA/modsnd/gempak/bufr/model ; export MODEL_SND
   MODEL_SFC=$GEMDATA/modsnd/gempak/bufr/model ; export MODEL_SFC
   SAO=$GEMDATA/surface ; export SAO
   UPA=$GEMDATA/upperair ; export UPA
   RAW_SAO=$LDMDATA/surface/sao ; export RAW_SAO
   RAW_SYN=$LDMDATA/surface/syn ; export RAW_SYN
   RAW_UPA=$LDMDATA/upperair ; export RAW_UPA
   NLDN=$GEMDATA/nldn ; export NLDN

   TORN_WARN=$TEXT_DATA/watch_warn/TOR ; export TORN_WARN
   TSTRM_WARN=$TEXT_DATA/watch_warn/SVR ; export TSTRM_WARN
   TEXT_WARN=$TEXT_DATA/watch_warn ; export TEXT_WARN

   RBKGPH=$GEMDATA/redbook ; export RBKGPH
   SIGWX=$GEMDATA/sigwx ; export SIGWX
   SPDATA=$GEMDATA ; export SPDATA

   if [ $HOME ] ; then
      GRPHGD=$HOME
   else
      GRPHGD=$GEMPAK
   fi
   export GRPHGD
#
#  Print command and flags for systems
   LP="lp -c" ; export LP
   LPFLAG="-d" ; export LPFLAG

##
## PYTHON for GEMPAK
##
# Set up the variables used during the build

if [ -d /awips2/python ]; then
    export PYHOME="/awips2/python"
    pv="`${PYHOME}/bin/python -V 2>&1 | cut -c8- | cut -d. -f1`"
    pr="`${PYHOME}/bin/python -V 2>&1 | cut -c8- | cut -d. -f2`"
    export PYTHONPATH="${PYHOME}/lib/python${pv}.${pr}/site-packages:${NAWIPS}/scripts/python"
else
    export PYHOME="/usr"
    pv="`${PYHOME}/bin/python -V 2>&1 | cut -c8- | cut -d. -f1`"
    pr="`${PYHOME}/bin/python -V 2>&1 | cut -c8- | cut -d. -f2`"
    if [[ ${pv}${pr} -lt 27 ]] ; then
      echo "python${pv}.${pr} is not supported for GEMPAK. Install python2.7+"
      return
    fi
    export PYTHONPATH="${PYHOME}/lib64/python${pv}.${pr}/site-packages:${PYHOME}/lib/python${pv}.${pr}/site-packages:${NAWIPS}/scripts/python"
    MACHTEST=${MACHTYPE:=`uname -m`}
    IS64=`echo $MACHTEST | grep -c "_64"`
    if [ ${IS64} -gt 0 ] ; then
      ARCH="64"
    fi
fi
# this is needed for the build, and not required at runtime
export LD_LIBRARY_PATH=/lib${ARCH}:/usr/lib${ARCH}:${OS_LIB}
export PYINC="-I${PYHOME}/include/python${pv}.${pr}"
export PYLIB="-L${PYHOME}/lib${ARCH} -lpython${pv}.${pr}"
export WITHPY="-DWITHPYTHON"
export PYDEP="-lpthread -ldl -lutil"
