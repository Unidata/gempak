#!/bin/csh
#
# DELIVER.CSH
#
# This script will use the tar command to create deliverables of
# the N-AWIPS software.
#
##
# Log:
# S. Jacobs/NMC		 7/94	Generalized and updated
# S. Jacobs/NMC		 3/95	Moved .cshrc, .cshrc_data and .login
#					to both source and execs
# S. Jacobs/NMC		 3/95	Removed filename input
# S. Jacobs/NMC		 6/95	Added .cshrc.user and .login.user
# S. Jacobs/NCEP	 5/96	Updated for V5.3.1
# S. Jacobs/NCEP	11/96	Updated for delivery to COSMIC
# S. Jacobs/NCEP	12/96	Removed Gemenviron
# S. Jacobs/NCEP	12/96	Added LDM delivery
# S. Jacobs/NCEP	 3/97	Added tape option
# S. Jacobs/NCEP	 3/97	Added backup option
# S. Jacobs/NCEP	 2/99	Cleaned up tape devices
# S. Jacobs/NCEP	12/99	Added rfc option
# S. Jacobs/NCEP	 2/00	Changed tape to exetape; Added srctape
# S. Jacobs/NCEP	 1/01	Added deliver option
# S. Jacobs/NCEP	11/01	Added awips option
# S. Jacobs/NCEP	10/02	Added check for linux2.2 for exe tar files
# S. Jacobs/NCEP	 3/04	Cleaned up old options
#

set dt=`date "+%Y%m%d"`

if ( $#argv < 2 ) then

    echo " "
    echo " "
    echo " "
    echo " "
    echo "    USAGE: $0 version type [machine]"
    echo " "
    echo "              where 'version' is the release number #.#.# and" 
    echo "              the 'type' may have the following values:"
    echo "              source   ldm        backup     srctape  awips"
    echo "              execs    exeonly    srcexecs   exetape  rfc"
    echo " "
    echo "              The optional input 'machine' is used for delivering"
    echo "              executables only, and may have the following"
    echo "              values:  aix5  hpux11  linux2.4el  linux2.6el"
    echo " "
    echo " "
    echo " "

else

    set vers=${1}
    set type=${2}

  if ( ${type} == "source" ) then

    cd $NAWIPS

    set echo

    tar cvf $NAWIPS/deliver/tarfiles/source_${dt}.tar \
				bin doc extlibs gempak icons ldm \
				resource scripts versions \
				.cshrc_${vers} .cshrc_data_${vers} .cshrc.user \
				.login_${vers} .login.user \
				.profile_${vers} .profile_data_${vers}

    unset echo

    cd $NAWIPS/deliver/tarfiles
    compress source_${dt}.tar

  else if ( ${type} == "srctape" ) then

    cd $NAWIPS

    set echo

#   4mm tape on hp64
    if ( `hostname` == "hp64" )  then
	set command="tar cvf /dev/rmt/c1t3d0BESTn"

#   4mm tape on hp65
    else if ( `hostname` == "hp65" )  then
	set command="tar cvf /dev/rmt/c1t3d0BESTn"

    else
	set command="echo"
    endif

    $command	bin doc gempak icons \
		resource scripts versions \
		.cshrc .cshrc_data .cshrc.user \
		.login .login.user .profile .profile_data

    unset echo

  else if ( ${type} == "ldm" ) then

    cd $NAWIPS

    set echo

    tar cvf $NAWIPS/deliver/tarfiles/ldm_${dt}.tar ldm 

    unset echo

    cd $NAWIPS/deliver/tarfiles
    compress ldm_${dt}.tar

  else if ( ${type} == "backup" ) then

    cd $NAWIPS

    set echo

#   4mm tape on hp64
    if ( `hostname` == "hp64" )  then
	set command="tar cvf /dev/rmt/c1t3d0BESTn"

#   4mm tape on hp65
    else if ( `hostname` == "hp65" )  then
	set command="tar cvf /dev/rmt/c1t3d0BESTn"

    else
	set command="echo"
    endif

    $command .[a-z]* [a-ce-z]* doc deliver/*.csh

    unset echo

  else if ( ${type} == "deliver" ) then
    cd $NAWIPS

    set echo

    tar cvf $NAWIPS/deliver/tarfiles/deliver_${dt}.tar	\
	bin doc icons ldm resource scripts \
	gempak/{climo,data,error,fonts,help,include,maps,nts,pdf,tables} \
	exe lib versions

    unset echo

    cd $NAWIPS/deliver/tarfiles
    compress deliver_${dt}.tar

  else if ( ${type} == "rfc" ) then

	cd $NAWIPS

	set echo
#exe/hpux10/{ntl,nmap,nmap2,fax,gf,gn,nc,ps,rbk,tiff,utf,vg,xw,xwp,gplt} \
#exe/hpux10/{gdlist,gdgrib_nc,gpmap,gpend,nagrib_nc} \
#lib/hpux10 \

	tar cvf $NAWIPS/deliver/tarfiles/rfc_${dt}.tar	\
	    bin icons resource scripts \
	    gempak/{climo,data,error,help,nts,pdf,tables,include} \
	    gempak/maps/[b-mprst]* \
	    exe/linux2.4el/{ntl,nmap2,fax,gf,gn,nc,ps,rbk,tiff,utf,vg,xw,xwp,gplt} \
	    exe/linux2.4el/{gdlist,gdgrib_nc,gpmap,gpend,nagrib_nc} \
	    versions \
	    .cshrc_${vers} .cshrc_data_${vers} .login_${vers} \
	    .profile_${vers} .profile_data_${vers}

	unset echo

	cd $NAWIPS/deliver/tarfiles
	compress rfc_${dt}.tar

  else if ( ${type} == "awips" ) then

	cd $NAWIPS

	set echo
#exe/hpux10 \
#lib/hpux10 \

	tar cvf $NAWIPS/deliver/tarfiles/awips_${dt}.tar	\
	    awips doc bin icons resource scripts \
	    gempak/{climo,data,error,fonts,help,include,maps,nts,pdf,tables} \
	    exe/linux2.4el \
	    lib/linux2.4el \
	    versions \
	    .cshrc_${vers} .cshrc_data_${vers} .login_${vers} \
	    .profile_${vers} .profile_data_${vers}

	unset echo

	cd $NAWIPS/deliver/tarfiles
	compress awips_${dt}.tar

  else if ( ${type} == "execs" || ${type} == "srcexecs" || ${type} == "exetape" || ${type} == "exeonly" ) then

    if ( $#argv < 3 ) then

	echo " "
        echo "---------------------------------"
	echo "  You must give a machine type."
        echo "---------------------------------"
	echo " "

	echo " "
 	echo " "
	echo "    USAGE: $0 version type [machine]"
	echo " "
	echo "              where 'version' is the release number #.#.# and" 
	echo "              the 'type' may have the following values:"
	echo "              source   ldm        backup     srctape  awips"
	echo "              execs    exeonly    srcexecs   exetape  rfc"
	echo " "
	echo "              The optional input 'machine' is used for delivering"
	echo "              executables only, and may have the following"
	echo "              values:  aix5  hpux11  linux2.4el  linux2.6el"
	echo " "
	echo " "
	echo " "

    else
	set mach=${3}
      
      set exlist = "exe/${mach} lib/${mach}"

      if ( ${type} == "execs" ) then

	cd $NAWIPS

	set echo

	tar cvf $NAWIPS/deliver/tarfiles/execs_${mach}_${dt}.tar	\
			bin doc icons resource scripts \
		gempak/{climo,data,error,fonts,help,include,maps,nts,pdf,tables} \
			$exlist versions \
			.cshrc .cshrc_data .login \
			.cshrc.user .login.user .profile .profile_data

	unset echo

	cd $NAWIPS/deliver/tarfiles
	compress execs_${mach}_${dt}.tar

      else if ( ${type} == "exeonly" ) then

	cd $NAWIPS

	set echo

	tar cvf $NAWIPS/deliver/tarfiles/exeonly_${3}_${dt}.tar	$exlist

	unset echo

	cd $NAWIPS/deliver/tarfiles
	compress exeonly_${mach}_${dt}.tar

      else if ( ${type} == "srcexecs" ) then

	cd $NAWIPS

	set echo

	tar cvf $NAWIPS/deliver/tarfiles/srcexecs_${mach}_${dt}.tar	\
			bin doc icons resource scripts \
			gempak $exlist nprogs versions \
			.cshrc .cshrc_data .login \
			.cshrc.user .login.user .profile .profile_data

	unset echo

	cd $NAWIPS/deliver/tarfiles
	compress srcexecs_${mach}_${dt}.tar

      else

	cd $NAWIPS

	set echo

#	4mm tape on SGI86
	if ( `hostname` == "sgi86" )  then
		set command="tar cvbf 20 /dev/rmt/tps0d5ns"

#	4mm tape on AS2
	else if ( `hostname` == "as2" )  then
		set command="tar cvf /dev/rmt/c201d0c"

#	8mm tape on hp8
	else if ( `hostname` == "hp8" )  then
		set command="tar cvf /dev/rmt/c201d3m"

#	4mm tape on hp64
        else if ( `hostname` == "hp64" )  then
		set command="tar cvf /dev/rmt/c1t3d0BESTn"

	else 
		set command="echo"

	endif


	$command \
		bin doc icons resource scripts \
		gempak $exlist nprogs versions \
		.cshrc .cshrc_data .login \
		.cshrc.user .login.user .profile .profile_data


	unset echo

      endif

    endif

  else

    echo " "
    echo "-------------------------------------"
    echo "  Error invalid input -- $1 $2"
    echo "-------------------------------------"
    echo " "

    echo " "
    echo "    USAGE: $0 version type [machine]"
    echo " "
    echo "		where 'version' is the release number #.#.# and" 
    echo "		the 'type' may have the following values:"
    echo "		source   ldm        backup     srctape  awips"
    echo "		execs    exeonly    srcexecs   exetape  rfc"
    echo " "
    echo "		The optional input 'machine' is used for delivering"
    echo "		executables only, and may have the following"
    echo "		values:  aix5  hpux11  linux2.4el  linux2.6el"
    echo " "
    echo " "
    echo " "

  endif

endif
