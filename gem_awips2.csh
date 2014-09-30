#!/bin/csh

# If the AWIPS installation of Python exists, add Python to the path
# and library search and create variables used during the build process.

if ( -d /awips2/python ) then
    setenv PYHOME       /awips2/python
else
    unsetenv PYHOME
endif

if ( $?PYHOME ) then
      setenv PATH ${PATH}:${PYHOME}/bin
endif

# Add to the PATH
if ( $?PYHOME )  then
    foreach adir ( $PYHOME/bin )
	if  ( -d ${adir} )  then
	    echo $PATH | egrep -i '(^|:)'${adir}'(:|$)' >& /dev/null
	    if ( $status != 0 ) then
		set path = ( ${adir} ${path} )
	    endif
	endif
    end
endif

# LD_LIBRARY_PATH is included here because it was not needed until
# building functions that called Python scripts.
if ( $?LD_LIBRARY_PATH ) then
    # Reverse order for prepending
    foreach adir ( ${OS_LIB} /usr/lib /lib )
	if  ( -d ${adir} )  then
	    echo $LD_LIBRARY_PATH | egrep -i '(^|:)'${adir}'(:|$)' >& /dev/null
	    if ( $status != 0 ) then
		setenv LD_LIBRARY_PATH ${adir}:${LD_LIBRARY_PATH}
	    endif
	endif
    end
else
    setenv LD_LIBRARY_PATH /lib:/usr/lib:${OS_LIB}
endif

# Set up the variables used during the build
if ($?PYHOME ) then
    set pv = `$PYHOME/bin/python -V |& cut -c8- | cut -d. -f1`
    set pr = `$PYHOME/bin/python -V |& cut -c8- | cut -d. -f2`
    setenv PYINC "-I${PYHOME}/include/python${pv}.${pr}"
    setenv PYLIB "-L/awips2/python/lib -lpython${pv}.${pr}"
    setenv WITHPY "-DWITHPYTHON"
    setenv PYDEP "-lpthread -ldl -lutil"
    
    # Add to the PYTHONPATH for finding the scripts
    if ( $?PYTHONPATH ) then
	foreach adir ( ${PYHOME}/lib/python${pv}.${pr}/site-packages \
			/awips2/fxa/bin/src ${NAWIPS}/scripts/python )
	    if  ( -d ${adir} )  then
		echo $PYTHONPATH | egrep -i '(^|:)'${adir}'(:|$)' >& /dev/null
		if ( $status != 0 ) then
		    setenv PYTHONPATH ${PYTHONPATH}:${adir}
		endif
	    endif
	end
    else
	setenv PYTHONPATH ${PYHOME}/lib/python${pv}.${pr}/site-packages:${NAWIPS}/scripts/python:/awips2/fxa/bin/src
    endif

else

    # The AWIPS Python installation is not available
    setenv PYINC ""
    setenv PYLIB ""
    setenv PYDEP ""
    setenv WITHPY ""
endif
