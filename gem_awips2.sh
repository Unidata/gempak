#!/bin/sh

# If the AWIPS installation of Python exists, add Python to the path
# and library search and create variables used during the build process.

if [ -d /awips2/python ]; then
    export PYHOME="/awips2/python"
else
    export PYHOME=""
fi

# Add to the PATH
if [ -z "${PYHOME}" ]; then
    export PATH=${PYHOME}/bin:$PATH
fi

# LD_LIBRARY_PATH is included here because it was not needed until
# building functions that called Python scripts.
if [ -z "${LD_LIBRARY_PATH}" ]; then
    export LD_LIBRARY_PATH=/lib:/usr/lib:${OS_LIB}
else
    for adir in $OS_LIB /usr/lib /lib ; do
      if [ -d $adir ]; then
	if ! echo "$LD_LIBRARY_PATH" | /bin/egrep -q "(^|:)${adir}($|:)" ; then
	    export LD_LIBRARY_PATH=${adir}:$LD_LIBRARY_PATH
	fi  
      fi  
    done
fi

# Set up the variables used during the build
if [ -z "${PYHOME}" ]; then
    # The AWIPS Python installation is not available
    export PYINC=""
    export PYLIB=""
    export WITHPY=""
    export PYDEP=""
else
    pv="`$PYHOME/bin/python -V 2>&1 | cut -c8- | cut -d. -f1`"
    pr="`$PYHOME/bin/python -V 2>&1 | cut -c8- | cut -d. -f2`"
    export PYINC="-I/awips2/python/include/python${pv}.${pr}"
    export PYLIB="-L/awips2/python/lib -lpython${pv}.${pr}"
    export WITHPY="-DWITHPYTHON"
    export PYDEP="-lpthread -ldl -lutil"

    # Add to the PYTHONPATH for finding the scripts
    if [ -z "${PYTHONPATH}" ]; then
	export PYTHONPATH="${PYHOME}/lib/python${pv}.${pr}/site-packages:/awips2/fxa/bin/src:${NAWIPS}/scripts/python"
    else
	for adir in ${PYHOME}/lib/python${pv}.${pr}/site-packages /awips2/fxa/bin/src ${NAWIPS}/scripts/python ; do
	  if [ -d $adir ]; then
	    if ! echo "$PYTHONPATH" | /bin/egrep -q "(^|:)${adir}($|:)" ; then
		export PYTHONPATH=$PYTHONPATH:${adir}
	    fi  
	  fi  
	done
    fi
fi
