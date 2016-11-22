#
# Make sure NAWIPS directory exists
#
if [ ! -d $NAWIPS ] ; then
    echo "Can not find NAWIPS distribution."
    echo 'Check Gemenviron NAWIPS definition ->' $NAWIPS
    unset NAWIPS
    exit
fi

# Python for GEMPAK
COMMAND=`rpm -q awips2-python`
if [ $? -eq 0 ]; then
    export PYHOME="/awips2/python"
    pv="`${PYHOME}/bin/python -V 2>&1 | cut -c8- | cut -d. -f1`"
    pr="`${PYHOME}/bin/python -V 2>&1 | cut -c8- | cut -d. -f2`"
    export PYTHONPATH="${PYHOME}/lib/python${pv}.${pr}/site-packages:${NAWIPS}/scripts/python"
else
    export PYHOME="/usr"
    pv="`${PYHOME}/bin/python -V 2>&1 | cut -c8- | cut -d. -f1`"
    pr="`${PYHOME}/bin/python -V 2>&1 | cut -c8- | cut -d. -f2`"
    MACHTEST=${MACHTYPE:=`uname -m`}
    IS64=`echo $MACHTEST | grep -c "_64"`
    if [ ${IS64} -gt 0 ] ; then
	ARCH="64"
    fi
    export PYTHONPATH="${PYHOME}/lib64/python${pv}.${pr}/site-packages:${PYHOME}/lib/python${pv}.${pr}/site-packages:${NAWIPS}/scripts/python"
    export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/lib${ARCH}
fi

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${OS_LIB}

# this is needed for the build, not required at runtime
export PYINC="-I${PYHOME}/include/python${pv}.${pr}"
export PYLIB="-lpython${pv}.${pr}"
export WITHPY="-DWITHPYTHON"
export PYDEP="-lpthread -ldl -lutil"
export LDFLAGS="-L${PYHOME}/lib -L$OS_LIB -s"
