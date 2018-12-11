#
# Make sure NAWIPS directory exists
#
# Python for GEMPAK
if [ -d "/awips2/python" ]; then
    export PYHOME="/awips2/python"
    pv="`${PYHOME}/bin/python -V 2>&1 | cut -c8- | cut -d. -f1`"
    pr="`${PYHOME}/bin/python -V 2>&1 | cut -c8- | cut -d. -f2`"
    export PYTHONPATH="${PYHOME}/lib/python${pv}.${pr}/site-packages:${NAWIPS}/scripts/python"
else
    export PYTHONPATH="${NAWIPS}/scripts/python:${PYTHONPATH}"
fi

if [ ! -d $NAWIPS ] ; then
    echo "Can not find NAWIPS distribution."
    echo 'Check Gemenviron NAWIPS definition ->' $NAWIPS
    unset NAWIPS
    exit
fi

export PYINC="-I${PYHOME}/include/python${pv}.${pr}"
export PYLIB="-lpython${pv}.${pr}"
export WITHPY="-DWITHPYTHON"
export PYDEP="-lpthread -ldl -lutil"
export LDFLAGS="-L${PYHOME}/lib -L$OS_LIB -s"
