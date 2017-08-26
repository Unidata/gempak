#
# Make sure NAWIPS directory exists
#
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
