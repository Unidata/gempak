#
# Make sure NAWIPS directory exists
#
if ( ! -d $NAWIPS ) then
        echo "Can not find NAWIPS distribution."
        echo 'Check Gemenviron NAWIPS definition ->' $NAWIPS
        unsetenv NAWIPS
        exit
endif

setenv PYINC "-I${PYHOME}/include/python${pv}.${pr}"
setenv PYLIB "-lpython${pv}.${pr}"
setenv WITHPY "-DWITHPYTHON"
setenv PYDEP "-lpthread -ldl -lutil"
setenv LDFLAGS "-L${PYHOME}/lib -L$OS_LIB -s"

