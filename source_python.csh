#
# Make sure NAWIPS directory exists
#
if ( -d /awips2/python ) then
    setenv PYHOME "/awips2/python"
    set pv = `$PYHOME/bin/python -V |& cut -c8- | cut -d. -f1`
    set pr = `$PYHOME/bin/python -V |& cut -c8- | cut -d. -f2`
    setenv PYTHONPATH "${PYHOME}/lib/python${pv}.${pr}/site-packages:${NAWIPS}/scripts/python"
else
    if($?PYTHONPATH) then
        setenv PYTHONPATH "${NAWIPS}/scripts/python:${PYTHONPATH}"
    else
        setenv PYTHONPATH "${NAWIPS}/scripts/python"
    endif
endif

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

