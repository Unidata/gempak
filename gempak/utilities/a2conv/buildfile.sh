#! /bin/sh

SOURCES=( '
source/gemlib/cfl/cflinqr.c 
source/gemlib/cfl/cfliret.c
source/gemlib/cfl/cflrdln.c
source/gemlib/cfl/cflread.c
source/gemlib/cfl/cflropn.c
source/gemlib/cfl/cflseek.c
source/gemlib/cfl/cfluopn.c
source/gemlib/cfl/cflwopn.c
source/gemlib/cfl/cflwrit.c
source/cgemlib/cgr/cgrsegdist.c
source/gemlib/css/cssenvr.c
source/gemlib/cst/cstclst.c
source/gemlib/cst/cstcrnm.c
source/gemlib/cst/cstgtag.c
source/gemlib/cst/cstncpy.c
source/gemlib/cst/cstnocc.c
source/gemlib/cst/cstrmbl.c
source/gemlib/cst/cstrpst.c
source/gemlib/cst/cstrxbl.c
source/gemlib/cst/cstuclc.c
source/cgemlib/cvg/cvgallocgfablock.c
source/cgemlib/cvg/cvgrdgfa.c
source/cgemlib/cvg/cvgrdtca.c
source/cgemlib/cvg/cvgrdtc.c
source/cgemlib/cvg/cvgswap.c
' ) 

LCLSOURCES=( '
compareVgf.c
cvgopenj.c
cvgrdjele.c
cvgrdjhdr.c
cvgrdjrecnoc.c
cvgv2x.c
vgf2xml.c 
mvswp4.c
' ) 
 
INCLUDES=( '
include/afbufr_common.h
include/afbufr_structures.h
include/afcreatexml.h
include/bfrcmn.h
include/bridge.h
include/cascmn.h
include/cescmn.h
include/cgmcmn.h
include/color.h
include/cpgcmn.h
include/ctbcmn.h
source/cgemlib/cvg/cvgcmn.h
include/cvgdef.h
source/cgemlib/cvg/cvgtca.h
include/drwids.h
include/drwtbl.h
include/fortran_wrappers.h
include/gb2def.h
include/geminc.h
include/gemprm.h
include/g_error.h
include/gpc.h
include/grib2.h
include/hints.h
include/imgdef.h
include/nmpdef.h
include/nmsdef.h
include/no_xm_geminc.h
include/Nxm.h
include/NxmTxt.h
include/pgcmn.h
include/pgprm.h
include/proto_bfr.h
include/proto_bridge.h
include/proto_cap.h
include/proto_cas.h
include/proto_cgemlib.h
include/proto_cgr.h
include/proto_clo.h
include/proto_cmd.h
include/proto_cmm.h
include/proto_ctb.h
include/proto_dg.h
include/proto_gemlib.h
include/proto_gpc.h
include/proto_grc.h
include/proto.h
include/proto_na.h
include/proto_nfax.h
include/proto_nmaplib.h
include/proto_nxmlib.h
include/proto_textlib.h
include/proto_uka.h
include/proto_vf.h
include/proto_vg.h
include/proto_xw.h
include/pscmn.h
include/scnfll.h
include/sleep.h
include/shapefil.h
include/shpprm.h
include/spfcmn.h
include/uscore.h
include/usrtbl.h
include/vgftbl.h
include/vgstruct.h
include/vgtag.h
include/xwcmn.h
include/xwgui.h
include/xwpcmn.h
include/xwprm.h 
' ) 

A2SRC=$GEMPAK/utilities/a2conv/source/
A2LIB=$GEMPAK/utilities/a2lib

mkdir -p ${A2LIB} 

for file in ${SOURCES} ${INCLUDES} 
do 
  cp ${GEMPAK}/$file ${A2LIB}
done 
for file in ${LCLSOURCES}
do 
  cp ${A2SRC}/$file ${A2LIB}
done 

cd ${A2LIB}
gcc -fPIC -DLinux  -g -Wall -c *.c
gcc -shared -Wl,-soname,libVgfXml.so -o libVgfXml.so *.o -lc
mv libVgfXml.so ${OS_LIB}

cd ..
rm -rf ${A2LIB}
