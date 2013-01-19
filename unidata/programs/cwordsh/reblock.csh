#!/bin/csh -f

if ( -e 20070208.snd ) rm 20070208.snd
if ( -e 20070208.sfc ) rm 20070208.sfc
if ( -e 20070208.sfc_aux ) rm 20070208.sfc_aux

if ( -e bufr.725650.2007020812.unblock ) rm bufr.725650.2007020812.unblock
if ( -e bufr.725650.2007020812.reblock ) rm bufr.725650.2007020812.reblock


cwordsh << EOF_UNBLOCK
unblk
bufr.725650.2007020812.bige
bufr.725650.2007020812.unblock
EOF_UNBLOCK

cwordsh << EOF_REBLOCK
block
bufr.725650.2007020812.unblock
bufr.725650.2007020812.reblock
EOF_REBLOCK


namsnd << EOF_NAMSND
SNBUFR   = bufr.725650.2007020812.reblock
SNOUTF   = 20070208.snd
SFOUTF   = 20070208.sfc+
SNPRMF   = snclass1.prm
SFPRMF   = sfclass1.prm
TIMSTN   = 49/500
r

e
EOF_NAMSND
