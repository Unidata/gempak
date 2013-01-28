#!/bin/csh -f


set MODEL=eta
set DEVICE=xw

gptraj << EOF
!
! setup display
 DEVICE   = ${DEVICE}
 CLEAR    = YES
 MAP      = 1
 GAREA    = uslcc
 PROJ     = lcc/25;-95;45
 LATLON   = 0
 PANEL    = 0
 TITLE    = 1/-1/Backwards Trajectories ~
 TEXT     = .9/22/1/hw
 GDFILE   = $MODEL
 GVECT    = wnd
 GLEVEL   = 10
 GVCORD   = hght
 GDATTIM  = fall
 MARKER   = 6/15/3/1
 LINE     = 5
 RTRAJ    = yes
 TSTEP    = 30
 GPOINT   = 25/50/5;-125/-75/5
 run

 exit
EOF
