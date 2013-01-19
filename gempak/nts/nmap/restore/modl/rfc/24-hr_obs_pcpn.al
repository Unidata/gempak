!
! Restore File : 24-hr_obs_pcpn.al
!
! Log:
! D.W.Plummer/NCEP	 2/97	Initial creation
! D.W.Plummer/NCEP	 8/97	Added FILTER parameter and changed TEXT value
! S. Jacobs/NCEP	 4/99	Changed the value of GDATTIM
!
GDATTIM allA000
GLEVEL  0
GVCORD  none
PANEL   0                                                                       
SKIP    0
SCALE   0
GDPFUN  p24i
TYPE    f
CONTUR  1                                                                       
CINT    .25/4/
LINE    2//1/0
FINT    .01;.1;.25;.5;.75;1;1.25;1.5;1.75;2;2.25;2.5;2.75;3;3.25;3.5;3.75;4
FLINE   0;21-30;14-20;5
HILO    31;0/x#2////y
HLSYM   1.5
CLRBAR  1
WIND    0
REFVEC                                                                          
TITLE   1/-1/~ 24-HR OBS PRECIP (RFC, 40km avg) !0
TEXT    1/22/2/hw
CLEAR   yes                                                                     
STNPLT                                                                          
SATFIL                                                                          
RADFIL                                                                          
STREAM                                                                          
POSN    4                                                                       
COLORS  2                                                                       
MARKER  2                                                                       
GRDLBL  5                                                                       
LUTFIL  none
FILTER  yes
