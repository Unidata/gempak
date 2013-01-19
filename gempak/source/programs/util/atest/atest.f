	PROGRAM ATEST 
C************************************************************************
C* ATEST								*
C*									*
C* This program allows the user to call GEMPLT subroutines		*
C* individually.							*
C*									*
C* It prompts for a subroutine name and after the subroutine has been	*
C* selected it prompts for parameters.					*
C**									*
C* Log:									*
C* G.Chatters/RDS	7/84						*
C* I. Graffman/RDS	4/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	6/88	Clean up for GEMPAK4			*
C* S. Schotz/GSC	1/90    Added parameters for gsarrw, gqarrw,	*
C*                              gsbarb, gqbarb, gstext, gqtext, gsmrkr,	*
C*                              gqmrkr routines				*
C* S. Schotz/GSC	3/90	Added cloud/weather symbol routines	*
C* S. Schotz/GSC	8/90	Added arrow head size			*
C* S. Schotz/GSC	8/90	Update for GQLPAT change		*
C* M. desJardins/GSFC	8/91	Calling sequence change to GDRGRD	*
C* M. desJardins/GSFC	10/91	Calling sequence change: GD_OPNF,GCONTR	*
C* M. desJardins/GSFC	10/91	Fixed call to GSKY			*
C* M. desJardins/GSFC	11/91	Added call to GFILL			*
C* M. desJardins/GSFC	11/91	Added new contouring capabilities	*
C* S. Jacobs/EAI	 6/93	Added GGTPNT				*
C* A. Chang/EAI		12/93	Added   GSCTBL, GSFLNM, GSPLOT, GSPIXM, *
C*				GEPIXM, GLOOPC, GSHCAT, GSATIM, GSATMC	*
C* S. Jacobs/NMC         3/94   Renamed: GSPIXM ==> GSTANM              *
C*                                       GEPIXM ==> GENANM              *
C*                                       GSHCAT ==> GSDATT              *
C* S. Jacobs/NMC	 6/94	Added GMESG				*
C* S. Jacobs/NMC	 7/94	Removed GSATAO				*
C* A. Chang/EAI		 8/94	Added GTEXTC				*
C* S. Jacobs/NMC	12/94	Updated call to GGTPNT			*
C* S. Jacobs/NMC	 1/95	Added GSLUTF				*
C* G. Krueger/EAI	12/95	Removed HLS;Added XNAME;Mod. RGB range	*
C*				Added GSBRGB				*
C* J. Cowie/COMET	11/95	Added GSCOLB, GQCLRS, removed GSLUTF	*
C* M. Linda/GSC		12/95	Removed GCLDHT, GSATNP, and GSATMC	*
C* S. Jacobs/NCEP	 3/96	Added GSDEVA, modified GSDATT		*
C* L. Williams/EAI	 3/96	Increased color to 40 characters	*
C* S. Jacobs/NCEP	 4/96	Added iunit to GSDATT			*
C* S. Jacobs/NCEP	 5/96	Added GSLWIN and GQDATT			*
C* S. Jacobs/NCEP	 5/96	Removed GSDEV and GSFLNM		*
C* K. Brill/EMC		 5/96	Added more FORMATS			*
C* M. Linda/GSC		 6/96	Increased MAXP from 50 to 3*LLMXPT+10	*
C* S. Jacobs/NCEP        6/96   Fixed calling sequence to GD_OPNF       *
C* M. Linda/GSC		 8/96	Added xICNG, xSPCL, and xTURB		*
C* E. Wehner/EAi	10/96	Added xFRNT				*
C* S. Maxwell/GSC	11/96	Removed save and restore functions	*
C* E. Wehner/EAi	11/96	Removed paramters for xFRNT		*
C* K. Tyle/GSC           2/97   Added GCLPNL                            *
C* M. Linda/GSC		 2/97	Removed GFLUSH				*
C* D. Keiser/GSC	 3/97	Added xSPLN				*
C* E. Safford/GSC	 4/97	Added code for GTXSY			*
C* E. Safford/GSC        6/97   Added xTXSY               		*
C* C. Lin/EAI            6/97   Added code for GSROAM, GROAM            *
C*				Added 'S' into coordinate system	*
C* S. Maxwell/GSC        6/97   Added GSFILL and GQFILL                 *
C* S. Maxwell/GSC        7/97   Added GSGRP and GEGRP                   *
C* E. Safford/GSC	 7/97   Modified added more params to GTXSY     *
C* G. Krueger/EAI	 7/97	Added GETMAP				*
C* M. Linda/GSC		 7/97	Added GLOGO				*
C* S. Jacobs/NCEP	 9/97	Removed GSTXSY and GQTXSY		*
C* S. Jacobs/NCEP	 9/97	Changed call to GSTEXT and GQTEXT	*
C* S. Jacobs/NCEP	11/97	Changed FORMAT for GETMAP: I -> I9	*
C* I. Durham/GSC	12/97	Added GQSIZD				*
C* S. Jacobs/NCEP	 2/98	Added GSSMTH, and GQSMTH		*
C* S. Jacobs/NCEP	 3/98	Added GSDASH, and GQDASH		*
C* I. Durham/GSC	 3/98	Added GDARR, GSDARR, and GQDARR		*
C* I. Durham/GSC	 3/98	Added GHASH, GSHASH, and GQHASH		*
C* I. Durham/GSC	 3/98	Added GSCLR2 and GQCLR2			*
C* S. Jacobs/NCEP	 6/98	Changed GQ/GSFRNT to use REAL size	*
C* S. Jacobs/NCEP	 6/98	Removed NP from call to GGTPNT		*
C* S. Jacobs/NCEP	 6/98	Removed GSDATT and GOUTP		*
C* S. Jacobs/NCEP	 6/98	Moved GSCTBL from CONTROL to COLORS	*
C* T. Lee/GSC		 7/98	Added GQCVSC				*
C* S. Jacobs/NCEP	 7/98	Changed call to GTEXTC			*
C* A. Hardy/GSC		10/98	Added GCMBO, GSCMBO and GQCMBO	        *
C* A. Hardy/GSC		11/98	Changed calling sequence for GCIRCL     *
C* A. Hardy/GSC		12/98	Hard coded np for GCIRCL                *
C* S. Jacobs/NCEP	 1/99	Changed range of values for GTXSY	*
C* S. Jacobs/NCEP	 5/99	Added GSRDUC and GQRDUC			*
C* M. Li/GSC		 1/00	Added GCNTLN, GCNTBX and GCNTFL		*
C* A. Hardy/GSC		 5/00   Added logo color mode to GLOGO		*
C* A. Hardy/GSC		 6/00	Added GARC				*
C* A. Hardy/GSC		12/00	Changed calling sequence to GDRGRD	*
C* J. Wu/GSC		 3/01   Added logo emblem ID 			*
C* M. Li/GSC		 4/01	Added GFLBND				*
C* M. Li/SAIC		11/01	Added ICNG				*
C* T. Lee/SAIC		11/01	Added fill types to GCFILL calling seq.	*
C* D. Kidwell/NCEP	 6/02	Added GSGTGN                            *
C* D.W.Plummer/NCEP	 9/02	Replace GFLBND with GPLBND		*
C* S. Jacobs/NCEP	 2/03	Allow values of 1-15 for GTXSY type	*
C* S. Jacobs/NCEP	 2/03	Increased string size for GTXSY		*
C* C. Bailey/HPC	 8/06	Added clbl to GCLGRN and GCNTLN 	*
C*				calling seq.				*
C* S. Gilbert/NCEP	 5/07	Removed GCNTLN, GCNTFL, GCNTBX, GCSPLN	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	 ( MAXP = ( 3 * LLMXPT + 10 ) )
	PARAMETER	 ( MAPP = IGBSIZ / 2 - ( MAXP + 6 ) )
	CHARACTER	proj*4, sysin*1, sysout*1, cmd*6, device*12
	CHARACTER	chars*80, file*80, color*40, symtyp*8
	CHARACTER	navtyp*8, time (2)*20, parm*12, ans*1
	CHARACTER	ctblnm*64, filnam*72, satfil*132, messag*80
	CHARACTER	xname*40, wname*72, tchars*160, clbl(50)*24
	INTEGER		lintyp (50), linlbl (50), icolr (50), lpat (8)
	INTEGER		linwid (50), level (2), ihdr (128), lblfrq (2)
	INTEGER		ixof (MAXP), iyof(MAXP), ielts (MAXP)
	INTEGER		icolrs(256), ireds(256), igrns(256), iblus(256)
	INTEGER		icomm, igroup, filcol, filtyp
	INTEGER		symcol, symnum, symwid
	REAL		axary (MAXP), xinp (MAXP), yinp (MAXP), clvl(50)
	REAL		xoutp (MAXP), youtp (MAXP), x (MAXP), y (MAXP)
	REAL		xeval (MAXP), yeval (MAXP)
	REAL		xlats (MAPP), ylons (MAPP)
	REAL		spd (10), dir (10), xla (10), yla (10), aloc(2)
	REAL		anlblk (128), rnvblk (256)
	REAL		grid (LLMXGD), uw (LLMXGD), vw (LLMXGD), 
     +			cod(MAXP), subgrd (LLMXGD)
        REAL            endpts(4)
	LOGICAL		pntvis ( MAXP ), misflg, laxis, grdflg, done
C------------------------------------------------------------------------
	CALL IN_BDTA (iret)
	grdflg = .false.
	done   = .false.
C*
	WRITE (6,*) 'Enter HELP for a list of subroutines and commands.' 
C*
10	CONTINUE
	CALL TM_WLIN  ( 'GPLT>', .false., ier )
	READ   ( 5, 20, IOSTAT = iostat )  cmd
20	FORMAT ( A )
	CALL ST_LCUC ( cmd, cmd, ier )
	IF ( cmd .EQ. 'HELP' ) THEN
	    WRITE ( 6, 25 )
25	    FORMAT (' Available commands:   HELP    EXIT' / )
            WRITE ( 6, 30 )
30	    FORMAT (' Available subroutines:'//,
     +         ' Access   : GINITP  GENDP   GSMODE  GQMODE'/,
     +         ' Control  : GCLEAR  GCLOSP'/,
     +         '            GQDEV   GEPLOT  GGTPNT'/,
     +         '            GSPLOT  GSTANM  GENANM  GLOOPC  GMESG'/,
     +         '            GSDEVA  GSLWIN  GCLPNL  GSROAM  GROAM'/,
     +         '            GSGRP   GEGRP   GSGTGN'/,
     +         ' Bounds   : GSVIEW  GQVIEW  GQBND'/,
     +         ' Map      : GSMMAP  GSMPRJ  GSMMGN  '/,
     +         '            GQMMAP  GQMPRJ  GQMMGN'/,
     +         ' Satellite: GQSATN  GSATIM'/, 
     +         ' Graph    : GSGRAF  GSGMGN  GDAXIS  GAAXIS'/,
     +         '            GQGRAF  GQGMGN  '/,
     +         ' Grid     : GSGMAP  GSGPRJ  GSGGRF  GQGMAP  GQGPRJ'/,
     +         '            GQGGRF  ' )
	    WRITE ( 6, 31 )
31	    FORMAT (
     +         ' Utility  : GCONTR  GSTRML  GSMFIL  GDRGRD'/,
     +         '            GDRMAP  GETMAP  GQMFIL  GPLBND'/,
     +         ' Plot     : GLINE   GTEXT   GMARK   GBARB   GARRW'/,
     +         '            GCIRCL  GSKY    GWTHR   GPTND   GPWTH'/,
     +         '            GCTYP   GFILL   GTEXTC  GICNG   GSPCL'/,
     +         '            GTURB   GFRNT   GSPLN   GTXSY   GLOGO'/,
     +         '            GDARR   GHASH   GCMBO   GARC'/,
     +         ' Transform: GTRANS  GPTVIS'/,
     +         ' Attribute: GSLINE  GSTEXT  GSBARB  GSARRW  GSMRKR'/,
     +         '            GSLPAT  GSSKY   GSWTHR  GSPTND  GSPWTH'/,  
     +         '            GSCTYP  GQLINE  GQTEXT  GQBARB  GQARRW'/,
     +         '            GQMRKR  GQLPAT  GQSYSZ  GSTICK  GQSKY '/,
     +         '            GQWTHR  GQPTND  GQPWTH  GQCTYP  GSCNTR'/,
     +         '            GQCNTR  GQDATT  GSICNG  GQICNG'/,
     +         '            GSSPCL  GQSPCL  GSTURB  GQTURB  GSFRNT'/,
     +         '            GQFRNT  GSSPLN  GQSPLN  GSFILL  GQFILL'/,
     +         '            GSSMTH  GQSMTH  GSDASH  GQDASH  GSDARR'/,
     +         '            GQDARR  GSHASH  GQHASH  GQCVSC  GSCMBO'/,
     +         '            GQCMBO  GSRDUC  GQRDUC'/,
     +         ' Color    : GSCOLR  GSCINT  GSCNAM  GSBRGB  GSCRGB'/,
     +         '            GQCOLR  GQCOMP  GQNCOL  GSCOLB  GQCLRS'/,
     +         '            GSCLR2  GQCLR2  GSCTBL'/,
     +         ' Curve    : GCYEVL' )
	    WRITE ( 6, 32 )
32	    FORMAT (
     +         ' Contour:   GCLGRN  GCFILL  GCBOXX'/
     +         ' Read grid: GETGRD' )
C------------------------------------------------------------------------
C*	Access
C
	  ELSE IF ( cmd .eq. 'GINITP' ) THEN
	    WRITE ( 6, *) 'GINITP - Enter mode: '
	    READ  ( 5, *, IOSTAT = iostat )  mode
	    CALL GINITP ( mode, istat, iret )
	    WRITE ( 6,* ) 'GINITP - istat, iret ', istat, iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .EQ. 'GENDP' ) THEN
	    WRITE ( 6, *) 'GENDP - Enter ieop: '
	    READ  ( 5, *, IOSTAT = iostat ) ieop
	    CALL GENDP ( ieop, iret )
	    WRITE ( 6,* ) ' GENDP - iret ', iret 
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
	    STOP
C
	  ELSE IF ( cmd .EQ. 'GSMODE' ) THEN
	    WRITE ( 6, * ) ' GSMODE - Enter mode: ' 
	    READ  ( 5, *, IOSTAT = iostat )  mode
	    CALL GSMODE ( mode, iret )
	    WRITE ( 6,* ) ' GSMODE - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .EQ. 'GQMODE' ) THEN
	    CALL GQMODE ( mode, iret )
	    WRITE( 6, * ) ' GQMODE - mode, iret', mode, iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C------------------------------------------------------------------------
C*	Control
C
	  ELSE IF ( cmd .eq. 'GCLEAR' ) THEN
	    CALL GCLEAR ( iret )
	    WRITE (6,*) 'GCLEAR  iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GEPLOT' ) THEN
	    CALL GEPLOT ( iret )
	    WRITE (6,*) 'GEPLOT  iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GCLOSP' ) THEN
	    CALL GCLOSP ( iret )
	    WRITE( 6, * ) ' GCLOSP   iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQDEV' ) THEN
	    CALL GQDEV ( device, iunit, iatyp, iret )
	    WRITE ( 6, * ) ' GQDEV--device, iunit, iatyp, iret ',
     +			   device, iunit, iatyp, iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GGTPNT' ) THEN
	    CALL GQDEV ( device, iunit, iatyp, iret )
	    IF  ( device .eq. 'xw' .or. device .eq. 'XW' )  THEN
		WRITE (6,*)
     +		    'GGTPNT-Enter coordinate system ( S,D,N,V,P,M,G )'
		READ  (5,20) sysin
		WRITE (6,*)
     +		    ' Enter the point type (1=point,2=line,3=box): '
		READ  (5,*) ityp
		CALL GGTPNT ( sysin, ityp, x, y, iret )
		WRITE ( 6, * ) ' GGTPNT--sysin, ityp, iret ',
     +			       sysin, ityp, iret
		WRITE ( 6, * ) ' GGTPNT--The points are:'
C
		IF  ( ityp .eq. 1 )  THEN
		    np = 1
		  ELSE
		    np = 2
		END IF
C
		DO  icnt = 1, np
		    WRITE ( 6, 101 ) x(icnt), y(icnt)
101		    FORMAT ( 5X, 2F10.3 )
		END DO
	    END IF
C
	  ELSE IF ( cmd .eq. 'GSPLOT' ) THEN
	    CALL GSPLOT ( iret )
	    WRITE (6,*) 'GSPLOT  iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSTANM' ) THEN
	    CALL GSTANM ( iret )
	    WRITE (6,*) 'GSTANM  iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GENANM' ) THEN
	    CALL GENANM ( iret )
	    WRITE (6,*) 'GENANM  iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GLOOPC' ) THEN
	    WRITE ( 6, '(A,$)') ' GLOOPC - Enter icomm : ' 
	    READ  (5, *, IOSTAT = iostat )  icomm
	    CALL GLOOPC ( icomm, iret )
	    WRITE (6,*) 'GLOOPC  iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSGRP' ) THEN
	    WRITE ( 6, '(A,$)') ' GSGRP - Enter igroup : '
	    READ  (5, *, IOSTAT = iostat )  igroup
	    CALL GSGRP ( igroup, iret )
	    WRITE (6,*) 'GSGRP  iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GEGRP' ) THEN
	    CALL GEGRP ( iret )
	    WRITE (6,*) 'GEGRP  iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GMESG' ) THEN
	    WRITE ( 6, '(A,$)') ' GMESG - Enter messag : ' 
	    READ  (5,20) messag
	    CALL GMESG ( messag, iret )
	    WRITE (6,*) 'GMESG  iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSDEVA' ) THEN
	    WRITE ( 6, *) ' GSDEVA - Enter device: ' 
	    READ  (5, 20, IOSTAT = iostat )  device
	    WRITE (6,*) 'Enter iunit'
	    READ  (5,*)  iunit
	    WRITE (6,*) 'Enter filnam'
	    READ  (5,20)  filnam
	    WRITE (6,*) 'Enter itype'
	    READ  (5,*)  itype
	    WRITE (6,*) 'Enter xsize'
	    READ  (5,*)  xsize
	    WRITE (6,*) 'Enter ysize'
	    READ  (5,*)  ysize
	    CALL GSDEVA ( device, iunit, filnam, itype,
     +			  xsize, ysize, iret )
	    WRITE ( 6, * ) ' GSDEVA - iret ', iret 
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSLWIN' ) THEN
	    WRITE ( 6, '(A,$)') ' GSLWIN - Enter window name : ' 
	    READ  (5, 20, IOSTAT = iostat )  wname
	    CALL GSLWIN ( wname, iret )
	    WRITE (6,*) 'GSLWIN  iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GCLPNL' ) THEN
	    WRITE ( 6, *) 'GCLPNL - Enter XLLF, YLLF, XURF, YURF'
	    READ  (5, *, IOSTAT = iostat )  xlf, ybf, xrf, ytf
	    CALL GCLPNL ( xlf, ybf, xrf, ytf, iret )
	    WRITE ( 6, * ) ' GCLPNL - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSROAM' ) THEN
	    WRITE ( 6, *) 'GSROAM - Enter IRMFLG, IPWDTH, IPHGHT'
	    READ  (5, *)  irmflg, ipwdth, iphght
	    CALL GSROAM ( irmflg, ipwdth, iphght, iret)
	    WRITE ( 6, * ) ' GSROAM - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GROAM' ) THEN
	    WRITE(6, *) 'GROAM - Enter type (0=UL, 1=C, 2=delta)'
	    READ  (5, *) ityp 
	    WRITE(6, *)
     +		'GROAM - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE ( 6, *) 'GROAM - Enter X, Y'
	    READ  (5, *, IOSTAT = iostat )  xrm, yrm
	    CALL GROAM ( ityp, sysin, xrm, yrm, iret )
	    WRITE ( 6, * ) ' GROAM - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
	    CALL GEPLOT( ier )
C
	  ELSE IF ( cmd .eq. 'GSGTGN' ) THEN
	    WRITE ( 6, *) 'GSGTGN - Enter IGTYP, IGNUM'
	    READ  (5, *)  igtyp, ignum
	    CALL GSGTGN ( igtyp, ignum, iret)
	    WRITE ( 6, * ) ' GSGTGN - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
C------------------------------------------------------------------------
C*	Bounds
C
	  ELSE IF ( cmd .eq. 'GSVIEW' ) THEN
	    WRITE ( 6, *) 'GSVIEW - Enter XLLF, YLLF, XURF, YURF'
	    READ  (5, *, IOSTAT = iostat )  xlf, ybf, xrf, ytf
	    CALL GSVIEW ( xlf, ybf, xrf, ytf, iret )
	    WRITE ( 6, * ) ' GSVIEW - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQVIEW' ) THEN
	    CALL GQVIEW ( xl, yb, xr, yt, iret )
	    WRITE (6, 102) 'GQVIEW - iret, xl, yb, xr, yt', 
     +                    iret, xl, yb, xr, yt
102	    FORMAT ( 5X, A, I5, 4F10.3 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQBND' ) THEN
	    WRITE (6,*)
     +		'GQBND - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5, 20, IOSTAT = iostat ) sysin
	    CALL GQBND ( sysin, xl, yb, xr, yt, iret )
	    WRITE ( 6, 102 ) ' GQBND - iret, xl, yb, xr, yt',
     +			     iret, xl, yb, xr, yt
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
          ELSE IF ( cmd .eq. 'GPLBND' ) THEN
	    WRITE (6,*) 'Enter bounds type (eg., STATE_BNDS) '//
     +' including any tag (eg., STATE_BNDS|<STATE>VA) : '
	    READ  (5,20) chars
            CALL ST_LSTR ( chars, lens, iret )
	    WRITE (6,*) 'For the fill:'
	    WRITE (6,*) 'Enter the fill color '
	    READ  (5,*) filcol
	    WRITE (6,*) 'Enter the fill size '
	    READ  (5,*) filsiz
	    WRITE (6,*) 'Enter the fill type '
	    READ  (5,*) filtyp
	    WRITE (6,*) 'Enter the filter ( 0 ~ 1 )'
	    READ  (5,*) filter
	    WRITE (6,*) 'Enter the minimum points:'
	    READ  (5,*) minpts
	    WRITE (6,*) 'For the outline:'
	    WRITE (6,*) 'Enter the line color '
            READ  (5,*) lincol 
	    WRITE (6,*) 'Enter the line type '
            READ  (5,*) lintyp (1)
	    WRITE (6,*) 'Enter the line width '
            READ  (5,*) linwid (1)
	    WRITE (6,*) 'For the centroid plotting:'
	    WRITE (6,*) 'Enter either "mark" or "wthr" '
            READ  (5,20) symtyp 
            CALL ST_LSTR ( symtyp, lenst, iret )
	    WRITE (6,*) 'Enter the marker/symbol color '
            READ  (5,*) symcol 
	    WRITE (6,*) 'Enter the marker/symbol number '
            READ  (5,*) symnum 
	    WRITE (6,*) 'Enter the marker/symbol size '
            READ  (5,*) symsiz 
	    WRITE (6,*) 'Enter the marker/symbol width '
            READ  (5,*) symwid 
            CALL GPLBND ( chars(1:lens), filcol, filsiz, filtyp, filter, 
     +                    minp, lincol, lintyp(1), linwid(1),
     +                    symtyp(1:lenst), symcol, symnum, symsiz, 
     +                    symwid, iret )
	    CALL GEPLOT ( ier )
            chars= ' '
            WRITE (6,*) ' GPLBND iret = ', iret
            CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C------------------------------------------------------------------------
C*	Map
C
	  ELSE IF ( cmd .eq. 'GSMMAP' ) THEN
	    WRITE ( 6, *) 'GSMMAP - Enter PROJ: '
	    READ  ( 5, 20, IOSTAT = iostat )  proj
	    WRITE ( 6, *) ' Enter DLATLL, DLONLL, DLATUR, DLONUR'
	    READ  ( 5, *, IOSTAT = iostat ) dlats, dlonw, dlatn, dlone
	    CALL GSMMAP ( proj, dlats, dlonw, dlatn, dlone, iret )
	    WRITE ( 6, * ) ' GSMMAP - iret ', iret 
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSMPRJ' ) THEN
	    WRITE (6, *) 'GSMPRJ - Enter PROJ'
	    READ  ( 5, 20, IOSTAT = iostat ) proj
	    WRITE ( 6, * ) ' Enter projection angles 1, 2, and 3'
	    READ  ( 5, *, IOSTAT = iostat ) angle1, angle2, angle3
	    WRITE ( 6, *) ' Enter DLATLL, DLONLL, DLATUR, DLONUR'
	    READ  ( 5, *, IOSTAT = iostat ) dlats, dlonw, dlatn, dlone
	    CALL GSMPRJ ( proj, angle1, angle2, angle3,
     +			  dlats, dlonw, dlatn, dlone, iret )
	    WRITE ( 6, * ) ' GSMPRJ - iret ', iret 
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSMMGN' ) THEN
	    WRITE ( 6, *) 'GSMMGN - Enter XL, YB, XR, YT'
	    READ  ( 5, *, IOSTAT = iostat ) xl, yb, xr, yt
	    CALL GSMMGN ( xl, yb, xr, yt, iret )
	    WRITE ( 6, * ) ' GSMMGN iret', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSMFIL' ) THEN
	    WRITE ( 6,*) ' GSMFIL - Enter map file name '
	    READ  ( 5, 20, IOSTAT = iostat ) file
	    CALL GSMFIL ( file, iret )
	    WRITE ( 6, * ) ' GSMFIL iret', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GDRGRD' ) THEN
	    WRITE (6, *) 'Enter lat/lon interval, lat/lon frequency'
	    READ (5,*,IOSTAT=iostat) dellat,dellon,lblfrq(1),lblfrq(2)
	    WRITE (6, *) 'Enter lat/lon label placements'
	    READ (5,*,IOSTAT=iostat) aloc(1), aloc(2)
	    WRITE (6, *) 'Enter lat/lon label format (1 or 2)'
	    READ (5,*,IOSTAT=iostat) ifrmat
	    CALL GDRGRD ( dellat, dellon, lblfrq, aloc (1), aloc(2),
     +                    ifrmat, iret )
	    CALL GEPLOT ( ier )
	    WRITE (6,*) ' GDRGRD iret', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GDRMAP' ) THEN
	    CALL GDRMAP ( iret )
	    CALL GEPLOT ( ier )
	    WRITE (6,*) 'GDRMAP - iret', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GETMAP' ) THEN
	    WRITE ( 6, * ) 'GETMAP - Enter MAPTYP (0-line, 1-polygon)'
	    READ  ( 5, *, IOSTAT = iostat ) maptyp
	    CALL GETMAP ( maptyp, MAXP, MAPP / 2,
     +                    nelts, np, ielts, xlats, ylons, iret )
	    IF ( np .ge. 1 .and. iret .eq. 0 ) THEN
		ie = 1
		DO WHILE ( ie .ge. 1 .and. ie .le. nelts )
		    ihipt = ielts (ie+1) - 1
		    IF ( ie .eq. nelts ) ihipt = np
		    WRITE ( 6, 1010 ) ( xlats(ip), ylons(ip),
     +					ip = ielts (ie), ihipt )
		    WRITE ( 6, 1012 ) 'Select a map element from 1 - ',
     +				      nelts, ' (or 0 to stop):'
		    READ ( 5, *, IOSTAT = iostat ) ie
		END DO
	    END IF
1010        FORMAT ( 5X, 2F10.3 )
1012        FORMAT ( 5X, A, I9, A )
	    WRITE (6,*) 'GETMAP - iret', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQMMAP' ) THEN
	    CALL GQMMAP ( proj, dlats, dlonw, dlatn, dlone, iret )
	    WRITE (6,*)'GQMMAP - proj, dlats, dlonw, dlatn, dlone, iret'
	    WRITE (6,1021) proj, dlats, dlonw, dlatn, dlone, iret
1021	    FORMAT ( 5X, A, 4F9.2, I7 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	 ELSE IF ( cmd .eq. 'GQMPRJ' ) THEN
	    CALL GQMPRJ ( proj, angl1, angl2, angl3, dlats,
     +                    dlonw, dlatn, dlate, iret )
	    WRITE (6,*) ' GQMPRJ - proj, angl1, angl2, angl3, ',
     +                   'dlats, dlonw, dlatn, dlate, iret'
	    WRITE (6,1022) proj, angl1, angl2, angl3, dlats, 
     +                  dlonw, dlatn, dlate, iret
1022	    FORMAT ( 5X, A, 7F9.2, I7 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQMMGN' ) THEN
	    CALL GQMMGN ( xl, yb, xt, yr, iret )
	    WRITE (6,107)' GQMMGN - xl, yb, xt, yr, iret', xl, yb, xt, 
     +                   yr, iret
107	    FORMAT ( 5X, A, 4F8.3, I7 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQMFIL' ) THEN
	    CALL GQMFIL ( file, iret )
	    WRITE (6,*)' GQMFIL - iret, file', file, iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C------------------------------------------------------------------------
C*	Satellite
C
	  ELSE IF ( cmd .eq. 'GQSATN' ) THEN
	    navtyp = ' '
	    file   = ' '
	    CALL GQSATN ( navtyp, file, iret )
	    WRITE (6,*) 'iret, navtyp ', iret, navtyp
	    WRITE (6,*) 'image name ', file
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSATIM' ) THEN
	    WRITE ( 6, '(A,$)') ' GSATIM - Enter satellite filename : ' 
	    READ  (5, 20, IOSTAT = iostat )  satfil
	    CALL GSATIM ( satfil, iret )
	    WRITE (6,*) 'GSATIM  iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C------------------------------------------------------------------------
C*	Graph
C
	  ELSE IF ( cmd .eq. 'GSGRAF' ) THEN
	    WRITE ( 6,*) 'GSGRAF - Enter IXTYP, IYTYP:'
	    READ  (5,*)   ixty, iyty
	    WRITE (6,*) ' Enter height to width ratio: '
	    READ  (5,*)  yszxsz
	    WRITE (6,*) ' Enter XL, YB, XR, YT'
	    READ  (5,*)  xl, yb, xr, yt
	    CALL GSGRAF ( ixty, iyty, yszxsz, xl, yb, xr, yt, iret )
	    WRITE (6,*) ' GSGRAF - iret', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSGMGN' ) THEN
	    WRITE (6,*) 'GSGMGN - Enter XL, YB, XR, YT'
	    READ  (5,*)  xl, yb, xr, yt
	    CALL GSGMGN ( xl, yb, xr, yt, iret )
	    WRITE (6,*) 'GSGMGN - iret', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	 ELSE IF ( cmd .eq. 'GDAXIS' ) THEN
	    WRITE (6,*) 'Enter IAXIS, AXPOS, LAXIS, LBSTFQ, MTSTFQ,',
     +			 ' LGSTFQ, NDEC, NP'
	    READ (5,*) iaxis, axpos, laxis, lbstfq, mtstfq, lgstfq,
     *                 ndec, np
	    WRITE ( 6, * ) 'Enter ', np, ' values for axary' 
	    READ (5,*) ( axary (i), i=1,np)
	    CALL GDAXIS ( iaxis, axpos, laxis, lbstfq, mtstfq, lgstfq,
     *                    ndec, np, axary, iret )
	    WRITE ( 6,*) ' GDAXIS - iret', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQGRAF' ) THEN
	    CALL GQGRAF ( ixty, iyty, yszxsz, xl, yb, xr, yt, iret )
	    WRITE (6,104) 'GQGRAF - iret,ixtyp,iytyp,yszxyz,xl,yb,xr,yt',
     +                   iret, ixty, iyty, yszxsz, xl, yb, xr, yt
104	    FORMAT ( 5X, A, /, 5X, 3I5, 5F10.3 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQGMGN' ) THEN
	     CALL GQGMGN ( xl, yl, xr, yt, iret )
	     WRITE (6,108) ' GQGMGN - iret, xl, yl, xr, yt ', iret, 
     +		           xl, yl, xr, yt
108	    FORMAT ( 5X, A, I7, 4F8.3 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C------------------------------------------------------------------------
C*	GRID
C
	  ELSE IF ( cmd .eq. 'GSGMAP' ) THEN
	    WRITE (6, *) 'GSGMAP  - Enter PROJ'
	    READ  (5,20) proj
	    WRITE (6, *) ' Enter kx, ky'
	    READ  (5,*)  kx, ky
	    WRITE (6, *) ' Enter 4 map coords: '
	    READ  (5,*)  dlats, dlonw, dlatn, dlone
	    CALL GSGMAP ( proj, kx, ky, dlats, dlonw, dlatn, dlone, 
     +	                  iret )
	    WRITE ( 6, *) ' GSGMAP - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSGPRJ' ) THEN
	    WRITE (6, *) 'GSGPRJ - Enter projection: '
	    READ  (5,20) proj
	    WRITE (6,*) ' Enter 3 map angles: '
	    READ  (5,*) dangl1, dangl2, dangl3
	    WRITE (6, *) ' Enter kx, ky'
	    READ  (5,*) kx, ky
	    WRITE (6, *) ' Enter 4 map coords: '
	    READ  (5,*) dlats, dlonw, dlatn, dlone
	    CALL GSGPRJ ( proj, dangl1, dangl2, dangl3, kx, ky, 
     +		          dlats, dlonw, dlatn, dlone, iret )
	    WRITE ( 6, *) ' GSGPRJ - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSGGRF' ) THEN
	    WRITE (6,*) ' GSGGRF - Enter X and Y coord. types: '
	    READ  (5,*)  ixtyp, iytyp
	    WRITE (6,*) ' Enter KX, KY: '
	    READ  (5,*)  kx, ky
	    WRITE (6,*) ' Enter 4 GRAPH coords: '
	    READ  (5,*)  xl, yb, xr, yt
	    CALL GSGGRF ( ixtyp, iytyp, kx, ky, xl, yb, xr, yt, iret )
	    WRITE ( 6, *) ' GSGGRF - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQGMAP' ) THEN
	    CALL GQGMAP ( proj, kx, ky, dlats, dlonw, dlatn, 
     +	                  dlone, iret )
	    WRITE (6,*) 'GQGMAP  iret = ', iret
	    WRITE (6,*) '  proj, kx, ky: ', proj, ' ', kx, ky
	    WRITE (6,109) '  bounds: ', dlats, dlonw, dlatn, dlone
109	    FORMAT ( 5X, A, 4F8.3 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQGPRJ' ) THEN
	    CALL GQGPRJ ( proj, dangl1, dangl2, dangl3, kx, ky,
     +		          dlats, dlonw, dlatn, dlone, iret )
	    WRITE (6,*) 'GQGPRJ  iret = ', iret
	    WRITE (6,*) '  proj, kx, ky: ', proj, ' ', kx, ky
	    WRITE (6,109) '  angles: ', dangl1, dangl2, dangl3
	    WRITE (6,109) '  bounds: ', dlats, dlonw, dlatn, dlone
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQGGRF' ) THEN
	    CALL GQGGRF ( ixtyp, iytyp, kx, ky, xl, yb, xr, yt, iret )
	    WRITE (6,*) ' GQGGRF - iret, ixtyp, iytyp, kx, ky', 
     +	                  iret, ixtyp, iytyp, kx, ky
	    WRITE (6,109)' Graph coords: ', xl, yb, xr, yt
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GCLGRN' ) THEN
	    IF  ( .not. grdflg )  THEN
		WRITE (6,*) 'Call GETGRD first'
	    ELSE
	      WRITE (6,*) 'Enter NLVL'
	      READ  (5,*)  nlvl
	      WRITE (6,*) 'Enter CLVL '
	      READ  (5,*)  (clvl(i), i=1,nlvl)
	      WRITE (6,*) 'Enter CLBL '
	      READ  (5,*)  (clbl(i), i=1,nlvl)
	      WRITE (6,*) 'Enter ICOLR'
	      READ  (5,*)  (icolr (i), i=1,nlvl)
	      WRITE (6,*) 'Enter LINTYP:'
	      READ  (5,*)  (lintyp (i), i=1,nlvl)
	      WRITE (6,*) 'Enter LINWID'
	      READ  (5,*)  (linwid (i),i=1,nlvl)
	      WRITE (6,*) 'Enter LINLBL:'
	      READ  (5,*)  (linlbl (i),i=1,nlvl)
	      CALL GCLGRN  ( kx, ky, subgrd, ioffx, ioffy, iskip,
     +			   nlvl, clvl, clbl, icolr, lintyp, linwid,
     +			   linlbl, iret )
	      CALL GEPLOT (ier)
	      WRITE (6,*) ' GCLGRN - iret = ', iret
	      CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
	    END IF
C
	  ELSE IF ( cmd .eq. 'GCBOXX' ) THEN
	    IF  ( .not. grdflg )  THEN
		WRITE (6,*) 'Call GETGRD first'
	    ELSE
	      WRITE (6,*) 'Enter NLVL'
	      READ  (5,*)  nlvl
	      WRITE (6,*) 'Enter CLVL '
	      READ  (5,*)  (clvl(i), i=1,nlvl)
	      WRITE (6,*) 'Enter ICOLR'
	      READ  (5,*)  (icolr (i), i=1,nlvl)
	      WRITE (6,*) 'Enter LINTYP:'
	      READ  (5,*)  (lintyp (i), i=1,nlvl)
	      WRITE (6,*) 'Enter LINWID'
	      READ  (5,*)  (linwid (i),i=1,nlvl)
	      WRITE (6,*) 'Enter LINLBL:'
	      READ  (5,*)  (linlbl (i),i=1,nlvl)
	      CALL GCBOXX  ( kx, ky, subgrd, ioffx, ioffy, iskip,
     +			   nlvl, clvl, icolr, lintyp, linwid,
     +			   linlbl, iret )
	      CALL GEPLOT (ier)
	      WRITE (6,*) ' GCBOXX - iret = ', iret
	      CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
	    END IF
C
	  ELSE IF ( cmd .eq. 'GCFILL' ) THEN
	    IF  ( .not. grdflg )  THEN
		WRITE (6,*) 'Call GETGRD first'
	    ELSE
	      WRITE (6,*) 'Enter NLVL'
	      READ  (5,*)  nlvl
	      WRITE (6,*) 'Enter CLVL '
	      READ  (5,*)  (clvl(i), i=1,nlvl)
	      WRITE (6,*) 'Enter ICOLR'
	      READ  (5,*)  (icolr (i), i=1,nlvl+1)
	      WRITE (6,*) 'Enter fill label'
	      READ  (5,*)  (linlbl (i), i=1,nlvl+1)
	      WRITE (6,*) 'Enter fill type: 1- solid, 2- slanted dash,',
     +			  ' 3- slanted line'
	      READ  (5,*)  (lintyp (i), i=1,nlvl+1)
	      CALL GCFILL  ( kx, ky, subgrd, ioffx, ioffy, iskip,
     +			   nlvl, clvl, icolr, linlbl, lintyp, iret )
	      CALL GEPLOT (ier)
	      WRITE (6,*) ' GCFILL - iret = ', iret
	      CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
	    END IF
C
	  ELSE IF ( cmd .eq. 'GSTRML' ) THEN
	    WRITE (6,*) 'Enter name of grid file: '
	    READ  (5,20) file
	    CALL GD_OPNF ( file, .false., iflno, navxz, rnvblk, 
     +			   ianlsz, anlblk, ihdrsz, maxgrd, iret )
	    IF ( IRET .NE. 0 ) THEN
		WRITE (6,*) 'Error opening grid file ', iret
	    ELSE
	      WRITE (6,*) 'Enter numbers of grids containing U wind',
     +                    ' and V wind data'
	      READ  (5,*) numu, numv
	      CALL GD_GGRD ( iflno, numu, time, level, ivcord, parm,
     +			      uw, igx, igy, ihdr, iret )
	      IF ( iret .ne. 0 ) THEN
		WRITE (6,*) 'Error reading grid = ', iret
	      ELSE
	        CALL GD_GGRD ( iflno, numv, time, level, ivcord, parm,
     +			        vw, igx, igy, ihdr, iret )
	        CALL GD_CLOS (iflno, ier )
	        IF ( iret .ne. 0 ) THEN
		  WRITE (6,*) 'Error reading grid = ', iret
		ELSE
	          WRITE (6, *) 'GSTRML - Enter number of points in X ',
     +				'and Y direction'
	          READ  (5,*) ni, nj
	          WRITE (6,*)' Enter first X and Y points in subgrid: '
	          READ  (5,*) imin, jmin
	          WRITE (6,*)' Enter last X and Y points in subgrid: '
	          READ  (5,*) imax, jmax
	          WRITE (6,*) 'Enter MISFLG'
	          READ  (5,*)  misflg
	          CALL GSTRML ( ni, nj, uw, vw, imin, jmin, 
     +			  imax, jmax, misflg, iret )
	          CALL GEPLOT (ier)
	          WRITE (6,*) ' GSTRML - iret = ', iret
	          CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
		END IF
	      END IF
	    END IF
C------------------------------------------------------------------------
C*	Plot
C
	  ELSE IF ( cmd .eq. 'GLINE' ) THEN
	    WRITE (6,*)
     +		'GLINE - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) ' Enter number of line segments to draw: '
	    READ  (5,*) nl
	    WRITE (6,*)' Enter ', nl, ' coordinate pairs '
	    READ  (5,*) ( xinp (mm), yinp (mm), mm = 1,nl )
	    CALL GLINE ( sysin, nl, xinp, yinp, iret )
	    WRITE (6,*) ' GLINE - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GFILL' ) THEN
	    WRITE (6,*)
     +		'GFILL - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6, *)' Enter number of line segments to draw: '
	    READ  (5,*) nl
	    WRITE (6,*)' Enter ', nl, ' coordinate pairs '
	    READ  (5,*)( xinp (mm), yinp (mm), mm = 1,nl )
	    CALL GFILL ( sysin, nl, xinp, yinp, iret )
	    WRITE (6,*) ' GFILL - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GTEXT' ) THEN
	    WRITE (6,*)
     +		'GTEXT - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) ' Enter the X and Y point to start text: '
	    READ  (5,*) xc, yc
	    WRITE (6,*) ' Enter the character string: '
	    READ  (5,20) chars
	    CALL ST_LSTR ( chars, lens, iret )
	    WRITE (6,*) ' Enter rotation angle, xoffset, and yoffset'
	    READ  (5,*) rot, ixoff, iyoff
	    CALL GTEXT ( sysin, xc, yc, chars(1:lens), rot, ixoff, 
     +	                 iyoff, iret )
	    chars= ' '
	    WRITE (6,*) ' GTEXT iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GTEXTC' ) THEN
	    WRITE (6,*)
     +		'GTEXTC - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) ' Enter the X and Y point to start text: '
	    READ  (5,*) xc, yc
	    WRITE (6,*) ' Enter the character string: '
	    READ  (5,20) chars
	    CALL ST_LSTR ( chars, lens, iret )
	    WRITE (6,*) ' Enter rotation angle, xoffset, and yoffset'
	    READ  (5,*) rot, ixoff, iyoff
	    CALL ST_LSTR ( chars, lens, iret )
	    CALL GTEXTC ( sysin, xc, yc, chars(1:lens), rot, ixoff, 
     +	                 iyoff, iret )
	    chars= ' '
	    WRITE (6,*) ' GTEXTC iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GTXSY' ) THEN
	    WRITE (6,*)
     +		'GTXSY - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) ' Enter special text type (1 - 15): '
	    READ  (5,*) istt
	    WRITE (6,*) ' Enter the symbol type: '
	    READ  (5,*) isym
	    WRITE (6,*) ' Enter the X and Y point to start text: '
	    READ  (5,*) xc, yc
	    WRITE (6,*) ' Enter the character string: '
	    READ  (5,20) tchars
	    CALL ST_LSTR ( tchars, lens, iret )
	    WRITE (6,*) ' Enter alignment type (-1, 0, 1): '
	    READ  (5,*) ijust
	    WRITE (6,*) ' Enter rotation angle, xoffset, and yoffset'
	    READ  (5,*) rot, ixoff, iyoff
	    CALL GTXSY ( sysin, istt, isym, ijust, ixoff, iyoff,
     +			 rot, xc, yc, tchars(1:lens), iret )
	    tchars= ' '
	    WRITE (6,*) ' GTXSY iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GMARK' ) THEN
	    WRITE (6,*)
     +		'GMARK - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) ' Enter the number of markers: '
	    READ  (5,*,IOSTAT = iostat ) np
	    WRITE (6,*) ' Enter ', np, 'points for markers'
	    READ  (5,*,IOSTAT=iostat) ( xinp (ip), yinp (ip), ip=1, np )
	    CALL GMARK ( sysin, np, xinp, yinp, iret )
	    WRITE (6,*) ' GMARK - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF  ( cmd .eq. 'GBARB' ) THEN
	    WRITE (6,*)
     +		'GBARB - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) ' Enter number of points: '
	    READ  (5,*) np
	    WRITE (6,*) 'Enter ', np, ' locations for barbs'
	    READ  (5,*) ( xla (j), yla (j), j=1,np)
	    WRITE (6,*) 'Enter ', np, 'pairs of speed and direction'
	    READ  (5,*) ( spd (m), dir (m), m=1,np)
	    CALL GBARB ( sysin, np, xla,  yla, spd, dir, iret )
	    WRITE (6,*) ' GBARB - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF  ( cmd .eq. 'GARRW' ) THEN
	    WRITE (6,*)
     +		'GARRW - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) ' Enter number of points: '
	    READ  (5,*) np
	    WRITE (6,*) 'Enter ', np, ' locations for arrows'
	    READ  (5,*) ( xla (j), yla (j), j = 1,np)
	    WRITE (6,*) 'Enter ', np, 'pairs of speed and direction'
	    READ  (5,*) ( spd (m), dir (m), m = 1,np)
	    CALL GARRW ( sysin, np, xla, yla, spd, dir, iret )
	    WRITE (6,*) ' GARRW - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF  ( cmd .eq. 'GDARR' ) THEN
	    WRITE (6,*)
     +		'GDARR - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) ' Enter number of points: '
	    READ  (5,*) np
	    WRITE (6,*) 'Enter ', np, ' locations for arrows'
	    READ  (5,*) ( xla (j), yla (j), j = 1,np)
	    WRITE (6,*) 'Enter ', np, ' directions'
	    READ  (5,*) ( dir (m), m = 1,np)
	    CALL GDARR ( sysin, np, xla, yla, dir, iret )
	    WRITE (6,*) ' GDARR - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF  ( cmd .eq. 'GHASH' ) THEN
	    WRITE (6,*)
     +		'GHASH - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) ' Enter number of points: '
	    READ  (5,*) np
	    WRITE (6,*) 'Enter ', np, ' locations for hash marks'
	    READ  (5,*) ( xla (j), yla (j), j = 1,np)
	    WRITE (6,*) 'Enter ', np, ' directions'
	    READ  (5,*) ( dir (m), m = 1,np)
	    CALL GHASH ( sysin, np, xla, yla, dir, iret )
	    WRITE (6,*) ' GHASH - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF  ( cmd .eq. 'GCIRCL' ) THEN
	    WRITE (6,*)
     +		'GCIRCL - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*)'Enter x, y coordinates: '
	    READ  (5,*) xv, yv
	    WRITE (6,*)'Enter x, y curcumference points: '
	    READ  (5,*) xcr, ycr
            np = 50
	    CALL GCIRCL (sysin, xv, yv, xcr, ycr, np, iret)
	    CALL GEPLOT (ier)
	    WRITE (6,*) ' GCIRCL - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF  ( cmd .eq. 'GSKY' ) THEN
	    WRITE (6,*)
     +		'GSKY - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) 'Enter np'
	    READ  (5,*) np
	    WRITE (6,*) 'Enter', np, ' sky coverage codes'
	    READ  (5,*) ( cod (m), m = 1, np )
	    WRITE (6,*) 'Enter', np, ' x- y- locations'
	    READ  (5,*) ( x (m), y (m), m = 1, np )
	    WRITE (6,*) 'Enter', np, ' x- y- offsets'
	    READ  (5,*) ( ixof (m), iyof (m), m = 1, np )
	    CALL GSKY ( sysin, np, cod, x, y, ixof, iyof, iret ) 
	    WRITE (6,*) ' GSKY - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF  ( cmd .eq. 'GWTHR' ) THEN
	    WRITE ( 6,*)
     +		'GWTHR - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  ( 5, 20 ) sysin
	    WRITE ( 6,* ) 'Enter np'
	    READ  ( 5,* ) np
	    WRITE ( 6,* ) 'Enter ', np, ' weather symbol codes'
            READ  ( 5,* ) ( cod (m), m = 1, np )
	    WRITE ( 6,* ) 'Enter', np, ' x- y- coordinates'
	    READ  ( 5,* ) ( x (m), y (m), m = 1, np )
	    WRITE ( 6,* ) 'Enter', np, ' x- y- offsets'
	    READ  ( 5,* ) ( ixof (m), iyof (m), m = 1, np )
	    CALL GWTHR ( sysin, np, cod, x, y, ixof, iyof, iret ) 
C
	  ELSE IF  ( cmd .eq. 'GPTND' ) THEN
	    WRITE (6,*)
     +		'GPTND - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) 'Enter np'
	    READ  (5,*)  np
	    WRITE (6,*) 'Enter ', np, ' pressure tendency codes,'
	    READ  (5,*)  ( cod (m), m = 1, np )
	    WRITE ( 6,* ) 'Enter', np, ' x- y- coordinates'
	    READ  ( 5,* ) ( x (m), y (m), m = 1, np )
	    WRITE ( 6,* ) 'Enter', np, ' x- y- offsets'
	    READ  ( 5,* ) ( ixof (m), iyof (m), m = 1, np )
	    CALL GPTND ( sysin, np, cod, x, y, ixof, iyof, iret ) 
	    WRITE (6,*) ' GPTND - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF  ( cmd .eq. 'GPWTH' ) THEN
	    WRITE (6,*)
     +		'GPWTH - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) 'Enter np'
	    READ  (5,*) np
	    WRITE (6,*) 'Enter ', np, ' past weather symbol codes,'
            WRITE (6,*) 'locations for past weather symbols,',
     +                   ' and offsets'
	    READ  (5,*) ( cod (m), x (m), y (m), ixof(m), iyof(m),
     +                   m = 1,np)
            CALL GPWTH ( sysin, np, cod, x, y, ixof, iyof, iret ) 
	    WRITE (6,*) ' GPWTH - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF  ( cmd .eq. 'GCTYP' ) THEN
	    WRITE (6,*)
     +		'GCTYP - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) 'Enter np'
	    READ  (5,*) np
	    WRITE (6,*) 'Enter ', np, ' cloud type codes'
	    READ  (5,*) ( cod (m), m = 1, np )
	    WRITE (6,*) 'Enter', np, ' x- y- coordinates'
	    READ  (5,*) ( x (m), y (m), m = 1, np )
	    WRITE (6,*) 'Enter', np, ' x- y- offsets'
	    READ  (5,*) ( ixof (m), iyof (m), m = 1, np )
            CALL GCTYP ( sysin, np, cod, x, y, ixof, iyof, iret ) 
	    WRITE (6,*) ' GCTYP - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF  ( cmd .eq. 'GICNG' ) THEN
	    WRITE ( 6,*)
     +		'GICNG - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  ( 5, 20 ) sysin
	    WRITE ( 6,* ) 'Enter np'
	    READ  ( 5,* ) np
	    WRITE ( 6,* ) 'Enter ', np, ' icing symbol codes'
            READ  ( 5,* ) ( cod (m), m = 1, np )
	    WRITE ( 6,* ) 'Enter', np, ' x- y- coordinates'
	    READ  ( 5,* ) ( x (m), y (m), m = 1, np )
	    WRITE ( 6,* ) 'Enter', np, ' x- y- offsets'
	    READ  ( 5,* ) ( ixof (m), iyof (m), m = 1, np )
	    CALL GICNG ( sysin, np, cod, x, y, ixof, iyof, iret ) 
C
	  ELSE IF  ( cmd .eq. 'GSPCL' ) THEN
	    WRITE ( 6,*)
     +		'GSPCL - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  ( 5, 20 ) sysin
	    WRITE ( 6,* ) 'Enter np'
	    READ  ( 5,* ) np
	    WRITE ( 6,* ) 'Enter ', np, ' special symbol codes'
            READ  ( 5,* ) ( cod (m), m = 1, np )
	    WRITE ( 6,* ) 'Enter', np, ' x- y- coordinates'
	    READ  ( 5,* ) ( x (m), y (m), m = 1, np )
	    WRITE ( 6,* ) 'Enter', np, ' x- y- offsets'
	    READ  ( 5,* ) ( ixof (m), iyof (m), m = 1, np )
	    CALL GSPCL ( sysin, np, cod, x, y, ixof, iyof, iret ) 
C
	  ELSE IF  ( cmd .eq. 'GTURB' ) THEN
	    WRITE ( 6,*)
     +		'GTURB - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  ( 5, 20 ) sysin
	    WRITE ( 6,* ) 'Enter np'
	    READ  ( 5,* ) np
	    WRITE ( 6,* ) 'Enter ', np, ' turbulence symbol codes'
            READ  ( 5,* ) ( cod (m), m = 1, np )
	    WRITE ( 6,* ) 'Enter', np, ' x- y- coordinates'
	    READ  ( 5,* ) ( x (m), y (m), m = 1, np )
	    WRITE ( 6,* ) 'Enter', np, ' x- y- offsets'
	    READ  ( 5,* ) ( ixof (m), iyof (m), m = 1, np )
	    CALL GTURB ( sysin, np, cod, x, y, ixof, iyof, iret ) 
C
	  ELSE IF ( cmd .eq. 'GFRNT' ) THEN
	    WRITE (6,*)
     +		'GFRNT - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) ' Enter number of line segments to draw: '
	    READ  (5,*) nl
	    WRITE (6,*)' Enter ', nl, ' coordinate pairs '
	    READ  (5,*) ( xinp (mm), yinp (mm), mm = 1,nl )
	    CALL GFRNT ( sysin, nl, xinp, yinp, iret )
	    WRITE (6,*) ' GFRNT - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSPLN' ) THEN
	    WRITE (6,*)
     +		'GSPLN - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*)' Enter number of points along the special line: '
	    READ  (5,*) nl
	    WRITE (6,*)' Enter ', nl, ' coordinate pairs '
	    READ  (5,*) ( xinp (mm), yinp (mm), mm = 1,nl )
	    CALL GSPLN ( sysin, nl, xinp, yinp, iret )
	    WRITE (6,*) ' GSPLN - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GLOGO' ) THEN
	    WRITE (6,*)
     +		'GLOGO - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*)
     +		' Enter X and Y for the logo center (REAL, REAL):'
	    READ  (5,*) xc, yc
	    WRITE (6,*) ' Enter the logo size (REAL): '
	    READ  (5,*) size
	    WRITE (6,*) ' Enter the logo color mode 1 - Monochrome ' 
	    WRITE (6,*) '                           2 - Color ' 
	    READ  (5,*) iclmod 
	    WRITE (6,*) ' Enter the logo ID 1 - NOAA ' 
	    READ  (5,*) ilogo 
	    CALL GLOGO ( sysin, xc, yc, size, iclmod, ilogo, iret )
	    WRITE (6,*) ' GLOGO - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF  ( cmd .eq. 'GCMBO' ) THEN
	    WRITE ( 6,*)
     +		'GCMBO - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  ( 5, 20 ) sysin
	    WRITE ( 6,* ) 'Enter np'
	    READ  ( 5,* ) np
	    WRITE ( 6,* ) 'Enter ', np, ' combination symbol codes'
            READ  ( 5,* ) ( cod (m), m = 1, np )
	    WRITE ( 6,* ) 'Enter', np, ' x- y- coordinates'
	    READ  ( 5,* ) ( x (m), y (m), m = 1, np )
	    WRITE ( 6,* ) 'Enter', np, ' x- y- offsets'
	    READ  ( 5,* ) ( ixof (m), iyof (m), m = 1, np )
	    CALL GCMBO ( sysin, np, cod, x, y, ixof, iyof, iret ) 
C
	  ELSE IF  ( cmd .eq. 'GARC' ) THEN
	    WRITE (6,*)
     +		'GARC - Enter coordinate system ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*)'Enter x, y coordinates: '
	    READ  (5,*) xv, yv
	    WRITE (6,*)'Enter x, y circumference points: '
	    READ  (5,*) xcr, ycr
	    WRITE (6,*)'Enter angle 1, angle2 points: '
	    READ  (5,*) ang1, ang2
            np = 65
	    CALL GARC (sysin, xv, yv, xcr, ycr, np, ang1, ang2, 
     +				endpts, iret)
	    CALL GEPLOT (ier)
            WRITE (6,*) ' End points : ', endpts
	    WRITE (6,*) ' GARC - iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
C------------------------------------------------------------------------
C*	Transform
C
	  ELSE IF ( cmd .eq. 'GTRANS' ) THEN
	    WRITE (6,*)
     +		'GTRANS - Enter input coordinate ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) 'Enter output coordinate ( S,D,N,V,P,M,G )'
	    READ  (5,20) sysout
	    WRITE (6,*) ' Enter the  number of points: '
	    READ  (5,*,IOSTAT = iostat) np
	    WRITE (6,*) 'Enter ', np, ' pairs of points'
	    READ  (5,*,IOSTAT=iostat) ( xinp (ip), yinp (ip), ip=1, np )
	    CALL GTRANS ( sysin, sysout, np, xinp, yinp, xoutp, youtp, 
     +				iret )
	    WRITE ( 6, * )' GTRANS iret ', iret 
	    WRITE ( 6, 9013 ) ( xinp(ip), yinp(ip), xoutp(ip), 
     +				youtp(ip), ip = 1, np )
9013	    FORMAT ( 5X, 4F10.3 )
C
	  ELSE IF ( cmd .eq. 'GPTVIS' ) THEN
	    WRITE (6,*)'GPTVIS -Enter coordinate system (S,D,N,V,P,M,G )'
	    READ  (5,20) sysin
	    WRITE (6,*) ' Enter the number of points: '
	    READ  (5,*,IOSTAT = iostat) np
	    WRITE (6,*)'Enter ',np,' X, Y pairs of points ' 
	    READ  (5,*,IOSTAT = iostat) ( xinp (ip), yinp (ip), ip=1,np )
	    CALL GPTVIS ( sysin, np, xinp, yinp, pntvis, iret )
	    WRITE (6,*) ' GPTVIS iret ', iret
	    WRITE (6,9014) ( xinp (ip), yinp (ip), pntvis (ip),
     +				ip = 1, np )
9014	    FORMAT ( 5X, 2F10.3, L5 )
C------------------------------------------------------------------------
C*	Attribute
C
	  ELSE IF ( cmd .eq. 'GSLINE' ) THEN
	    WRITE (6,*) 'GSLINE - Enter ILTYP, ILTHW, IWIDTH, ILWHW:'
	    READ  (5,*,IOSTAT=iostat) iltyp, ilthw, iwidth, iwhw
	    CALL GSLINE ( iltyp, ilthw, iwidth, iwhw, iret )
	    WRITE (6,*) ' GSLINE iret ', iret 
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSCNTR' ) THEN
	    WRITE (6,*) ' GSCNTR - Enter ibxsub, ilblbk, ',
     +				'ismoth, isadjf'
	    READ  (5,*,IOSTAT=iostat) ibxsub, ilblbk, ismoth, isadjf
	    CALL GSCNTR ( ibxsub, ilblbk, ismoth, isadjf, iret )
	    WRITE (6,*) ' GSCNTR iret ', iret 
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSTEXT' ) THEN
	    WRITE (6,*)' GSTEXT -Enter ITXFN, ITXHW, SZTEXT, ITXWID,'
	    WRITE (6,*)'               IBRDR, IRROTN, IJUST:'
	    READ  (5,*,IOSTAT=iostat) itfn, ithw, sztext, iwidth,
     +					ibrdr, irrotn, ijust
	    CALL GSTEXT ( itfn, ithw, sztext, iwidth,
     +				ibrdr, irrotn, ijust, iret )
	    WRITE (6,*) ' GSTEXT iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSBARB' ) THEN
	    WRITE (6,*) ' GSBARB - Enter SZBARB, IBRWID, IBRTYP'
	    READ  (5,*,IOSTAT=iostat) szbarb, ibrwid, ibrtyp
	    CALL GSBARB ( szbarb, ibrwid, ibrtyp, iret )
	    WRITE (6,*) ' GSBARB - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSARRW' ) THEN
	    WRITE (6,*) ' GSARRW - Enter SZARRW, SZARRH, IARWID, IARTYP:'
	    READ  (5,*,IOSTAT=iostat) szarrw, szarrh, iarwid, iartyp
	    CALL GSARRW ( szarrw, szarrh, iarwid, iartyp, iret )
	    WRITE (6,*) ' GSARRW - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSDARR' ) THEN
	    WRITE (6,*) ' GSDARR - Enter SZDARW, SZDARH, IDARWD, IDARTP:'
	    READ  (5,*,IOSTAT=iostat) szdarw, szdarh, idarwd, idartp
	    CALL GSDARR ( szdarw, szdarh, idarwd, idartp, iret )
	    WRITE (6,*) ' GSDARR - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSHASH' ) THEN
	    WRITE (6,*) ' GSHASH - Enter SZHSH, IHWID, ILWID:'
	    READ  (5,*,IOSTAT=iostat) szhsh, ihwid, ilwid
	    CALL GSHASH ( szhsh, ihwid, ilwid, iret )
	    WRITE (6,*) ' GSHASH - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSMRKR' ) THEN
	    WRITE (6,*) 'GSMRKR - Enter imark, ihw, size, iwidth '
	    READ  (5,*,IOSTAT=iostat) imark, imhw, szmark, iwidth
	    CALL GSMRKR ( imark, imhw, szmark, iwidth, iret )
	    WRITE (6,*) ' GSMRKR - iret ', iret 
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSTICK' ) THEN
	    WRITE (6,*) 'GSTICK - Enter itktyp, sztick'
	    READ  (5,*,IOSTAT=iostat) itktyp, sztick
	    CALL GSTICK ( itktyp, sztick, iret )
	    WRITE (6,*) ' GSTICK - iret ', iret 
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSLPAT' ) THEN
	    WRITE (6,*) ' GSLPAT - Enter 8 pattern values'
	    READ  (5,*,IOSTAT=iostat) ( lpat (ip), ip = 1, 8 )
	    CALL GSLPAT ( lpat, iret )
	    WRITE (6,*) ' GSLPAT - iret ', iret 
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
          ELSE IF ( cmd .eq. 'GSSKY' ) THEN
	    WRITE (6,*) ' GSSKY - Enter szsky, isktyp, iskwid'
            READ  (5,*) szsky, isktyp, iskwid
	    CALL GSSKY ( szsky, isktyp, iskwid, iret )
            WRITE (6,*) ' GSSKY - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSWTHR' ) THEN
	    WRITE (6,*) ' GSWTHR - Enter szwthr, iwtwid'
	    READ  (5,*) szwthr, iwtwid
	    CALL GSWTHR ( szwthr, iwtwid, iret)
	    WRITE (6,*) ' GSWTHR - IRET ', IRET
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSFILL' ) THEN
	    WRITE (6,*) ' GSFILL - Enter szfil, ifltyp'
	    READ  (5,*) szfil, ifltyp
	    CALL GSFILL ( szfil, ifltyp, iret)
	    WRITE (6,*) ' GSFILL - IRET ', IRET
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSSMTH' ) THEN
	    WRITE (6,*) ' GSSMTH - Enter ismtyp, dens'
	    READ  (5,*) ismtyp, dens
	    CALL GSSMTH ( ismtyp, dens, iret )
	    WRITE (6,*) ' GSSMTH - IRET ', IRET
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSRDUC' ) THEN
	    WRITE (6,*) ' GSRDUC - Enter filter'
	    READ  (5,*) filter
	    CALL GSRDUC ( filter, iret )
	    WRITE (6,*) ' GSRDUC - IRET ', IRET
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSDASH' ) THEN
	    WRITE (6,*) ' GSDASH - Enter szdsh'
	    READ  (5,*) szdsh
	    CALL GSDASH ( szdsh, iret )
	    WRITE (6,*) ' GSDASH - IRET ', IRET
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSPTND' ) THEN
	    WRITE (6,*) ' GSPTND - Enter szptnd, iptwid'
	    READ  (5,*) szptnd, iptwid
	    CALL GSPTND ( szptnd, iptwid, iret)
	    WRITE (6,*) ' GSPTND - IRET ', IRET
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSPWTH' ) THEN
	    WRITE (6,*) ' GSPWTH - Enter szpwth, ipwwid'
	    READ  (5,*) szpwth, ipwwid
	    CALL GSPWTH ( szpwth, ipwwid, iret)
	    WRITE (6,*) ' GSPWTH - IRET ', IRET
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSCTYP' ) THEN
	    WRITE (6,*) ' GSCTYP - Enter szctyp, ictwid'
	    READ  (5,*) szctyp, ictwid
	    CALL GSCTYP ( szctyp, ictwid, iret)
	    WRITE (6,*) ' GSCTYP - IRET ', IRET
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSICNG' ) THEN
	    WRITE (6,*) ' GSICNG - Enter szicng, icewid'
	    READ  (5,*) szicng, icewid
	    CALL GSICNG ( szicng, icewid, iret)
	    WRITE (6,*) ' GSICNG - IRET ', IRET
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSSPCL' ) THEN
	    WRITE (6,*) ' GSSPCL - Enter szspcl, ispwid'
	    READ  (5,*) szspcl, ispwid
	    CALL GSSPCL ( szspcl, ispwid, iret)
	    WRITE (6,*) ' GSSPCL - IRET ', IRET
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSTURB' ) THEN
	    WRITE (6,*) ' GSTURB - Enter szturb, ituwid'
	    READ  (5,*) szturb, ituwid
	    CALL GSTURB ( szturb, ituwid, iret)
	    WRITE (6,*) ' GSTURB - IRET ', IRET
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSFRNT' ) THEN
	    WRITE (6,*) 'GSFRNT - Enter IFCOD, PIPSZ, IPIPST, IPIPDR:'
	    READ  (5,*,IOSTAT=iostat) ifcod, pipsz, ipipst, ipipdr
	    CALL GSFRNT ( ifcod, pipsz, ipipst, ipipdr, iret )
	    WRITE (6,*) ' GSFRNT iret ', iret 
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSSPLN' ) THEN
	    WRITE(6,*)'GSSPLN - Enter ISLTYP,ISLSTR,ISLDIR,SLSIZ,ISLWID:'
	    READ(5,*,IOSTAT=iostat) isltyp, islstr, isldir, slsiz, islwid
	    CALL GSSPLN ( isltyp, islstr, isldir, slsiz, islwid, iret )
	    WRITE (6,*) ' GSSPLN iret ', iret 
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSCMBO' ) THEN
	    WRITE (6,*) ' GSCMBO- Enter szcmwx, icmbwd'
	    READ  (5,*) szcmwx, icmbwd
	    CALL GSCMBO( szcmwx, icmbwd, iret)
	    WRITE (6,*) ' GSCMBO- IRET ', IRET
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQLINE' ) THEN
	    CALL GQLINE ( iltyp, ilthw, iwidth, iwhw, iret )
	    WRITE (6,*) 'iret, iltyp, ilthw, iwidth, iwhw',
     +				iret, iltyp, ilthw, iwidth, iwhw
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQCNTR' ) THEN
	    CALL GQCNTR ( ibxsub, ilblbk, ismoth, isadjf, iret )
	    WRITE (6,*) 'iret, ibxsub, ilblbk, ismoth, isadjf',
     +				iret, ibxsub, ilblbk, ismoth, isadjf
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQTEXT' ) THEN
	    CALL GQTEXT ( itfn, ithw, sztext, iwidth,
     +				ibrdr, irrotn, ijust, iret )
	    WRITE (6,110) ' GQTEXT: itfn, ithw, sztext, iwidth,',
     +				' ibrdr, irrotn, ijust',
     +				itfn, ithw, sztext, iwidth,
     +				ibrdr, irrotn, ijust
110	    FORMAT ( 1X, A, A, 2X, 2I7, F8.3, 4I7 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQBARB' ) THEN
	    CALL GQBARB ( szbarb, ibrwid, ibrtyp, iret )
	    WRITE (6,111) ' iret, szbarb, ibrwid, ibrtyp ', iret,
     +				szbarb, ibrwid, ibrtyp
111	    FORMAT ( 5X, A, 2X, I7, F8.3, 2I7 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQARRW' ) THEN
	    CALL GQARRW ( szarrw, szarrh, iarwid, iartyp, iret )
	    WRITE (6,112) ' iret, szarrw, szarrh, iarwid, iartyp ', 
     +                    iret, szarrw, szarrh, iarwid, iartyp
112	    FORMAT ( 5X, A, 2X, I7, 2F8.3, 2I7 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQDARR' ) THEN
	    CALL GQDARR ( szdarw, szdarh, idarwd, idartp, iret )
	    WRITE (6,113) ' iret, szdarw, szdarh, idarwd, idartp ', 
     +				iret, szdarw, szdarh, idarwd, idartp
113	    FORMAT ( 5X, A, 2X, I7, 2F8.3, 2I7 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQHASH' ) THEN
	    CALL GQHASH ( szhsh, ihwid, ilwid, iret )
	    WRITE (6,114) ' iret, szhsh, ihwid, ilwid ', 
     +                    iret, szhsh, ihwid, ilwid
114	    FORMAT ( 5X, A, 2X, I7, F8.3, 2I7 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQMRKR' ) THEN
	    CALL GQMRKR ( imark, imhw, szmark, iwidth, iret )
	    WRITE (6,115) ' iret, imark, imhw, szmark, iwidth ', 
     +				iret, imark, imhw, szmark, iwidth
115	    FORMAT ( 5X, A, 2X, 3I7, F8.3, I7 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQLPAT' ) THEN
	    CALL GQLPAT ( lpat, iret )
	    WRITE (6,*) ' GQLPAT - iret ', iret 
	    WRITE (6,*) ( lpat (ip), ip=1, 8 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQSYSZ' ) THEN
	    CALL GQSYSZ ( rxszmk, ryszmk, rxsztx, rysztx, 
     +				rxszwb, ryszwb, iret )
	    WRITE (6,*) ' rxszmk, ryszmk, rxsztx, rysztx,',
     +				' rxszwb, ryszwb, iret'
	    WRITE (6,116)  rxszmk, ryszmk, rxsztx, rysztx, 
     +				rxszwb, ryszwb, iret
116	    FORMAT ( 5X, 6F8.3, I7 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQSIZD' ) THEN
	    CALL GQSIZD ( szmk, sztx, szwb, szws,
     +			  szab, szah, iret )
	    WRITE (6,*) ' szmk, sztx, szwb, szws,',
     +				' szab, szah, iret'
	    WRITE (6,116)  szmk, sztx, szwb, szws,
     +				szab, szah, iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQSKY' ) THEN
	    CALL GQSKY ( szsky, isktyp, iskwid, iret )
	    WRITE (6,111) ' iret, szsky, isktyp, iskwid', iret, 
     +				szsky, isktyp, iskwid
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQWTHR' ) THEN
	    CALL GQWTHR ( szwthr, iwtwid, iret )
	    WRITE (6,111) ' iret, szwthr, iwtwid ',
     +				iret, szwthr, iwtwid
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQFILL' ) THEN
	    CALL GQFILL ( szfil, ifltyp, iret )
	    WRITE (6,111) ' iret, szfil, ifltyp ',
     +				iret, szfil, ifltyp
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQSMTH' ) THEN
	    CALL GQSMTH ( ismtyp, dens, iret )
	    WRITE (6,118) ' iret, ismtyp, dens ',
     +				iret, ismtyp, dens
118	    FORMAT ( 5X, A, 2(2X, I7), F8.3 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQRDUC' ) THEN
	    CALL GQRDUC ( filter, iret )
	    WRITE (6,120) ' iret, filter ', iret, filter
120	    FORMAT ( 5X, A, I7, F8.3 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQDASH' ) THEN
	    CALL GQDASH ( szdsh, iret )
	    WRITE (6,119) ' iret, szdsh ', iret, szdsh
119	    FORMAT ( 5X, A, I7, F8.3 )
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQPTND' ) THEN
	    CALL GQPTND ( szptnd, iptwid, iret )
	    WRITE (6,111) ' iret, szptnd, iptwid',
     +				iret, szptnd, iptwid
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQPWTH' ) THEN
	    CALL GQPWTH ( szpwth, ipwwid, iret)
	    WRITE (6,111) ' iret, szpwth, ipwwid',
     +				iret, szpwth, ipwwid
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQCTYP' ) THEN
	    CALL GQCTYP ( szctyp, ictwid, iret )
	    WRITE (6,111) ' iret, szctyp, ictwid',
     +				iret, szctyp, ictwid
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQICNG' ) THEN
	    CALL GQICNG ( szicng, icewid, iret )
	    WRITE (6,111) ' iret, szicng, icewid ',
     +				iret, szicng, icewid
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQSPCL' ) THEN
	    CALL GQSPCL ( szspcl, ispwid, iret )
	    WRITE (6,111) ' iret, szspcl, ispwid ',
     +				iret, szspcl, ispwid
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQTURB' ) THEN
	    CALL GQTURB ( szturb, ituwid, iret )
	    WRITE (6,111) ' iret, szturb, ituwid ',
     +			    iret, szturb, ituwid
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQFRNT' ) THEN
	    CALL GQFRNT ( ifcod, pipsz, ipipst, ipipdr, iret )
	    WRITE (6,*) 'iret, ifcod, pipsz, ipipst, ipipdr, ',
     +	                    iret, ifcod, pipsz, ipipst, ipipdr
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQSPLN' ) THEN
	    CALL GQSPLN ( isltyp, islstr, isldir, slsiz, islwid, iret )
	    WRITE (6,*) 'iret, isltyp, islstr, isldir, slsiz, islwid, ',
     +	                    iret, isltyp, islstr, isldir, slsiz, islwid 
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQDATT' ) THEN
	    CALL GQDATT ( iunit, filnam, itype, xsize, ysize, ncurwn,
     +			  iret )
	    IF  ( iret .eq. 0 )  THEN
		WRITE (6,*) ' GQDATT - iunit  = ', iunit
		WRITE (6,*) ' GQDATT - filnam = ', filnam
		WRITE (6,*) ' GQDATT - itype  = ', itype
		WRITE (6,117) ' GQDATT - xsize  = ', xsize
		WRITE (6,117) ' GQDATT - ysize  = ', ysize
117		FORMAT ( 1X, A, F10.4 )
		WRITE (6,*) ' GQDATT - ncurwn = ', ncurwn
		WRITE (6,*) ' GQDATT - iret   = ', iret
	    END IF
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQCVSC' ) THEN
	    CALL GQCVSC ( dvcvsc, iret )
	    WRITE (6,*) ' GQCVSC - dvcvsc = ', dvcvsc
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQCMBO' ) THEN
	    CALL GQCMBO( szcmwx, icmbwd, iret )
	    WRITE (6,111) ' iret, szcmwx, icmbwd',
     +			    iret, szcmwx, icmbwd
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
C------------------------------------------------------------------------
C*	Color
C
	  ELSE IF  ( cmd .eq. 'GSBRGB' )  THEN
	    WRITE (6,*) 'GSBRGB - Enter ICBANK:'
	    READ  (5,*)  icbank 
	    WRITE (6,*) 'GSBRGB - Enter NCOLR'
	    READ  (5,*)  ncolr 
	    WRITE (6,*) 'GSBRGB - Enter ICOLR IRED IGREEN IBLUE'
	    READ  (5,*)  ( icolrs(i), ireds(i), igrns(i), iblus(i),
     +			   i = 1,ncolr ) 
	    CALL GSBRGB  ( icbank, ncolr, icolrs, ireds, igrns, iblus,
     +			   iret )
	    WRITE (6,*) 'GSBRGB - iret = ', iret
C
	  ELSE IF ( cmd .eq. 'GSCOLR' ) THEN
	    WRITE (6,*) 'GSCOLR - Enter color number: '
	    READ  (5,*,IOSTAT=iostat) icol
	    CALL GSCOLR ( icol, iret )
	    WRITE (6,*) ' GSCOLR - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSCLR2' ) THEN
	    WRITE (6,*) 'GSCLR2 - Enter color numbers: '
	    READ  (5,*,IOSTAT=iostat) icol, icol2
	    CALL GSCLR2 ( icol, icol2, iret )
	    WRITE (6,*) ' GSCLR2 - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSCTBL' ) THEN
	    WRITE ( 6, '(A,$)') ' GSCTBL - Enter color table name: ' 
	    READ  (5, 20, IOSTAT = iostat )  ctblnm
	    CALL GSCTBL ( ctblnm, iret )
	    WRITE (6,*) 'GSCTBL  iret = ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSCOLB' ) THEN
	    WRITE (6,*) 'GSCOLB - Enter color bank, color number'
	    READ  (5,*,IOSTAT=iostat) icbank, icol
	    CALL GSCOLB ( icbank, icol, iret )
	    WRITE (6,*) ' GSCOLB - iret ', iret
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GSCRGB' ) THEN
	    WRITE (6,*) 'GSCRGB  Enter color number, red, green, blue'
	    READ  (5,*) ico, ired, igreen, iblue
	    CALL GSCRGB ( ico, ired, igreen, iblue, iret )
	    WRITE (6,*)' GSCRGB - iret = ', iret
C
	  ELSE IF ( cmd .eq. 'GSCINT' ) THEN
	    CALL GSCINT (iret)
	    WRITE (6,*)' GSCINT - iret ', iret 
C
	  ELSE IF ( cmd .eq. 'GSCNAM' ) THEN
	    WRITE (6,*) ' GSCNAM - Enter color number'
	    READ  (5,*)  ico
	    WRITE (6,*)' Enter color name'
	    READ  (5,20) color
	    CALL GSCNAM ( ico, color, iret )
	    WRITE (6,*)' GSCNAM - iret ', iret
C
	  ELSE IF ( cmd .eq. 'GQCOMP' ) THEN
	      WRITE (6, *) 'GQCOMP - Enter color to query '
	      READ  (5,*) ico
	      CALL GQCOMP ( ico, color, ired, igreen, iblue, xname,
     +			    iret )
	      WRITE (6,*) ' GQCOMP return code ', iret,
     +			  ', and RGB ', ired, igreen, iblue
	      WRITE (6,*) '   Names:  ', color, ', and X ', xname
C
	  ELSE IF ( cmd .eq. 'GQCOLR' ) THEN
	    CALL GQCOLR ( jcolr, iret )
	    WRITE (6,*) ' GQCOLR iret, jcolr', iret, jcolr
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQCLR2' ) THEN
	    CALL GQCLR2 ( jcolr, jcolr2, iret )
	    WRITE (6,*) ' GQCLR2 iret, jcolr, jcolr2', iret, jcolr, 
     +		          jcolr2
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQNCOL' ) THEN
	    CALL GQNCOL ( jcolr, iret )
	    WRITE (6,*) ' GQNCOL iret, jcolr', iret, jcolr
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C
	  ELSE IF ( cmd .eq. 'GQCLRS' ) THEN
	    WRITE (6,*) ' GQCLRS - Enter color bank to querry'
	    READ  (5,*) icbank
	    CALL GQCLRS ( icbank, ncolr, iret )
	    WRITE (6,*) ' GQCLRS iret, ncolr', iret, ncolr
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C------------------------------------------------------------------------
C*	Curve
C
	  ELSE IF ( cmd .eq. 'GCYEVL' ) THEN
	    WRITE (6,*) 'Enter itype, np, npout'
	    READ  (5,*,IOSTAT=iostat) itype, np, npout
	    WRITE (6,*) 'Enter x (i),y (i),i = 1,np'
	    READ  (5,*,IOSTAT=iostat) ( x (i), y (i), i = 1,np )
	    WRITE (6,*) 'Enter xeval (i),i = 1,npout'
	    READ  (5,*,IOSTAT=iostat) ( xeval (i),i = 1,npout)
	    CALL GCYEVL ( itype, np, x, y, npout, xeval, yeval, iret )
	    WRITE (6,*) 'GCYEVL - iret = ', iret
	    WRITE (6,*) 'yeval = '
	    WRITE (6,123) (yeval (i),i = 1, npout)
C------------------------------------------------------------------------
C*	Read in grid.
C
	  ELSE IF ( cmd .eq. 'GETGRD' ) THEN
	    WRITE (6,*)  'Enter name of grid file: '
	    READ  (5,20)  file
	    CALL GD_OPNF ( file, .false., iflno, navxz, rnvblk, 
     +			   ianlsz, anlblk, ihdrsz, maxgrd, iret )
	    IF ( iret .ne. 0 ) THEN
		WRITE (6,*) ' Error opening grid file ', iret
	    ELSE
	      CALL GR_SNAV ( navxz, rnvblk, ier )
	      WRITE (6,*) 'Enter grid number'
	      READ  (5,*)  num
	      CALL GD_GGRD ( iflno, num, time, level, ivcord, parm,
     +			   grid, igx, igy, ihdr, iret )
	      CALL GD_CLOS ( iflno, ier )
	      IF  ( iret .ne. 0 )  THEN
		WRITE (6,*) 'Grid not read'
		grdflg = .false.
	      ELSE
		CALL GR_WTRM ( 6, .false., num, time, level,
     +			       ivcord, parm, ier )
	        WRITE (6,*) 'Enter iminx, iminy, imaxx, imaxy,',
     +			' iskip, misflg'
	        READ  (5,*) iminx, iminy, imaxx, imaxy, iskip, misflg
	        CALL GR_SUBX ( igx, igy, grid, iminx, iminy, imaxx,
     +			       imaxy, iskip, misflg, kx, ky, subgrd,
     +			       ioffx, ioffy, iskip, ier )
	        grdflg = .true.
	        WRITE (6,*) 'KX,KY,IOFFX, IOFFY, ISKIP: ', kx, ky,
     +		             ioffx, ioffy, iskip
	        WRITE (6,*) 'Enter Y to list grid'
	        READ  (5,20) ans
	        IF ( ( ans .eq. 'Y') .or. ( ans .eq. 'y') )  THEN
		    WRITE (6,123) ( subgrd (iii), iii=1,kx*ky )
123		    FORMAT ( 1X, 8F9.3 )
	        END IF
	      END IF
	    END IF
C------------------------------------------------------------------------
C*	Check for exit.
C
	  ELSE IF ( cmd (1:1) .EQ. 'E' ) THEN
	    done = .true.
	END IF
C
C*	Go back to the beginning for another command.
C
	IF  ( .not. done )  GO TO 10
C
	END
