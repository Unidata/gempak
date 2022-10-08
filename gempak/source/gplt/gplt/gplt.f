	PROGRAM  GPLT
C************************************************************************
C* PROGRAM GPLT								*
C*									*
C* This module is the main program for the GPLT task.  It receives	*
C* information from the applications program and calls the appropriate	*
C* GEMPLT subroutines with identical names and calling sequences as	*
C* the applications modules.						*
C*									*
C* Log:									*
C* M. desJardins/GSFC	4/85	GEMPLT 3.1				*
C* M. desJardins/GSFC	12/85	Added curve fitting			*
C* M. desJardins/GSFC	 7/89	Increased number of points for GAAXIS	*
C* S. Schotz/GSC	 1/90	Updates for addtion of barb/arrow	*
C*                              width and type and marker/text width	*
C* S. Schotz/GSC	 2/90	Fixed bug with GAAXIS, NPTS not set	*
C* S. Schotz/GSC	 3/90	Added cloud/weather symbols GEMPLT 5.0	*
C* S. Schotz/GSC	 8/90	Added arrow head size			*
C* M. desJardins/GSFC	 9/90	Added GCURVE				*
C* M. desJardins/NMC	 7/91	UNIX version				*
C* S. Jacobs/EAI	 6/93	Added GGTPNT				*
C* S. Jacobs/EAI	 9/93	Added ITYP to GGTPNT			*
C* J. Cowie/NPS         10/93   Added check for MCI satnav; FSATMC.	*
C* A. Chang/EAI		12/93	Added   GSCTBL, GSFLNM, GSPLOT, GSPIXM, *
C*				GEPIXM, GLOOPC, GSHCAT, GSATIM		*
C* S. Jacobs/NMC	 3/94	Fixed GSATIM - added satfil		*
C* S. Jacobs/NMC	 3/94	Renamed: GSPIXM ==> GSTANM		*
C*					 GEPIXM ==> GENANM		*
C*					 GSHCAT ==> GSDATT		*
C* S. Jacobs/NMC	 7/94	Removed GSATAO				*
C* A. Chang/EAI		 8/94	Added GTEXTC				*
C* J. Cowie/COMET	 8/94	Added GSATMG (Mcidas Goes)		*
C* J. Cowie/COMET	12/94	Added GSLUTF - set LUT file		*
C* A. Chang/EAI		 1/95	Added GSLUTF input parameter "itype"	*
C* J. Cowie/COMET	 1/95	Incresed NAV size for GVAR in GSATMG	*
C* J. Cowie/COMET	 1/95	Added GSIMGB				*
C* J. Cowie/COMET	 2/95	Added GSICMN				*
C* J. Cowie/COMET	 5/95	Added arguments in GSATMG call		*
C* J. Cowie/COMET	 5/95	Added GSCOLB				*
C* C. Lin/EAI		 6/95	Added GSBRGB				*
C* G. Krueger/EAI	11/95	Removed HLS color scheme; Added XNAME.	*
C* J. Cowie/COMET       11/95   Removed GSLUTF function, added GQCLRS   *
C* M. Linda/GSC		12/95	Removed GCLDHT, GSATNP, and GSATMC	*
C* M. Linda/GSC		 3/96	Added GSDEVA, added filnam to GSDATT	*
C* S. Jacobs/NCEP	 4/96	Added iunit to GSDATT			*
C* S. Jacobs/NCEP	 5/96	Added GSLWIN and GQDATT			*
C* S. Jacobs/NCEP	 5/96	Removed GSDEV and GSFLNM		*
C* D.W.Plummer/NCEP	 5/96	Added parameters to FSTRML		*
C* S. Jacobs/NCEP	 8/96	Removed GSIMGB				*
C* M. Linda/GSC		 8/96	Added xICNG, xSPCL, and xTURB		*
C* E. Wehner/EAi	10/96	Added xFRNT				*
C* S. Maxwell/GSC	11/96	Removed save and restore functions	*
C* S. Jacobs/NCEP	11/96	Changed ITOS to STOI for GQMPRJ		*
C* E. Wehner/EAi	11/96	Remove parameter from xFRNT		*
C* M. Linda/GSC		 1/97	Changed from cbuf*500 to cbuf*400	*
C* K. Tyle/GSC           2/97   Added GCLPNL                            *
C* M. Linda/GSC		 2/97	Removed GFLUSH				*
C* D. Keiser/GSC	 3/97	Added xSPLN				*
C* E. Safford/GSC	 4/97	Added code for GTXSY			*
C* C. Lin/EAI	 	 6/97	Removed carray, use sysup		*
C* E. Safford/GSC        6/97   Added xTXSY                             *
C* C. Lin/EAI            6/97   Added code for GSROAM, GROAM            *
C* S. Maxwell/GSC        6/97   Added GSFILL                            *
C* S. Maxwell/GSC        6/97   Added GSGRP and GEGRP                   *
C* E. Safford/GSC        7/97   Modified xTXSY for additional params    *
C* G. Krueger/EAI	 7/97	Added GETMAP				*
C* M. Linda/GSC		 7/97	Added GLOGO				*
C* S. Jacobs/NCEP	 9/97	Removed GSTXSY and GQTXSY		*
C* S. Jacobs/NCEP	 9/97	Changed call to GSTEXT and GQTEXT	*
C* S. Jacobs/NCEP	 2/98	Added GSSMTH and GQSMTH			*
C* S. Jacobs/NCEP        2/98   Added GSDASH and GQDASH                 *
C* I. Durham/GSC	 3/98	Added GDARR, GSDARR and GQDARR		*
C* I. Durham/GSC	 3/98	Added GHASH, GSHASH, and GQHASH		*
C* I. Durham/GSC	 3/98	Added GSCLR2 and GQCLR2			*
C* S. Jacobs/NCEP	 6/98	Removed NP from GGTPNT call		*
C* S. Jacobs/NCEP	 6/98	Removed GSDATT and GOUTP		*
C* S. Jacobs/NCEP	 6/98	Moved GSCTBL from CONTROL to COLORS	*
C* T. Lee/GSC		 7/98	Added GQCVSC				*
C* S. Jacobs/NCEP	 7/98	Changed call to GTEXTC			*
C* A. Hardy/GSC         10/98   Added GCMBO, GSCMBO and GQCMBO          *
C* A. Hardy/GSC         11/98   Added parameter to GCIRCL call          *
C* S. Jacobs/NCEP	 5/99	Added GSRDUC and GQRDUC			*
C* T. Lee/GSC		 9/99	Added GSATPX				*
C* T. Lee/GSC		12/99	Added pixel area and mode to GSATPX	*
C* M. Li/GSC		 1/00	Added GCNTLN, GCNTFL and GCNTBX		*
C* A. Hardy/GSC          5/00	Added parameter to GLOGO call		*
C* A. Hardy/GSC          6/00   Added GARC call          		*
C* S. Jacobs/NCEP	 6/00	Increased imgnam from 80 to 256 chars	*
C* A. Hardy/GSC         12/00   Changed call to GDRGRD			*
C* J. Wu/GSC		 3/01   Added logo emblem ID 			*
C* M. Li/GSC		 4/01	Added GFLBND				*
C* T. Lee/SAIC		10/01	Added fill types to GCFILL calling seq.	*
C* D. Kidwell/NCEP	 6/02	Added GSGTGN                            *
C* D.W.Plummer/NCEP	 9/02	Chg GFLBND to GPLBND & add more parms.	*
C* D.W.Plummer/NCEP	 6/04	Chgs for GSATPX				*
C* R. McTaggart-Cowan/SUNY 01/05 Add semaphore locks for IPC queues.    *
C* C. Bailey/HPC	 1/05	Added GGSAVE				*
C* C. Bailey/HPC	 6/06	Added contour label array to call to 	*
C*				GCNTLN & GCLGRN				*
C* S. Gilbert/NCEP	 8/06	Increased sysstr to 12			*
C* C. Bailey/HPC	10/06	Added scflag to GCNTLN & GCLGRN		*
C* S. Gilbert/NCEP	 4/07	Removed CONTOR group                    *
C* S. Guan/NCEP          1/18   Added GSATMG4 for NETCDF4 Himawari data *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'GBUFF.CMN'
	INCLUDE		'ADBUFF.CMN'
	INCLUDE		'IMGDEF.CMN'
C
	LOGICAL		done
	CHARACTER	device*12, sys1*1, sys2*1, cbuf*400, sysstr*12
	CHARACTER	ctblnm*64, filnam*72, xname*80, wname*72
	CHARACTER	cprj*4, navtyp*8, imgnam*256, fil*80, satfil*132
	CHARACTER	mapnam*80, chrary (530)*24, messag*64
	CHARACTER	cbuf1*400, goe4*4
C*
C
	INTEGER		icmnarr (NIMCMN)
        EQUIVALENCE     ( igbuff (67 + 64),  goe4 )
	EQUIVALENCE	( icmnarr, imftyp )
C------------------------------------------------------------------------
C*	Initialize the GPLT subprocess.
C
	cprj = ' '
	mproc = 0
	CALL GIPROC  ( mproc, mbchna, iret )
C
C*	Initialize message buffer common area.
C
	jrtype = 1
	jwtype = 2
	ntypsr = -1
	irtype = 2
	iwtype = 1
C*
	IF  ( iret .eq. NORMAL ) THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
C
C*	Initialize common areas.
C
	CALL GINIT
C
C*     Obtain semaphore lock on IPC message queues.
C
       mproc = 0
       CALL GSGRAB  ( mproc, igrab, isemid, iret )
       IF ( igrab .ne. 1 ) THEN
           CALL ER_WMSG  ( 'GPLT', -1, ' ', iret )
           iout = igbuff (1) + 1
           igbuff ( iout ) = 2
           CALL GPUTB  ( igbuff (iout), iret )
           STOP
       END IF
C
C*	Check for and terminate any device driver processes on UNIX machines.
C
	IF  ( MTMACH .ne. MTVAX )  THEN
	    mproc = 1
	    CALL CCHECK ( mproc, mbchan, iwact, iret )
	    IF  ( iwact .eq. 1 )  THEN
		CALL DENDD  ( 1, ier )
		ddev = ' '
	    END IF
	END IF
C
C*	Assign sysup to sysstr
C
	sysstr = sysup
C
C*	Loop until GENDP requests task be deleted.
C
	DO  WHILE  ( .not. done )
C
C*	    Read in the next request.
C*	    Function code is in word 2.  
C
	    CALL GGETB  ( iret )
	    ifunc  = igbuff (2)
	    igroup = ifunc / 100
C
C*	    igbuff (1) is the number of words in input buffer.
C*	    Let iout be the first word in the output buffer.
C*	    The first word in the output buffer must contain the
C*	    number of words to return.  The second word is the return
C*	    code.
C
	    iout   = igbuff (1) + 1
	    igbuff ( iout ) = 2
	    igbuff ( iout + 1 ) = NFNCCD
C
C*	    Branch to the right subroutine using the group code and then
C*	    the function code.
C------------------------------------------------------------------------
	    IF  ( ifunc .eq. -999 ) THEN
		igbuff ( iout + 1 ) = -999
C------------------------------------------------------------------------
	      ELSE IF  ( igroup .eq. ACCESS ) THEN
C*
		IF  ( ifunc .eq. FINITP ) THEN
		    CALL GINITP (igbuff (3), igbuff (iout+2),
     +                           igbuff (iout+1 ))
		    igbuff (iout) = 3
C*
		  ELSE IF ( ifunc .eq. FENDP ) THEN
		    IF  ( igbuff (3) .eq. 1 ) done = .true.
		    CALL GENDP ( igbuff (3), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FSMODE ) THEN
		    CALL GSMODE ( igbuff (3), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FQMODE ) THEN
		    CALL GQMODE ( igbuff (iout+2), igbuff (iout+1) )
		    igbuff (iout) = 3
		END IF
C------------------------------------------------------------------------
	      ELSE IF ( igroup .eq. CONTRL ) THEN
C*
		  IF ( ifunc .eq. FSDEVA ) THEN
		    CALL ST_ITOS ( igbuff (3),  3, nc, device, ier )
		    CALL ST_ITOS ( igbuff (7), 18, nc, filnam, ier )
		    CALL GSDEVA ( device, igbuff (6), filnam,
     +				  igbuff (25), igbuff (26), igbuff (27), 
     +				  igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FCLEAR ) THEN
		    CALL GCLEAR ( igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FEPLOT ) THEN
		    CALL GEPLOT ( igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FEGRP ) THEN
		    CALL GEGRP ( igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FCLOSP ) THEN
		    CALL GCLOSP ( igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FQDEV ) THEN
		    CALL GQDEV ( device, igbuff (iout+5),
     +                           igbuff (iout+6),
     +				 igbuff (iout+1) )
		    CALL ST_STOI ( device, 12, nv, igbuff (iout+2), 
     +				   ier )
		    igbuff (iout) = 7
C*
		  ELSE IF ( ifunc .eq. FMESG ) THEN
		    CALL ST_ITOS  ( igbuff (3), 16, nc, messag, ier )
		    CALL GMESG ( messag, igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FGTPNT ) THEN
		    isys = igbuff (3)
		    sys1 = sysstr(isys:isys) 
		    ityp = igbuff (4)
		    IF  ( ityp .eq. 1 )  THEN
			np = 1
		      ELSE
			np = 2
		    END IF
		    CALL GGTPNT ( sys1, ityp, igbuff (iout+2),
     +				  igbuff (iout+2+np), igbuff (iout+1) )
		    igbuff (iout) = 2 + 2*np
C*
		  ELSE IF ( ifunc .eq. FSPLOT ) THEN
		    CALL GSPLOT ( igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FSTANM ) THEN
		    CALL GSTANM ( igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FENANM ) THEN
		    CALL GENANM ( igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FLOOPC ) THEN
		    CALL GLOOPC ( igbuff (3), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FSGRP ) THEN
		    CALL GSGRP ( igbuff (3), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FSLWIN ) THEN
		    CALL ST_ITOS  ( igbuff (3), 18, nc, wname, ier )
		    CALL GSLWIN ( wname, igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FGSAVE ) THEN
		    CALL ST_ITOS  ( igbuff (5), 18, nc, filnam, ier )
		    CALL GGSAVE ( filnam, igbuff (3), igbuff (4), 
     +				  igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FCLPNL ) THEN
		    CALL GCLPNL ( igbuff (3), igbuff (4), igbuff (5),
     +				  igbuff (6), igbuff (iout+1) )
C*
                  ELSE IF ( ifunc .eq. FSROAM ) THEN
                    CALL GSROAM ( igbuff (3), igbuff (4), igbuff (5),
     +                            igbuff (iout+1) )
C*
                  ELSE IF ( ifunc .eq. FROAM ) THEN
		    isys = igbuff(4)
                    sys1 = sysstr ( isys:isys )
                    CALL GROAM (igbuff(3), sys1,igbuff (5),igbuff (6),
     +                            igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FSGTGN ) THEN
		    CALL GSGTGN ( igbuff (3), igbuff (4),
     +				  igbuff (iout+1) )
C*
		END IF
C------------------------------------------------------------------------
	      ELSE IF ( igroup .eq. PLOT ) THEN
C*
		IF ( ifunc .eq. FLINE ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
	            CALL GLINE ( sys1, igbuff (4), igbuff (5), 
     +                           igbuff (5+np), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FSPLN ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
	            CALL GSPLN ( sys1, igbuff (4), igbuff (5), 
     +                           igbuff (5+np), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FFRNT ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
	            CALL GFRNT ( sys1, igbuff (4), igbuff (5), 
     +                           igbuff (5+np), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FTEXT ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            num = igbuff (1) - 8
	            cbuf = ' '
		    CALL ST_ITOS  ( igbuff (6), num, nc, cbuf, ier )
	            CALL GTEXT ( sys1, igbuff (4), igbuff (5), cbuf,
     +                           igbuff (6+num), igbuff (7+num), 
     +                           igbuff (8+num), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FTEXTC ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            num = igbuff (1) - 8
	            cbuf = ' '
		    CALL ST_ITOS  ( igbuff (6), num, nc, cbuf, ier )
	            CALL GTEXTC ( sys1, igbuff (4), igbuff (5), cbuf, 
     +				 igbuff (6+num), igbuff (7+num), 
     +				 igbuff (8+num), igbuff (iout+1) )
C*
  		  ELSE IF ( ifunc .eq. FTXSY ) THEN
		    isys = igbuff (3)
  	            sys1 = sysstr ( isys:isys )
  	            num = igbuff (1) - 11
  	            cbuf = ' '
  		    CALL ST_ITOS  ( igbuff (12), num, nc, cbuf, ier )
	            CALL GTXSY ( sys1, igbuff (4), igbuff(5),
     +				 igbuff (6), igbuff (7), igbuff (8),
     +				 igbuff (9), igbuff (10), igbuff (11),
     +				 cbuf, igbuff (iout+1) )
C*
	           ELSE IF ( ifunc .eq. FMARK ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
	            CALL GMARK ( sys1, igbuff (4), igbuff (5), 
     +                           igbuff (5+np), igbuff (iout+1) )
C*
	           ELSE IF ( ifunc .eq. FARRW ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
	            CALL GARRW ( sys1, igbuff (4), igbuff (5), 
     +                           igbuff (5+np), igbuff (5+2*np), 
     +                           igbuff (5+3*np), igbuff (iout+1) )
C*
	           ELSE IF ( ifunc .eq. FDARR ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
	            CALL GDARR ( sys1, igbuff (4), igbuff (5), 
     +                           igbuff (5+np), igbuff (5+2*np),
     +				 igbuff (iout+1) )
C*
	           ELSE IF ( ifunc .eq. FBARB ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
	            CALL GBARB ( sys1, igbuff (4), igbuff (5), 
     +                           igbuff (5+np), igbuff (5+2*np), 
     +                           igbuff (5+3*np), igbuff (iout+1) )
C*
	           ELSE IF ( ifunc .eq. FCIRCL ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            CALL GCIRCL ( sys1, igbuff (4), igbuff (5),
     +                            igbuff (6), igbuff (7), igbuff (8),
     +                            igbuff (iout + 1) )
C*
	           ELSE IF ( ifunc .eq. FHASH ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
	            CALL GHASH ( sys1, igbuff (4), igbuff (5), 
     +                           igbuff (5+np), igbuff (5+2*np),
     +				 igbuff (iout+1) )
C*
                   ELSE IF ( ifunc .eq. FSKY ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
                    CALL GSKY ( sys1, igbuff(4), igbuff(5), 
     +                          igbuff(5+np), igbuff(5+2*np),
     +                          igbuff(5+3*np), igbuff(5+4*np),
     +                          igbuff(iout+1) )
C*
                   ELSE IF ( ifunc .eq. FWTHR ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
                    CALL GWTHR ( sys1, igbuff(4), igbuff(5), 
     +                          igbuff(5+np), igbuff(5+2*np),
     +                          igbuff(5+3*np), igbuff(5+4*np),
     +                          igbuff(iout+1) )
C*
                   ELSE IF ( ifunc .eq. FPTND ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
                    CALL GPTND ( sys1, igbuff(4), igbuff(5), 
     +                          igbuff(5+np), igbuff(5+2*np),
     +                          igbuff(5+3*np), igbuff(5+4*np),
     +                          igbuff(iout+1) )
C*
                   ELSE IF ( ifunc .eq. FPWTH ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
                    CALL GPWTH ( sys1, igbuff(4), igbuff(5), 
     +                          igbuff(5+np), igbuff(5+2*np),
     +                          igbuff(5+3*np), igbuff(5+4*np),
     +                          igbuff(iout+1) )
C*
                   ELSE IF ( ifunc .eq. FCTYP ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
                    CALL GCTYP ( sys1, igbuff(4), igbuff(5), 
     +                          igbuff(5+np), igbuff(5+2*np),
     +                          igbuff(5+3*np), igbuff(5+4*np),
     +                          igbuff(iout+1) )
C*
                   ELSE IF ( ifunc .eq. FICNG ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
                    CALL GICNG ( sys1, igbuff(4), igbuff(5), 
     +                           igbuff(5+np), igbuff(5+2*np),
     +                           igbuff(5+3*np), igbuff(5+4*np),
     +                           igbuff(iout+1) )
C*
                   ELSE IF ( ifunc .eq. FSPCL ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
                    CALL GSPCL ( sys1, igbuff(4), igbuff(5), 
     +                           igbuff(5+np), igbuff(5+2*np),
     +                           igbuff(5+3*np), igbuff(5+4*np),
     +                           igbuff(iout+1) )
C*
                   ELSE IF ( ifunc .eq. FTURB ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
                    CALL GTURB ( sys1, igbuff(4), igbuff(5), 
     +                           igbuff(5+np), igbuff(5+2*np),
     +                           igbuff(5+3*np), igbuff(5+4*np),
     +                           igbuff(iout+1) )
C*
		   ELSE IF ( ifunc .eq. FFILL ) THEN
		    isys = igbuff (3)
		    sys1 = sysstr ( isys:isys )
		    np = igbuff (4)
		    CALL GFILL  ( sys1, igbuff (4), igbuff (5), 
     +				  igbuff (5+np), igbuff (iout+1) )
C*
		   ELSE IF ( ifunc .eq. FLOGO ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
		    CALL GLOGO ( sys1, igbuff (4), igbuff (5), 
     +                           igbuff (6), igbuff(7), igbuff(8),
     +				 igbuff (iout+1) )
C*
                   ELSE IF ( ifunc .eq. FCMBO ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            np = igbuff (4)
                    CALL GCMBO ( sys1, igbuff(4), igbuff(5), 
     +                          igbuff(5+np), igbuff(5+2*np),
     +                          igbuff(5+3*np), igbuff(5+4*np),
     +                          igbuff(iout+1) )
C*
	           ELSE IF ( ifunc .eq. FARC ) THEN
		    isys = igbuff (3)
	            sys1 = sysstr ( isys:isys )
	            CALL GARC ( sys1, igbuff (4), igbuff (5),
     +                            igbuff (6), igbuff (7), igbuff (8),
     +                            igbuff (9), igbuff (10), 
     +				  igbuff(iout+2), igbuff (iout + 1) )
	            igbuff (iout) = 6
	         END IF
C------------------------------------------------------------------------
	      ELSE IF ( igroup .eq. ATTRBT ) THEN
C*
	        IF ( ifunc .eq. FSLINE ) THEN
		    CALL GSLINE ( igbuff (3), igbuff (4), igbuff (5), 
     +                            igbuff (6), igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQLINE ) THEN
		    CALL GQLINE ( igbuff (iout+2), igbuff (iout+3), 
     +                            igbuff (iout+4), igbuff (iout+5),
     +                            igbuff (iout+1) )
	            igbuff (iout) = 6
C*
		  ELSE IF ( ifunc .eq. FSTEXT ) THEN
		    CALL GSTEXT ( igbuff (3), igbuff (4), 
     +                            igbuff (5), igbuff (6), 
     +                            igbuff (7), igbuff (8), 
     +                            igbuff (9), igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQTEXT ) THEN
		    CALL GQTEXT ( igbuff (iout+2), igbuff (iout+3), 
     +                            igbuff (iout+4), igbuff (iout+5),
     +                            igbuff (iout+6), igbuff (iout+7),
     +                            igbuff (iout+8), igbuff (iout+1) )
	            igbuff (iout) = 9
C*
		  ELSE IF ( ifunc .eq. FSARRW ) THEN
		    CALL GSARRW ( igbuff (3), igbuff(4), igbuff(5),
     +                            igbuff (6), igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQARRW ) THEN
		    CALL GQARRW ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+4), igbuff (iout+5),
     +                            igbuff (iout+1) )
		    igbuff (iout) = 6
C*
		  ELSE IF ( ifunc .eq. FSDARR ) THEN
		    CALL GSDARR ( igbuff (3), igbuff(4), igbuff(5),
     +                            igbuff (6), igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQDARR ) THEN
		    CALL GQDARR ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+4), igbuff (iout+5),
     +                            igbuff (iout+1) )
		    igbuff (iout) = 6
C*
		  ELSE IF ( ifunc .eq. FSBARB ) THEN
		    CALL GSBARB ( igbuff (3) , igbuff(4), igbuff(5),
     +                            igbuff ( iout + 1) )
		  ELSE IF ( ifunc .eq. FQBARB ) THEN
		    CALL GQBARB ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+4), igbuff (iout+1) )
		    igbuff (iout) = 5
C*
		  ELSE IF ( ifunc .eq. FSHASH ) THEN
		    CALL GSHASH ( igbuff (3), igbuff(4), igbuff(5),
     +                            igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQHASH ) THEN
		    CALL GQHASH ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+4), igbuff (iout+1) )
		    igbuff (iout) = 5
C*
		  ELSE IF ( ifunc .eq. FSMRKR ) THEN
		    CALL GSMRKR ( igbuff (3), igbuff (4), 
     +                            igbuff (5), igbuff (6),
     +                            igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FSTICK ) THEN
		    CALL GSTICK ( igbuff (3), igbuff (4), 
     +                            igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQMRKR ) THEN
		    CALL GQMRKR ( igbuff (iout+2), igbuff (iout+3), 
     +                            igbuff (iout+4), igbuff (iout+5),
     +                            igbuff (iout+1) )
	            igbuff (iout) = 6
C*
		  ELSE IF ( ifunc .eq. FSLPAT ) THEN
		    CALL GSLPAT ( igbuff (3), igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQLPAT ) THEN
		    CALL GQLPAT ( igbuff (iout+2), 
     +				  igbuff (iout+1) )
	            igbuff (iout) = 10
C*
		  ELSE IF ( ifunc .eq. FQSYSZ ) THEN
		    CALL GQSYSZ ( igbuff (iout+2), igbuff (iout+3), 
     +                            igbuff (iout+4), igbuff (iout+5),
     +	                          igbuff (iout+6), igbuff (iout+7),
     +	                          igbuff (iout+1) )
	            igbuff (iout) = 8
C*
		  ELSE IF ( ifunc .eq. FQSIZD ) THEN
		    CALL GQSIZD ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+4), igbuff (iout+5),
     +	                          igbuff (iout+6), igbuff (iout+7),
     +	                          igbuff (iout+1) )
		    igbuff (iout) = 8
C*
                  ELSE IF ( ifunc .eq. FSSKY ) THEN
                    CALL GSSKY ( igbuff (3) , igbuff(4), igbuff(5),
     +                           igbuff ( iout + 1 ) )
                  ELSE IF ( ifunc .eq. FQSKY ) THEN
                    CALL GQSKY ( igbuff (iout+2), igbuff (iout+3),
     +                           igbuff (iout+4), igbuff (iout+1) )
                    igbuff (iout) = 5
C*
                  ELSE IF ( ifunc .eq. FSWTHR ) THEN
		    CALL GSWTHR ( igbuff (3), igbuff (4), 
     +                            igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQWTHR ) THEN
		    CALL GQWTHR ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+1) )
                    igbuff (iout) = 4
C*
		  ELSE IF ( ifunc .eq. FSFILL ) THEN
		    CALL GSFILL ( igbuff (3), igbuff (4), 
     +				  igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQFILL ) THEN
		    CALL GQFILL ( igbuff (iout+2), igbuff (iout+3),
     +				  igbuff (iout+1) )
		    igbuff (iout) = 4
C*
		  ELSE IF ( ifunc .eq. FSSMTH ) THEN
		    CALL GSSMTH ( igbuff (3), igbuff (4),
     +				  igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQSMTH ) THEN
		    CALL GQSMTH ( igbuff (iout+2), igbuff (iout+3),
     +				  igbuff (iout+1) )
		    igbuff (iout) = 4
C*
		  ELSE IF ( ifunc .eq. FSRDUC ) THEN
		    CALL GSRDUC ( igbuff (3), igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQRDUC ) THEN
		    CALL GQRDUC ( igbuff (iout+2), igbuff (iout+1) )
		    igbuff (iout) = 3
C*
		  ELSE IF ( ifunc .eq. FSDASH ) THEN
	            CALL GSDASH ( igbuff (3), igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQDASH ) THEN
		    CALL GQDASH ( igbuff (iout+2), igbuff (iout+1) )
		    igbuff (iout) = 3
C*
                  ELSE IF ( ifunc .eq. FSPTND ) THEN
		    CALL GSPTND ( igbuff (3), igbuff (4), 
     +                            igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQPTND ) THEN
		    CALL GQPTND ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+1) )
                    igbuff (iout) = 4
C*
                  ELSE IF ( ifunc .eq. FSPWTH ) THEN
		    CALL GSPWTH ( igbuff (3), igbuff (4), 
     +                            igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQPWTH ) THEN
		    CALL GQPWTH ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+1) )
                    igbuff (iout) = 4
C*
                  ELSE IF ( ifunc .eq. FSCTYP ) THEN
		    CALL GSCTYP ( igbuff (3), igbuff (4), 
     +                            igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQCTYP ) THEN
		    CALL GQCTYP ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+1) )
                    igbuff (iout) = 4
C*
                  ELSE IF ( ifunc .eq. FSICNG ) THEN
		    CALL GSICNG ( igbuff (3), igbuff (4), 
     +                            igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQICNG ) THEN
		    CALL GQICNG ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+1) )
                    igbuff (iout) = 4
C*
                  ELSE IF ( ifunc .eq. FSSPCL ) THEN
		    CALL GSSPCL ( igbuff (3), igbuff (4), 
     +                            igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQSPCL ) THEN
		    CALL GQSPCL ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+1) )
                    igbuff (iout) = 4
C*
                  ELSE IF ( ifunc .eq. FSTURB ) THEN
		    CALL GSTURB ( igbuff (3), igbuff (4), 
     +                            igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQTURB ) THEN
		    CALL GQTURB ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+1) )
                    igbuff (iout) = 4
C*
	          ELSE IF ( ifunc .eq. FSFRNT ) THEN
		    CALL GSFRNT ( igbuff (3), igbuff (4), igbuff (5), 
     +		                  igbuff (6),  
     +				  igbuff (iout+1) )

		  ELSE IF ( ifunc .eq. FQFRNT ) THEN
		    CALL GQFRNT ( igbuff (iout+2), igbuff (iout+3), 
     +                            igbuff (iout+4), igbuff (iout+5),
     +                            igbuff (iout+1) )
	            igbuff (iout) = 6
C*
	          ELSE IF ( ifunc .eq. FSSPLN ) THEN
		    CALL GSSPLN ( igbuff (3), igbuff (4), igbuff (5), 
     +				  igbuff (6), igbuff (7),
     +				  igbuff (iout+1) )

		  ELSE IF ( ifunc .eq. FQSPLN ) THEN
		    CALL GQSPLN ( igbuff (iout+2), igbuff (iout+3), 
     +				  igbuff (iout+4), igbuff (iout+5),
     +				  igbuff (iout+6), igbuff (iout+1) )
	            igbuff (iout) = 7
C*
		  ELSE IF ( ifunc .eq. FQDATT ) THEN
		    CALL GQDATT ( igbuff (iout+2), filnam,
     +				  igbuff (iout+21), igbuff (iout+22),
     +				  igbuff (iout+23), igbuff (iout+24),
     +				  igbuff (iout+1) )
		    CALL ST_STOI ( filnam, 72, nv, igbuff (iout+3),
     +				   ier )
		    igbuff (iout) = 25
C*
		  ELSE IF ( ifunc .eq. FQCVSC )  THEN
		    CALL GQCVSC ( igbuff (iout+2), igbuff (iout+1) )
		    igbuff (iout) = 3
C*
                  ELSE IF ( ifunc .eq. FSCMBO) THEN
		    CALL GSCMBO( igbuff (3), igbuff (4), 
     +                            igbuff (iout+1) )
		  ELSE IF ( ifunc .eq. FQCMBO) THEN
		    CALL GQCMBO( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+1) )
                    igbuff (iout) = 4
C*
	        END IF
C------------------------------------------------------------------------
	      ELSE IF ( igroup .eq. MAP ) THEN
C*
	        IF ( ifunc .eq. FSMMAP ) THEN
		    CALL ST_ITOS  ( igbuff (3), 1, nc, cprj, ier )
	            CALL GSMMAP ( cprj, igbuff (4), igbuff (5), 
     +                            igbuff (6), igbuff (7), 
     +                            igbuff (iout+1) )
C*
	          ELSE IF ( ifunc .eq. FSMPRJ ) THEN
		    CALL ST_ITOS  ( igbuff (3), 1, nc, cprj, ier )
	            CALL GSMPRJ ( cprj, igbuff (4), igbuff (5),
     +                            igbuff (6), igbuff (7), igbuff (8),
     +                            igbuff (9), igbuff (10), 
     +                            igbuff (iout+1) )
C*
	          ELSE IF ( ifunc .eq. FSMMGN ) THEN
	            CALL GSMMGN ( igbuff (3), igbuff (4), igbuff (5),
     +                            igbuff (6), igbuff (iout+1) )
C*
	          ELSE IF ( ifunc .eq. FSMFIL ) THEN
	            nchar = igbuff (3)
		    nwd   = ( nchar - 1 ) / 4 + 1
		    CALL ST_ITOS  ( igbuff (4), nwd, nc, fil, ier )
	            CALL GSMFIL ( fil, igbuff (iout+1) )
C*
	          ELSE IF ( ifunc .eq. FQMMAP ) THEN
	            CALL GQMMAP ( cprj, igbuff (iout+3),
     +                            igbuff (iout+4), igbuff (iout+5),
     +                            igbuff (iout+6), igbuff (iout+1) )
	            CALL ST_STOI ( cprj, 4, nv, igbuff (iout+2), ier )
	            igbuff (iout) = 7
C*
	          ELSE IF ( ifunc .eq. FQMPRJ ) THEN
	            CALL GQMPRJ ( cprj, igbuff (iout+3),
     +                            igbuff (iout+4), igbuff (iout+5),
     +                            igbuff (iout+6), igbuff (iout+7),
     +                            igbuff (iout+8), igbuff (iout+9),
     +                            igbuff (iout+1) )
	            CALL ST_STOI ( cprj, 4, nv, igbuff (iout+2), ier )
	            igbuff (iout) = 10
C*
	          ELSE IF ( ifunc .eq. FQMMGN ) THEN
	            CALL GQMMGN ( igbuff (iout+2), igbuff (iout+3),
     +                            igbuff (iout+4), igbuff (iout+5),
     +                            igbuff (iout+1) )	
                    igbuff (iout) = 6
C*
		  ELSE IF ( ifunc .eq. FQMFIL ) THEN
		    CALL GQMFIL ( mapnam, igbuff (iout+1) )
		    CALL ST_STOI ( mapnam, 80, nv, igbuff (iout+2), ier )
		    igbuff (iout) = 2 + 20 
		END IF
C------------------------------------------------------------------------
	      ELSE IF ( igroup .eq. GRAPH ) THEN
C*
		IF ( ifunc .eq. FSGRAF ) THEN
		    CALL GSGRAF ( igbuff (3), igbuff (4), igbuff (5),
     +				  igbuff (6), igbuff (7), igbuff (8),
     +				  igbuff (9), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FSGMGN ) THEN
		    CALL GSGMGN  ( igbuff (3), igbuff (4), igbuff (5),
     +				   igbuff (6), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FDAXIS ) THEN
		    CALL GDAXIS ( igbuff (3), igbuff (4), igbuff (5),
     +				  igbuff (6), igbuff (7), igbuff (8),
     +				  igbuff (9), igbuff (10), igbuff (11),
     +				  igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FAAXIS ) THEN
C
C*		    np   = the number of elements in the array
C*		    nwrd = the maximum number of words per element in the 
C*		           character array
C
		    np = igbuff (9)
		    nwrd = igbuff (10+np)
		    DO i = 1, np
			chrary (i) = ' '
			j = ( (i - 1) * nwrd )
		    CALL ST_ITOS ( igbuff (11+np+j), nwrd, nc,
     +                                 chrary (i), ier) 
		    END DO
		    CALL GAAXIS ( igbuff (3), igbuff (4), igbuff (5),
     +				  igbuff (6), igbuff (7), igbuff (8),
     +				  np,       igbuff (10), chrary,
     +				  igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FQGRAF ) THEN
		    CALL GQGRAF ( igbuff (iout+2), igbuff (iout+3), 
     +			 igbuff (iout+4), igbuff (iout+5), 
     +			 igbuff (iout+6), igbuff (iout+7), 
     +                   igbuff (iout+8), igbuff (iout+1) )
		    igbuff(iout) = 9
C*
		  ELSE IF ( ifunc .eq. FQGMGN ) THEN
		    CALL GQGMGN ( igbuff (iout+2), igbuff (iout+3), 
     +			 igbuff (iout+4), igbuff (iout+5), 
     +		         igbuff (iout+1) ) 
	            igbuff (iout) = 6
		END IF
C------------------------------------------------------------------------
	      ELSE IF ( igroup .eq. GRID ) THEN
C*
		IF ( ifunc .eq. FSGMAP ) THEN
		    CALL ST_ITOS ( igbuff (3), 1, nc, cprj, ier )
		    CALL GSGMAP ( cprj, igbuff (4), igbuff (5),
     +				  igbuff (6), igbuff (7), igbuff (8),
     +				  igbuff (9), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FSGPRJ ) THEN
		    CALL ST_ITOS ( igbuff (3), 1, nc, cprj, ier )
		    CALL GSGPRJ ( cprj, igbuff (4), igbuff (5),
     +				  igbuff (6), igbuff (7), igbuff (8),
     +				  igbuff (9), igbuff (10), igbuff (11),
     +				  igbuff (12), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FSGGRF ) THEN
		    CALL GSGGRF ( igbuff (3), igbuff (4), igbuff (5),
     +				  igbuff (6), igbuff (7), igbuff (8) ,  
     +				  igbuff (9), igbuff (10), 
     +				  igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FQGMAP ) THEN
		    CALL GQGMAP ( cprj, igbuff (iout+3),
     +                            igbuff (iout+4),
     +				  igbuff (iout+5), igbuff (iout+6), 
     +				  igbuff (iout+7), igbuff (iout+8),
     +				  igbuff (iout+1) )
		    CALL ST_STOI ( cprj, 4, nv, igbuff (iout+2), ier )
		    igbuff (iout) = 9
C*
		  ELSE IF ( ifunc .eq. FQGPRJ ) THEN
		    CALL GQGPRJ ( cprj,             igbuff (iout+3), 
     +				  igbuff (iout+4),  igbuff (iout+5),
     +				  igbuff (iout+6),  igbuff (iout+7),
     +				  igbuff (iout+8),  igbuff (iout+9),
     +				  igbuff (iout+10), igbuff (iout+11),
     +				  igbuff (iout+1) )
		    CALL ST_STOI ( cprj, 4, nv, igbuff (iout+2), ier )
		    igbuff (iout) = 12
C*
		  ELSE IF ( ifunc .eq. FQGGRF ) THEN
		    CALL GQGGRF ( igbuff (iout+2),  igbuff (iout+3), 
     +				  igbuff (iout+4),  igbuff (iout+5),
     +				  igbuff (iout+6),  igbuff (iout+7),
     +				  igbuff (iout+8),  igbuff (iout+9),
     +				  igbuff (iout+1) )
		    igbuff (iout) = 10
		END IF
C------------------------------------------------------------------------
	      ELSE IF ( igroup .eq. UTILTY ) THEN
C*
	        IF ( ifunc .eq. FDRMAP ) THEN
	            CALL GDRMAP ( igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FGTMAP ) THEN
		    mxelts = igbuff (4)
		    mxpts  = igbuff (5)
		    CALL GETMAP ( igbuff (3), igbuff (4), igbuff (5),
     +                            nelts, np, igbuff (iout+4),
     +                            igbuff (iout+4+mxelts),
     +                            igbuff (iout+4+mxelts+mxpts),
     +                            igbuff (iout+1)  )
		    igbuff (iout) = 4 + nelts + 2 * np
		    igbuff (iout+2) = nelts
		    igbuff (iout+3) = np
C
C*                  Shift the arrays to reduce the number of elements
C*                  that need to be sent back.  This way, we only
C*                  send arrays that are NELTS and NP long, instead of
C*                  MXELTS and MXPTS long.
C
		    DO i = 1, np
			igbuff (iout+4+nelts+i-1) =
     +                          igbuff (iout+4+mxelts+i-1)
		    END DO
		    DO i = 1, np
			igbuff (iout+4+nelts+np+i-1) =
     +                          igbuff (iout+4+mxelts+mxpts+i-1)
		    END DO
C*
	          ELSE IF ( ifunc .eq. FDRGRD ) THEN
	            CALL GDRGRD ( igbuff (3), igbuff (4), igbuff (5), 
     +                            igbuff(7), igbuff(8), igbuff(9),
     +                            igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FSTRML ) THEN
		    np =  (igbuff (1) - 14) / 2
		    CALL GSTRML ( igbuff(3), igbuff (4), igbuff (5), 
     +                            igbuff (5+np), igbuff ((2*np)+5), 
     +                            igbuff ((2*np)+6), igbuff ((2*np)+7), 
     +                            igbuff ((2*np)+8), igbuff ((2*np)+9), 
     +                            igbuff ((2*np)+10),igbuff ((2*np)+11),
     +                            igbuff ((2*np)+12),igbuff ((2*np)+13),
     +                            igbuff ((2*np)+14),
     +                            igbuff (iout+1) )
C*
		  ELSE IF (ifunc .eq. FFLBND ) THEN
                    num = igbuff (1) - 14 - 2
                    cbuf = ' '
		    numwbt = igbuff (3)
                    CALL ST_ITOS ( igbuff (4), numwbt, ncbt, cbuf, ier )
                    cbuf1 = ' '
		    numst = igbuff (2+1+numwbt+9)
                    CALL ST_ITOS ( igbuff (2+1+numwbt+10), numst, ncst, 
     +			cbuf1, ier )
		    CALL GPLBND ( cbuf, igbuff (3+numwbt+1), 
     +   		igbuff (3+numwbt+2), igbuff (3+numwbt+3), 
     +                  igbuff (3+numwbt+4), igbuff (3+numwbt+5), 
     +                  igbuff (3+numwbt+6), igbuff (3+numwbt+7),
     +                  igbuff (3+numwbt+8), cbuf1, 
     +                  igbuff (3+numwbt+10+numst), 
     +                  igbuff (3+numwbt+10+numst+1), 
     +                  igbuff (3+numwbt+10+numst+2),
     +                  igbuff (3+numwbt+10+numst+3), 
     +                  igbuff (iout+1) )
		END IF
C------------------------------------------------------------------------
	      ELSE IF ( igroup .eq. COLORS) THEN
C*
		IF ( ifunc .eq. FSCOLR ) THEN
		    CALL GSCOLR ( igbuff (3), igbuff (iout+1) )
C*
		ELSE IF ( ifunc .eq. FSCLR2 ) THEN
		    CALL GSCLR2 ( igbuff (3), igbuff (4),
     +			          igbuff (iout+1) )
C*
		ELSE IF ( ifunc .eq. FSCTBL ) THEN
		    CALL ST_ITOS  ( igbuff (3), 16, nc, ctblnm, ier )
		    CALL GSCTBL ( ctblnm, igbuff (iout+1) )
C*
		ELSE IF ( ifunc .eq. FSCOLB ) THEN
		    CALL GSCOLB ( igbuff (3), igbuff (4),
     +				  igbuff (iout+1) )
C* ##################################################################### 
		  ELSE IF ( ifunc .eq. FSBRGB ) THEN
		    nc = igbuff (4)
		    CALL GSBRGB ( igbuff (3), igbuff (4),
     +				  igbuff (5),
     +                            igbuff (5+nc),
     +                            igbuff (5+2*nc),
     +                            igbuff (5+3*nc), igbuff (iout+1) )
C* ##################################################################### 
C*
		  ELSE IF ( ifunc .eq. FQCOLR ) THEN
		    CALL GQCOLR ( igbuff (iout+2), igbuff (iout+1) )
		    igbuff (iout) = 3
C*
		  ELSE IF ( ifunc .eq. FQCLR2 ) THEN
		    CALL GQCLR2 ( igbuff (iout+2), igbuff (iout+3), 
     +    	                  igbuff (iout+1) )
		    igbuff (iout) = 4
C*
		  ELSE IF ( ifunc .eq. FQNCOL ) THEN
		    CALL GQNCOL ( igbuff (iout+2), igbuff (iout+1) )
		    igbuff (iout) = 3
C*
		  ELSE IF ( ifunc .eq. FQCLRS ) THEN
		    CALL GQCLRS ( igbuff (3), igbuff (iout+2),
     +				  igbuff (iout+1) )
		    igbuff (iout) = 3
C*
	          ELSE IF ( ifunc .eq. FQCOMP ) THEN
	            CALL GQCOMP ( igbuff (3), cbuf, igbuff (iout+22),
     +                            igbuff (iout+23), igbuff (iout+24),
     +                            xname, igbuff (iout+1) )
	            CALL ST_STOI ( cbuf, 80, nv, igbuff (iout+2), ier )
		    CALL ST_STOI ( xname, 80, nv, igbuff (iout+25),
     +				   ier )
	            igbuff (iout) = 45
C*
	          ELSE IF ( ifunc .eq. FSCINT ) THEN
	            CALL GSCINT ( igbuff (iout+1) )
C*
	          ELSE IF ( ifunc .eq. FSCNAM ) THEN
	            nc = igbuff (4)
		    nwd = nc / 4
		    CALL ST_ITOS  ( igbuff (5), nwd, nc, cbuf, ier )
	            CALL GSCNAM ( igbuff (3), cbuf, igbuff (iout+1) )
C*
	          ELSE IF ( ifunc .eq. FSCRGB ) THEN
	            CALL GSCRGB ( igbuff (3), igbuff (4), igbuff (5),
     +                            igbuff (6), igbuff (iout+1) )
	        END IF
C------------------------------------------------------------------------
	      ELSE IF ( igroup .eq. BOUNDS ) THEN
C*
		IF  ( ifunc .eq. FSVIEW ) THEN
		    CALL GSVIEW ( igbuff (3), igbuff (4), igbuff (5),
     +				  igbuff (6), igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FQVIEW ) THEN
		    CALL GQVIEW ( igbuff (iout+2), igbuff (iout+3),
     +				  igbuff (iout+4), igbuff (iout+5),
     +				  igbuff (iout+1) )
		    igbuff (iout) = 6
C*
		  ELSE IF ( ifunc .eq. FQBND ) THEN
		    isys = igbuff (3)
		    sys1 = sysstr ( isys:isys )
		    CALL GQBND ( sys1, igbuff (iout+2), igbuff (iout+3),
     +				 igbuff (iout+4), igbuff (iout+5),
     +				 igbuff (iout+1) )
		    igbuff (iout) = 6
		END IF
C----------------------------------------------------------------------------
	      ELSE IF ( igroup .eq. TRNSFM ) THEN
C*
		IF ( ifunc .eq. FTRANS ) THEN
		    isys = igbuff (3)
		    sys1 = sysstr ( isys:isys )
		    isys = igbuff (4)
		    sys2 = sysstr ( isys:isys )
		    np = igbuff (5)
		    CALL GTRANS ( sys1, sys2, np, igbuff (6),
     +                            igbuff (6+np),
     +				  igbuff (iout+2), igbuff(iout+2+np),
     +				  igbuff (iout+1) )
		    igbuff (iout) = 2 * np + 2
C*
		  ELSE IF ( ifunc .eq. FPTVIS ) THEN
		    isys = igbuff (3)
		    sys1 = sysstr ( isys:isys )
		    np   = igbuff (4)
		    CALL GPTVIS ( sys1, np, igbuff (5), igbuff (5+np),
     +				  igbuff (iout+2), igbuff (iout+1) )
		    igbuff (iout) = np + 2
C*
		END IF
C------------------------------------------------------------------------
	      ELSE IF ( igroup .eq. CURVE ) THEN
C*
		IF ( ifunc .eq. FCYEVL ) THEN
		    np = igbuff (4)
		    npout = igbuff (5+2*np)
		    CALL GCYEVL ( igbuff (3), np, igbuff (5),
     +                            igbuff (5+np),
     +				  npout, igbuff (6+2*np),
     +				  igbuff (iout+2), igbuff (iout+1) )
		    igbuff (iout) = npout + 2
		  ELSE IF  ( ifunc .eq. FCURVE )  THEN
		    np = igbuff (4)
		    npout = igbuff (5+2*np)
		    CALL GCURVE  ( igbuff (3), np, igbuff (5),
     +                             igbuff (5+np),
     +				   npout, igbuff (6+2*np),
     +				   igbuff (iout+2), igbuff (iout+1) )
		    igbuff (iout) = npout + 2
		END IF
C------------------------------------------------------------------------
	      ELSE IF ( igroup .eq. SATLTE ) THEN
C*
		  IF ( ifunc .eq. FQSATN ) THEN
		    CALL GQSATN ( navtyp, imgnam, igbuff (iout+1) )
		    CALL ST_STOI ( navtyp, 8, nv, igbuff (iout+2), 
     +				   ier )
		    CALL ST_STOI ( imgnam, 256, nv, igbuff (iout+4), 
     +				   ier )
		    igbuff (iout) = 68
C*
                  ELSE IF ( ifunc .eq. FSATMG ) THEN
		    CALL ST_ITOS  ( igbuff (3), 64, nc, imgnam, ier )
                    IF ( goe4 .eq. 'GOE4' ) THEN
                       CALL GSATMG4 ( imgnam, igbuff (67),
     +				  igbuff (67 + 64),
     +				  igbuff (67 + 64 + 640 ),
     +				  igbuff (67 + 64 + 640 + 1),
     +				  igbuff (67 + 64 + 640 + 2),
     +				  igbuff (67 + 64 + 640 + 3),
     +				  igbuff (iout+1) )
                    ELSE
                       CALL GSATMG ( imgnam, igbuff (67),
     +                            igbuff (67 + 64),
     +                            igbuff (67 + 64 + 640 ),
     +                            igbuff (67 + 64 + 640 + 1),
     +                            igbuff (67 + 64 + 640 + 2),
     +                            igbuff (67 + 64 + 640 + 3),
     +                            igbuff (iout+1) )
                    END IF        
C*
                  ELSE IF ( ifunc .eq. FSATIM ) THEN
		    CALL ST_ITOS  ( igbuff (3), 33, nc, satfil, ier )
		    CALL GSATIM ( satfil, igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FSICMN ) THEN
		    DO ic = 1, NIMCMN
		       icmnarr (ic) = igbuff (ic + 2)
		    END DO
		    CALL GSICMN ( igbuff (iout+1) )
C*
		  ELSE IF ( ifunc .eq. FSATPX ) THEN
		    isys = igbuff (3)
		    sys1 = sysstr(isys:isys) 
		    CALL ST_ITOS  ( igbuff (8), 33, nc, satfil, ier )
		    CALL GSATPX ( sys1, igbuff (4), igbuff (5), 
     +				  igbuff (6), igbuff (7), satfil,
     +				  igbuff ( 8 + 33 ), 
     +				  igbuff ( 8 + 33 + 1 ),
     +				  igbuff (iout + 2),
     +				  igbuff (iout + 3),
     +				  igbuff (iout + 4),
     +				  igbuff (iout + 1) )
		    igbuff (iout) = 5
C*
		END IF
C---------------------------------------------------------------------
	      ELSE
	    END IF
C
C*	    Send the return buffer to the applications program.
C
	    CALL GPUTB  ( igbuff (iout), iret )
	END DO
C
C*     Free semaphore lock on IPC message queues.
C
       mproc = 0
       CALL GSFREE  ( mproc, isemid, ifree, iret )
       IF ( ifree .ne. 1 ) THEN
           CALL ER_WMSG  ( 'GPLT', -2, ' ', iret )
           STOP
       END IF
C*
	END
