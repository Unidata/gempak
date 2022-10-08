 	PROGRAM DEVICE
C************************************************************************
C* PROGRAM DEVICE							*
C*									*
C* This module is the main program for the DEVICE task.  It receives	*
C* information from the GPLT task program and calls the appropriate	*
C* DEVICE subroutines with identical names and calling sequences as	*
C* the GPLT modules.							*
C*									*
C**									*
C* Log:									*
C* M. Vilardo/RDS	 4/85	GEMPLT 3.1				*
C* M. desJardins/GSFC	 4/85	GEMPLT 3.1				*
C* S. Schotz/GSC	 1/90	Added parameters for dsbarb, dsarrw,	*
C*				dsmrkr and dstext			*
C* S. Schotz/GSC	 2/90	Added call to DINIT			*
C* S. Schotz/GSC	 2/90	Removed call to DINIT			*
C* S. Schotz/GSC	 3/90	Added cloud/weather symbol routines	*
C* S. Schotz/GSC	 8/90	Added arrow head size			*
C* S. Schotz/GSC	 8/90	Updated for changes in DSLPAT,DQLPAT	*
C* M. desJardins/NMC	 7/91	Added DQDEV				*
C* M. desJardins/NMC	11/91	Added DFILL				*
C* M. desJardins/NMC	 1/92	Added DQDCHR				*
C* J. Whistler/GSFC	 1/92	Added event handling			*
C* S. Jacobs/EAI	 6/93	Added DGTPNT				*
C* S. Jacobs/EAI	 9/93	Added ITYP for DGTPNT			*
C* A. Chang/EAI	 	12/93	Added DSCTBL, DSFLNM, DSPLOT, DSPIXM,	*
C*				DEPIXM, DLOOPC, DSHCAT, DSATIM		*
C* S. Jacobs/NMC	 3/94	Fixed DSATIM - added satfil		*
C* S. Jacobs/NMC	 3/94	Renamed: DSPIXM ==> DSTANM		*
C*					 DEPIXM ==> DENANM		*
C*					 DSHCAT ==> DSDATT		*
C* A. Chang/EAI	 	 8/94	Added DTEXTC				*
C* M. desJardins/NMC	 8/94	Eliminated check after gsanmn, gsplot;	*
C*				added check after gloopc		*
C* J. Cowie/COMET	12/94	Added DSLUTF - set LUT file		*
C* A. Chang/EAI		 1/95	Added DSLUTF input parameter "itype"	*
C* J. Cowie/COMET	 1/95	Added image subsetting			*
C* J. Cowie/COMET	 3/95	Added DSICMN()				*
C* J. Cowie/COMET	 5/95	Added DSCOLB()				*
C* C. Lin/EAI		 6/95	Added DSBRGB()				*
C* G. Krueger/EAI	11/95	Removed HLS;Added XNAME;Mod. RGB range	*
C* J. Cowie/COMET	11/95	Remove DSLUTF, added DQCLRS		*
C* M. Linda/GSC		 3/96	Generalized the interface to GPLT	*
C* M. Linda/GSC		 3/96	Added DINITA, modified DSDATT call	*
C* L. Williams/EAI	 3/96	Increased buffer size for func. CSCNAM	*
C* S. Jacobs/NCEP	 4/96	Added iunit to DSDATT			*
C* S. Jacobs/NCEP	 5/96	Added DSLWIN and DQDATT			*
C* S. Jacobs/NCEP	 5/96	Removed DINITD and DSFLNM		*
C* M. Linda/GSC		 6/96	Modified DLINE calling sequence		*
C* M. Linda/GSC		 8/96	Added xICNG, xSPCL, and xTURB		*
C* E. Wehner/EAi	10/96	Added xFRNT				*
C* S. Jacobs/NCEP	10/96	Modified call to DINITA to include cdev	*
C* S. Maxwell/GSC	11/96	Removed save and restore functions	*
C* E. Wehner/EAi	11/96	Elim. param for xFRNT			*
C* M. Linda/GSC		 1/97	Changed from txt*500 to txt*400		*
C* M. Linda/GSC		 2/97	Changed DFILL, removed DFLUSH		*
C* K. Tyle/GSC		 2/97	Added DCLPNL				*
C* D. Keiser/GSC	 3/97	Added xSPLN				*
C* E. Safford/GSC	 4/97	Added code for GTXSY			*
C* E. Safford/GSC	 6/97	Added code for GxTXSY			*
C* C. Lin/EAI            6/97   Added code for DSROAM, DROAM            *
C* S. Maxwell/GSC	 6/97	Added DSFILL               		*
C* S. Maxwell/GSC	 7/97	Added DSGRP and DEGRP          		*
C* S. Safford/GSC	 7/97	Added additional params for GTXSY	*
C* M. Linda/GSC		 7/97	Added DLOGO				*
C* S. Jacobs/NCEP	 9/97	Removed DSTXSY				*
C* S. Jacobs/NCEP	 9/97	Changed call to DSTEXT			*
C* S. Jacobs/NCEP	 2/98	Added DSSMTH				*
C* S. Jacobs/NCEP        3/98   Added DSDASH                            *
C* I. Durham/GSC	 3/98 	Added DDARR and DSDARR			*
C* I. Durham/GSC	 3/98	Added DHASH and DSHASH			*
C* I. Durham/GSC	 3/98   Added DSCLR2				*
C* S. Jacobs/NCEP	 6/98	Removed NP from GGTPNT call		*
C* S. Jacobs/NCEP	 6/98	Removed DOUTP				*
C* S. Jacobs/NCEP	 6/98	Moved DSCTBL from DCNTRL to DCOLOR	*
C* S. Jacobs/NCEP	 7/98	Changed call to DTEXTC			*
C* A. Hardy/GSC         10/98   Added DCMBO and DSCMBO                  *
C* A. Hardy/GSC         11/98   Added calling parameter to DCIRCL       *
C* S. Jacobs/NCEP	 5/99	Added DSRDUC				*
C* T. Lee/GSC		 9/99	Added DSATPX				*
C* T. Lee/GSC		12/99	Added pixel area and mode to DSATPX	*
C* A. Hardy/GSC		 5/00   Added calling parameter to DLOGO	*
C* A. Hardy/GSC          6/00   Added DARC				*
C* J. Wu/GSC		 3/01   Added logo emblem ID 			*
C* D. Kidwell/NCEP	 6/02	Added DSGTGN                            *
C* D.W.Plummer/NCEP	 6/04	Chgs for DSATPX				*
C* R. McTaggart-Cowan/SUNY 01/05 Added semaphore locks for IPC queues	*
C* C. Bailey		 1/05	Added DGSAVE				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GBUFFD.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'IMGDEF.CMN'
C*
	LOGICAL		done, check
	CHARACTER	dev*12, cdev*12, txt*400, msg*64, filnam*80,
     +			wname*72, satfil*132, clrnm*80, xname*80,
     +			ctblnm*64
C
	INTEGER		  icmnarr (NIMCMN)
	EQUIVALENCE	( icmnarr, imftyp )
C
	INTEGER		  idevchr (NDVCHR)
	EQUIVALENCE	( idevchr, nncolr )
C------------------------------------------------------------------------
C
C*	Connect this DEVICE subprocess to the GPLT message queue.
C
	mproc = 1
	CALL GIPROC ( mproc, mbchnd, iret )
C
	IF ( iret .eq. NORMAL ) THEN
	    done = .false.
	ELSE
	    done = .true.
	END IF
C
C*	Initialize the message queue buffer.
C
	jrtypd = 1
	jwtypd = 2
C
C*	Obtain semaphore lock on IPC message queues.
C
	mproc = 1
	CALL GSGRAB  ( mproc, igrab, isemid, iret )
	IF ( igrab .ne. 1 ) THEN
	   CALL ER_WMSG  ( 'DEVICE', -1, ' ', iret )
	   iout = igbufd (1)
	   igbufd (iout+1) = 2
	   CALL GPUTC ( igbufd (iout+1), iret )
	   STOP
	ENDIF
C
C*	Loop until DENDD requests that task be terminated.
C
	check = .false.
C
	DO WHILE ( .not. done )
C
C*	    Check for event; handle it, or read next message from queue.
C
	    CALL DEVENT ( check, iret )
	    check = .false.
C
C	    A message was received and igbufd looks as follows:
C		igbufd (1) = number of words in buffer
C		igbufd (2) = function code
C		igbufd (3) = data (depends on function code)
C		igbufd (:) = data
C		igbufd (:) = data
C
	    ifunc  = igbufd (2)
	    igroup = ifunc / 100
C
C*	    Determine the pointer to the output buffer.
C	    The output buffer follows the input buffer.  Since igbufd (1)
C	    is the number of words in input buffer, the output buffer
C	    beginning is one past what igbufd (1) points to.
C
	    iout = igbufd (1)
C
C	    The output buffer is defined as follows:
C		igbufd (iout+1) = number of words in buffer
C		igbufd (iout+2) = return code
C		igbufd (iout+3) = data (depends on function code)
C		igbufd (  :   ) = data
C		igbufd (  :   ) = data
C
C*	    Initialize the output buffer.
C
	    igbufd (iout+1) = 2
	    igbufd (iout+2) = NFNCCD
C
C*	    Branch to a subroutine using the group and function codes.
C------------------------------------------------------------------------
	    IF  ( ifunc .eq. -999 ) THEN
		igbufd (iout+2) = -999
C------------------------------------------------------------------------
	    ELSE IF ( igroup .eq. DACCES ) THEN
C
		IF ( ifunc .eq. CINITA ) THEN
		    CALL ST_ITOS ( igbufd (3),  3, nc, dev, ier )
		    CALL ST_ITOS ( igbufd (6),  3, nc, cdev, ier )
		    CALL ST_ITOS ( igbufd (10), 18, nc, filnam, ier )
		    CALL DINITA ( dev, cdev, igbufd (9), filnam,
     +				  igbufd (28), igbufd (29),
     +				  igbufd (30), igbufd (iout+3),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 3
		    IF ( ( igbufd (iout+2) .ne. NORMAL ) .and.
     +			 ( igbufd (iout+2) .ne. NEWWIN ) ) done = .true.
C
		ELSE IF ( ifunc .eq. CENDD ) THEN
		    IF ( igbufd (3) .eq. 1 ) done = .true.
		    CALL DENDD ( igbufd (3), igbufd (iout+2) )
		    check = .true.
C
		ELSE IF ( ifunc .eq. CINITP ) THEN
		    CALL DINITP ( igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CQDCHR ) THEN
		    igbufd (iout+1) = 2 + NDVCHR
		    igbufd (iout+2) = NORMAL
		    DO ic = 1, NDVCHR
			igbufd (iout+2+ic) = idevchr (ic)
		    END DO
		END IF
C------------------------------------------------------------------------
	    ELSE IF ( igroup .eq. DCNTRL ) THEN
C
	        IF ( ifunc .eq. CCLEAR ) THEN
		    CALL DCLEAR ( igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CEPLOT ) THEN
		    CALL DEPLOT ( igbufd (iout+2) )
		    check = .true.
C
                ELSE IF ( ifunc .eq. CSGRP ) THEN
                    CALL DSGRP ( igbufd (3), igbufd (iout+2) )
C
                ELSE IF ( ifunc .eq. CEGRP ) THEN
                    CALL DEGRP ( igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CCLOSP ) THEN
		    CALL DCLOSP ( igbufd (iout+3), igbufd (iout+2) )
		    igbufd (iout+1) = 3
C
		ELSE IF ( ifunc .eq. CQDEV ) THEN
		    CALL DQDEV ( dev, igbufd (iout+6), igbufd (iout+7),
     +				 igbufd (iout+2) )
		    CALL ST_STOI ( dev, 12, nv, igbufd (iout+3), ier )
		    igbufd (iout+1) = 7
C
		ELSE IF ( ifunc .eq. CMESG ) THEN
		    CALL ST_ITOS ( igbufd (3), 16, nc, msg, ier )
		    CALL DMESG ( msg, igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CGTPNT ) THEN
		    ityp = igbufd (3)
		    IF  ( ityp .eq. 1 )  THEN
			np = 1
		      ELSE
			np = 2
		    END IF
		    CALL DGTPNT ( ityp, igbufd (iout+3),
     +				  igbufd (iout+3+np ), igbufd (iout+2) )
		    igbufd (iout+1) = 2 + 2*np
C
		ELSE IF ( ifunc .eq. CSPLOT ) THEN
		    CALL DSPLOT ( igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CSTANM ) THEN
		    CALL DSTANM ( igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CENANM ) THEN
		    CALL DENANM ( igbufd (iout+2) )
		    check = .true.
C
		ELSE IF ( ifunc .eq. CLOOPC ) THEN
		    CALL DLOOPC ( igbufd (3), igbufd (iout+2) )
		    check = .true.
C
		ELSE IF ( ifunc .eq. CSLWIN ) THEN
		    CALL ST_ITOS ( igbufd (3), 18, nc, wname, ier )
		    CALL DSLWIN ( wname, igbufd (iout+3),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 3
C
		ELSE IF (ifunc .eq. CGSAVE ) THEN
		    CALL ST_ITOS ( igbufd (5), 18, nc, filnam, ier )
		    CALL DGSAVE ( filnam, igbufd (3), igbufd (4),
     +                            igbufd (iout+2) )
                    igbufd (iout+1) = 3
		ELSE IF ( ifunc. eq. CCLPNL ) THEN
		    CALL DCLPNL ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (6), igbufd (iout+2) )
C
                ELSE IF ( ifunc. eq. CSROAM ) THEN
                    CALL DSROAM ( igbufd (3), igbufd (4), igbufd (5),
     +                            igbufd (iout+2) )
C
                ELSE IF ( ifunc. eq. CROAM ) THEN
                    CALL DROAM ( igbufd (3), igbufd (4), igbufd(5),
     +                           igbufd (iout+2) )
C
		ELSE IF ( ifunc. eq. CSGTGN ) THEN
		    CALL DSGTGN ( igbufd (3), igbufd (4),
     +				  igbufd (iout+2) )
		END IF
C------------------------------------------------------------------------
	    ELSE IF ( igroup .eq. DPLOT ) THEN
C
		IF ( ifunc .eq. CARRW ) THEN
		    np = igbufd (4)
		    CALL DARRW ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (5+2*np),
     +				 igbufd (5+3*np), igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CDARR ) THEN
		    np = igbufd (4)
		    CALL DDARR ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (5+2*np),
     +				 igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CBARB ) THEN
		    np = igbufd (4)
		    CALL DBARB ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (5+2*np),
     +				 igbufd (5+3*np), igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CHASH ) THEN
		    np = igbufd (4)
		    CALL DHASH ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (5+2*np),
     +				 igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CMARK ) THEN
		    np = igbufd (4)
		    CALL DMARK ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CTICM ) THEN
		    np = igbufd (6)
		    CALL DTICMK ( igbufd (3), igbufd (4), igbufd (5),
     +				  np, igbufd (7), igbufd (7+np),
     +				  igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CLINE ) THEN
		    np = igbufd (4)
		    CALL DLINE ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CTEXT ) THEN
		    lens = igbufd (6)
		    ni  = ( ( lens - 1 ) / 4 ) + 1
		    CALL ST_ITOS ( igbufd (7), ni, nc, txt, ier )
		    CALL DTEXT ( igbufd (3), igbufd (4), igbufd (5),
     +				 txt, lens, igbufd (7+ni),
     +				 igbufd (8+ni), igbufd (9+ni),
     +				 igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CTEXTC ) THEN
		    lens = igbufd (6)
		    ni  = ( ( lens - 1 ) / 4 ) + 1
		    CALL ST_ITOS ( igbufd (7), ni, nc, txt, ier )
		    CALL DTEXTC ( igbufd (3), igbufd (4), igbufd (5),
     +				  txt, lens, igbufd (7+ni),
     +				  igbufd (8+ni), igbufd (9+ni),
     +				  igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CCIRCL ) THEN
		    CALL DCIRCL ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (6), igbufd (7), igbufd (8),
     +				  igbufd (iout+2) )
C
	   	ELSE IF ( ifunc .eq. CSKY ) THEN
		    np = igbufd (4)
		    CALL DSKY ( igbufd (3), np, igbufd (5),
     +				igbufd (5+np), igbufd (5+2*np),
     +				igbufd (5+3*np), igbufd (5+4*np),
     +				igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CWTHR ) THEN
		    np = igbufd (4)
		    CALL DWTHR ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (5+2*np),
     +				 igbufd (5+3*np), igbufd (5+4*np),
     +				 igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CPTND ) THEN
		    np = igbufd (4)
		    CALL DPTND ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (5+2*np),
     +				 igbufd (5+3*np), igbufd (5+4*np),
     +				 igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CPWTH ) THEN
		    np = igbufd (4)
		    CALL DPWTH ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (5+2*np),
     +				 igbufd (5+3*np), igbufd (5+4*np),
     +				 igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CCTYP ) THEN
		    np = igbufd (4)
		    CALL DCTYP ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (5+2*np),
     +				 igbufd (5+3*np), igbufd (5+4*np),
     +				 igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CICNG ) THEN
		    np = igbufd (4)
		    CALL DICNG ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (5+2*np),
     +				 igbufd (5+3*np), igbufd (5+4*np),
     +				 igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CSPCL ) THEN
		    np = igbufd (4)
		    CALL DSPCL ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (5+2*np),
     +				 igbufd (5+3*np), igbufd (5+4*np),
     +				 igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CTURB ) THEN
		    np = igbufd (4)
		    CALL DTURB ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (5+2*np),
     +				 igbufd (5+3*np), igbufd (5+4*np),
     +				 igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CFILL ) THEN
		    np = igbufd (4)
		    CALL DFILL ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (iout+2) )

		ELSE IF ( ifunc .eq. CFRNT ) THEN
		    np = igbufd (4)
		    CALL DFRNT ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (iout+2) )

		ELSE IF ( ifunc .eq. CSPLN ) THEN
		    np = igbufd (4)
		    CALL DSPLN ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CTXSY ) THEN
		    lens = igbufd (12)
		    ni  = ( ( lens - 1 ) / 4 ) + 1
		    CALL ST_ITOS ( igbufd (13), ni, nc, txt, ier )
		    CALL DTXSY ( igbufd (3), igbufd (4), igbufd (5), 
     +				  igbufd (6), igbufd (7), igbufd (8), 
     +				  igbufd (9), igbufd (10), igbufd(11), 
     +				  txt, lens, igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CCMBO) THEN
		    np = igbufd (4)
		    CALL DCMBO ( igbufd (3), np, igbufd (5),
     +				 igbufd (5+np), igbufd (5+2*np),
     +				 igbufd (5+3*np), igbufd (5+4*np),
     +				 igbufd (iout+2) )
		ELSE IF ( ifunc .eq. CLOGO ) THEN
		    CALL DLOGO ( igbufd (3), igbufd (4), igbufd (5),
     +				 igbufd (6), igbufd(7), igbufd(8), 
     +				 igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CARC ) THEN
		    CALL DARC ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (6), igbufd (7), igbufd (8),
     +				  igbufd (9), igbufd (10), 
     +				  igbufd (iout+3), igbufd (iout+2) )
		    igbufd (iout+1) = 6
C
		END IF
C------------------------------------------------------------------------
	    ELSE IF ( igroup .eq. DATTRB ) THEN
C
		IF ( ifunc .eq. CSBARB ) THEN
		    CALL DSBARB ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+5), igbufd (iout+2) )
		    igbufd (iout+1) = 5
C
		ELSE IF ( ifunc .eq. CSARRW ) THEN
		    CALL DSARRW ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (6), igbufd (iout+3),
     +				  igbufd (iout+4), igbufd (iout+5),
     +				  igbufd (iout+6), igbufd (iout+2) )
		    igbufd (iout+1) = 6
C
		ELSE IF ( ifunc .eq. CSDARR ) THEN
		    CALL DSDARR ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (6), igbufd (iout+3),
     +				  igbufd (iout+4), igbufd (iout+5),
     +				  igbufd (iout+6), igbufd (iout+2) )
		    igbufd (iout+1) = 6
C
		ELSE IF ( ifunc .eq. CSHASH ) THEN
		    CALL DSHASH ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+5), igbufd (iout+2) )
		    igbufd (iout+1) = 5
C
		ELSE IF ( ifunc .eq. CSTEXT ) THEN
		    CALL DSTEXT ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (6), igbufd (7), igbufd (8),
     +				  igbufd (9), igbufd (iout+3),
     +				  igbufd (iout+4), igbufd (iout+5),
     +				  igbufd (iout+6), igbufd (iout+7),
     +				  igbufd (iout+8), igbufd (iout+9),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 9
C
		ELSE IF ( ifunc .eq. CSMRKR ) THEN
		    CALL DSMRKR ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (6), igbufd (iout+3),
     +				  igbufd (iout+4), igbufd (iout+5),
     +				  igbufd (iout+6), igbufd (iout+2) )
		    igbufd (iout+1) = 6
C
		ELSE IF ( ifunc .eq. CSLPAT ) THEN
		    CALL DSLPAT ( igbufd (3), igbufd (iout+3),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 3
C
		ELSE IF ( ifunc .eq. CQLPAT ) THEN
		    CALL DQLPAT ( igbufd (iout+3), igbufd (iout+2) )
		    igbufd (iout+1) = 10
C
		ELSE IF ( ifunc .eq. CSLINE ) THEN
		    CALL DSLINE ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (6), igbufd (iout+3),
     +				  igbufd (iout+4), igbufd (iout+5),
     +				  igbufd (iout+6), igbufd (iout+2) )
		    igbufd (iout+1) = 6
C
		ELSE IF ( ifunc .eq. CSSKY ) THEN
		    CALL DSSKY ( igbufd (3), igbufd (4), igbufd (5),
     +				 igbufd (iout+3), igbufd (iout+4),
     +				 igbufd (iout+5), igbufd (iout+2) )
		    igbufd (iout+1) = 5
C
		ELSE IF ( ifunc .eq. CSWTHR ) THEN
		    CALL DSWTHR ( igbufd (3), igbufd (4),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 4
C
		ELSE IF ( ifunc .eq. CSFILL ) THEN
		    CALL DSFILL ( igbufd (3), igbufd (4),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 4
C
		ELSE IF ( ifunc .eq. CSSMTH ) THEN
		    CALL DSSMTH ( igbufd (3), igbufd (4),
     +				  igbufd (5), igbufd (6),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+5), igbufd (iout+6),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 6
C
		ELSE IF ( ifunc .eq. CSRDUC ) THEN
		    CALL DSRDUC ( igbufd (3),
     +				  igbufd (iout+3), igbufd (iout+2) )
		    igbufd (iout+1) = 3
C
		ELSE IF ( ifunc .eq. CSDASH ) THEN
		    CALL DSDASH ( igbufd (3),
     +                            igbufd (iout+3), igbufd (iout+2) )
	            igbufd (iout+1) = 3
C
		ELSE IF ( ifunc .eq. CSPTND ) THEN
		    CALL DSPTND ( igbufd (3), igbufd (4),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 4
C
		ELSE IF ( ifunc .eq. CSPWTH ) THEN
		    CALL DSPWTH ( igbufd (3), igbufd (4),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 4
C
		ELSE IF ( ifunc .eq. CSCTYP ) THEN
		    CALL DSCTYP ( igbufd (3), igbufd (4),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 4
C
		ELSE IF ( ifunc .eq. CSDATT ) THEN
		    CALL ST_ITOS ( igbufd (4), 18, nc, filnam, ier )
		    CALL DSDATT ( igbufd (3), filnam, igbufd (22),
     +				  igbufd (23), igbufd (24),
     +				  igbufd (iout+3), igbufd (iout+2) )
		    igbufd (iout+1) = 3
C
		ELSE IF ( ifunc .eq. CQDATT ) THEN
		    CALL DQDATT ( igbufd (iout+3), filnam,
     +				  igbufd (iout+22), igbufd (iout+23),
     +				  igbufd (iout+24), igbufd (iout+25),
     +				  igbufd (iout+2) )
		    CALL ST_STOI ( filnam, 72, nv, igbufd (iout+4),
     +				   ier )
		    igbufd (iout+1) = 25
C
		ELSE IF ( ifunc .eq. CSICNG ) THEN
		    CALL DSICNG ( igbufd (3), igbufd (4),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 4
C
		ELSE IF ( ifunc .eq. CSSPCL ) THEN
		    CALL DSSPCL ( igbufd (3), igbufd (4),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 4
C
		ELSE IF ( ifunc .eq. CSTURB ) THEN
		    CALL DSTURB ( igbufd (3), igbufd (4),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 4
C
		ELSE IF ( ifunc .eq. CSFRNT ) THEN
		    CALL DSFRNT ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (6),  igbufd (iout+3),
     +				  igbufd (iout+4), igbufd (iout+5),
     +				  igbufd (iout+6), 
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 6
C
		ELSE IF ( ifunc .eq. CSSPLN ) THEN
		    CALL DSSPLN ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (6), igbufd (7), 
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+5), igbufd (iout+6),
     +				  igbufd (iout+7), igbufd (iout+2) )
		    igbufd (iout+1) = 7
C
		ELSE IF ( ifunc .eq. CSCMBO) THEN
		    CALL DSCMBO( igbufd (3), igbufd (4),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 4
C
		END IF
C------------------------------------------------------------------------
	    ELSE IF ( igroup .eq. DWINDW ) THEN
C
		IF ( ifunc .eq. CSWNDW ) THEN
		    np = 3
		    CALL DSWNDW ( igbufd (3), igbufd (3+np),
     +				  igbufd (3+2*np), igbufd (3+3*np),
     +				  igbufd (iout+2) )
		END IF
C------------------------------------------------------------------------
	    ELSE IF ( igroup .eq. DCOLOR ) THEN
C
		IF ( ifunc .eq. CSCOLR ) THEN
		    CALL DSCOLR ( igbufd (3), igbufd (iout+3),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 3
C
		ELSE IF ( ifunc .eq. CSCLR2 ) THEN
		    CALL DSCLR2 ( igbufd (3), igbufd (4),
     +				  igbufd (iout+3), igbufd (iout+4),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 4
C
		ELSE IF ( ifunc .eq. CSCTBL ) THEN
		    CALL ST_ITOS ( igbufd (3), 16, nc, ctblnm, ier )
		    CALL DSCTBL ( ctblnm, igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CSCOLB ) THEN
		    CALL DSCOLB ( igbufd (3), igbufd (4),
     +				  igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CQCLRS ) THEN
		    CALL DQCLRS ( igbufd (3), igbufd (iout+3),
     +				  igbufd (iout+2) )
		    igbufd (iout+1) = 3
C
		ELSE IF ( ifunc .eq. CSCRGB ) THEN
		    CALL DSCRGB ( igbufd (3), igbufd (4), igbufd (5),
     +				  igbufd (6), igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CSBRGB ) THEN
		    nc = igbufd (4)
		    CALL DSBRGB ( igbufd (3), nc, igbufd (5),
     +				  igbufd (5+nc), igbufd (5+2*nc),
     +				  igbufd (5+3*nc), igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CSCNAM ) THEN
		    CALL ST_ITOS ( igbufd (4), 10, nc, clrnm, ier )
		    CALL DSCNAM ( igbufd (3), clrnm, igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CSCINT) THEN
		    CALL DSCINT ( igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CQCOMP ) THEN
		    CALL DQCOMP ( igbufd (3), clrnm, igbufd (iout+23),
     +				  igbufd (iout+24), igbufd (iout+25),
     +				  xname, igbufd (iout+2) )
		    CALL ST_STOI ( clrnm, 80, nc, igbufd (iout+3),  ier )
		    CALL ST_STOI ( xname, 80, nc, igbufd (iout+26), ier )
		    igbufd (iout+1) = 45
		END IF
C------------------------------------------------------------------------
	    ELSE IF ( igroup .eq. DSAT ) THEN
C
		IF ( ifunc .eq. CSATIM ) THEN
		    CALL ST_ITOS ( igbufd (3), 33, nc, satfil, ier )
		    CALL DSATIM ( satfil, igbufd (36), igbufd (37),
     +				  igbufd (38), igbufd (39), igbufd (40),
     +				  igbufd (41), igbufd (42), igbufd (43),
     +				  igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CSICMN ) THEN
		    DO ic = 1, NIMCMN
			icmnarr (ic) = igbufd (ic + 2)
		    END DO
		    CALL DSICMN ( igbufd (iout+2) )
C
		ELSE IF ( ifunc .eq. CSATPX ) THEN
		    CALL ST_ITOS ( igbufd (7), 33, nc, satfil, ier )
		    CALL DSATPX ( igbufd (3), igbufd (4), 
     +                            igbufd (5), igbufd (6), satfil, 
     +				  igbufd (40), igbufd (41), igbufd (42),
     +				  igbufd (43), igbufd (44), igbufd (45),
     +				  igbufd (iout+3),
     +				  igbufd (iout+4),
     +				  igbufd (iout+5),
     +				  igbufd (iout+2 ) )
		    igbufd (iout+1) = 5
	      END IF
C------------------------------------------------------------------------
	    END IF
C
C*	    Send the output buffer to GPLT.
C
	    CALL GPUTC ( igbufd (iout+1), iret )
	END DO
C
C*	Free semaphore lock on IPC message queues.
C
	mproc = 1
	CALL GSFREE  ( mproc, isemid, ifree, iret )
	IF ( ifree .ne. 1 ) THEN
	   CALL ER_WMSG  ( 'DEVICE', -2, ' ', iret )
	   STOP
	ENDIF
C*
	END
