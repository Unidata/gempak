	PROGRAM  SNTSER
C************************************************************************
C* PROGRAM SNTSER							*
C*									*
C* This program draws time series using reports in a sounding file.	*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89	Based on GDTSER				*
C* S. Schotz/GSC	 7/90	Updates for IN_AXIS, IN_PTYPE, IN_TAXS	*
C* K. Brill/NMC		10/91	PANEL*24 --> *48			*
C* K. Brill/NMC		03/92	Change XTLBL & CTLBL (50) -> (MAXTIM)	*
C* S. Jacobs/EAI        11/92   Added call to GMESG and 'shrttl'        *
C* K. Brill/NMC		 8/93	stn*4 -> stn*8				*
C* L. Williams/EAI	 3/94	Clean up declarations of user input	*
C*				variables				*
C* S. Jacobs/NMC         6/94   DEVICE*12 --> *72                       *
C* S. Jacobs/NMC	 6/94	STNDEX*48 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to SNTUPD and added shrttl *
C*				to the user input variables		*
C* S. Jacobs/NMC	 9/94	Moved the title plotting to the end	*
C* S. Jacobs/NMC	 2/95	Added text				*
C* S. Jacobs/NMC	 3/95	Changed call to SNTLEV to pass file num	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in call to SNTDSP		*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* S. Jacobs/NCEP	 2/99	Removed respnd from call to SNTDAT	*
C* R. Curtis/EAI         8/00   Added calls to GSTANM and GENANM        *
C* T. Piper/SAIC	 4/02	Fixed UMR; init arecur, datcur, snfcur, *
C*							isnfln		*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C* S. Jacobs/NCEP	 9/10	Changed arrays to use LLMXTM not the	*
C*				local parameter MAXTIM			*
C* M. James/Unidata     11/10	Changed y & ylbl to use LLMXTM  	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	snfile*(LLMXLN), border*(LLMXLN), ptype*(LLMXLN),
     +			dattim*(LLMXLN), snparm*(LLMXLN),
     +			stndex*(LLMXLN), level*(LLMXLN), vcoord*(LLMXLN),
     +			title*(LLMXLN), yaxis*(LLMXLN), taxis*(LLMXLN),
     +			line*(LLMXLN), area*(LLMXLN), shrttl*(LLMXLN),
     +			device*(LLMXLN), marker*(LLMXLN), panel*(LLMXLN),
     +			text*(LLMXLN)
C*
	LOGICAL		clear
C*
	REAL		y (LLMXTM),  ylbl (LLMXTM), 
     +                  rmargn (4)
	CHARACTER	snfcur*72, arecur*48, pmdset (MMPARM)*4,
     +			stn*8,     stnprm*4,  parm*12
	CHARACTER	ttlstr*72, datcur*48, ctime*48
	CHARACTER	ttlinp*72, filnam*72
	LOGICAL		newfil, done, proces, respnd
C
	CHARACTER*20	timfil (LLMXTM), timfnd (LLMXTM), ctlbl (LLMXTM)
	REAL		x (LLMXTM), xtlbl (LLMXTM)
C------------------------------------------------------------------------
	isnfln = 0
	arecur = ' '
	datcur = ' '
	snfcur = ' '
C
C*	Initialize TAE and GEMPLT.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'SNTSER', ier )
	IF  ( iperr .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C 
            CALL GD_INIT  ( ier )
	    CALL GG_INIT  ( 0, iperr )
	END IF
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
C
C*	Main loop to read in TAE parameters and draw profile.
C
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done and initialize
C*	  parameter names.
C
	  proces = .true.
	  parm   = ' '
	  stnprm = ' '
C
C*	  Read in the variables from the TAE.
C
	  CALL SNTINP  ( snfile, dattim, level,  vcoord, snparm, stndex,
     +			 area,   ptype,  yaxis,  taxis,
     +			 border, line,   marker, title,
     +			 clear,  device, panel,  text,   iperr )
C
C*	  Exit if there is an error.
C
	  IF  ( iperr .ne. 0 )  THEN
	    done = .true.
	   ELSE
C
C*	    Set up the graphics device.
C
	    CALL GG_SDEV  ( device, iret )
	    IF  ( iret .ne. 0 )  proces = .false.
C
C*	    Set the text.
C
	    CALL IN_TEXT ( text, ier )
C
C*	    Open the sounding file.
C
	    CALL FL_MFIL ( snfile, ' ', filnam, ier )
	    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
	    CALL SNTFIL  ( filnam, snfcur, isnfln, newfil, pmdset,
     +			   npmdst, ivert,  iret )
	    IF  ( iret .ne. 0 )  proces = .false.
C
C*	    Set the area.
C
	    IF  ( proces )  THEN
		CALL LC_UARE  ( area, newfil, isnfln, arecur, stn,
     +				iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'LC', iret, ' ', ier )
		    proces = .false.
		END IF
	    END IF
C
C*	    Get the levels and vertical coordinate.
C
	    IF  ( proces )  THEN
		CALL SNTLEV  ( isnfln, level, vcoord, ivert, vlevel,
     +			       lvert, iret )
C
C*	        Get input times and pointers.
C
		IF  ( iret .eq. 0 )  CALL SNTDAT  (  isnfln, dattim, 
     +			             newfil, datcur, ntim,   
     +				     timfil, ctime,  iret )
		IF  ( iret .ne. 0 )  THEN
		    proces = .false.
		END IF
	    END IF
C
C*	    Get parameter information.
C
	    IF  ( proces )  THEN
		CALL SNTPRM  ( snparm, stndex, parm, stnprm, iret )
		IF  ( iret .ne. 0 ) proces = .false.
	    END IF
C
C*	    Get the data to plot.
C
	    IF  ( proces )  THEN
		CALL SNTDTA  ( isnfln, vlevel, lvert, parm, stnprm,
     +			       timfil, ntim,   timfnd, npts,   x, y,  
     +			       iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
	    IF  ( proces )  THEN
C
C*	        Get information about y axis.
C
		CALL SNTYAX  ( ptype,  yaxis,  npts,
     +			       y,      iyaxis, ratio,  ystrt,  
     +			       ystop,  ylbl,   nylbl,  rmargn,
     +			       datmin, datmax, iylbfr, iyglfr, iytmfr,
     +                         iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'SNTSER', iret, ' ', ier )
		    proces = .false.
		END IF
C
C*	        Get information about x axis.
C
		CALL IN_TAXS ( taxis, LLMXTM, npts, timfnd, x, xstrt, 
     +                         xstop, xtlbl, ctlbl, nxlbl, xmndst, 
     +                         ixlbfr, ixglfr, ixtmfr, iret )
		IF  ( iret .ne. 0 )  THEN
		    proces = .false.
		END IF
	    END IF
C
C*          Set the current pixmap.
C
            CALL GSTANM (iret)
C
C*	    Give user a chance to exit.
C
	    IF  ( proces )  THEN
		CALL SNTDSP  ( filnam, parm, stnprm, area,  datmin,
     +			       datmax, npts, ctime,  panel, respnd, 
     +			       iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Draw the time series.
C
	    IF  ( proces ) THEN
C
C*		Set plotting mode to graph mode.
C
		CALL GQMODE  ( mode, ier )
		CALL GSMODE  ( 2, ier )
		CALL GEPLOT  ( ier )
C
C*		Clear screen if requested and set panel.
C
		IF  ( clear )  CALL GCLEAR  ( ier )
		CALL GG_PANL  ( panel, ier )
C
C*		Plot axes and data.
C
		CALL SNTPLT  ( border,  line,   marker, iyaxis, ystrt, 
     +			       ystop,   ylbl,   nylbl,  iylbfr, iyglfr,
     +                         iytmfr,  xstrt,  xstop, 
     +			       xtlbl,   ctlbl,  nxlbl,  npts, ixlbfr, 
     +                         ixglfr, ixtmfr, x, y, ratio,  rmargn, 
     +                         iret )
C
C*		Write title.
C
		ipbar = INDEX ( title, '|' )
		IF  ( ipbar .ne. 0 )  THEN
		    shrttl = title(ipbar+1:)
		    IF  ( ipbar .eq. 1 )  THEN
			ttlinp = ' '
		    ELSE
			ttlinp = title(:ipbar-1)
		    END IF
		ELSE
		    ttlinp = title
		    CALL ST_LSTR ( area, len1, ier )
		    shrttl = 'TIME SERIES ' // timfnd(1)(5:9) //
     +			     '-' // timfnd(ntim)(5:9) // ' ' //
     +			     area(:len1)
		END IF
		IF  ( clear )  CALL GMESG ( shrttl, ier )
		CALL IN_TITL  ( ttlinp, 0, ititl, linttl, ttlstr, ier )
		IF  ( ititl .ne. 0 )  THEN
		    IF  ( ttlstr .eq. ' ' )  THEN
			CALL SNTTTL  ( ctime, area,   level,  ivert, 
     +				       parm,  stnprm, ttlstr, ier )
		    END IF
		    CALL GSCOLR   ( ititl, ier )
		    CALL GG_WSTR  ( ttlstr, linttl, ier )
		END IF
C
C*		Reset the plotting mode and flush buffers.
C
		CALL GSMODE  ( mode, ier )
		CALL GEPLOT  ( ier )
	    END IF
C
C*	    Prompt for next profile to be done.
C
	    CALL GENANM (iret)
            CALL IP_DYNM  ( done, ier )
	  END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF (iperr .ne. 0) CALL ER_WMSG ( 'SNTSER', iperr, ' ', ier )
C
C*	Close the file and exit from GEMPLT and the TAE.
C
	CALL SN_CLOS  ( isnfln, ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
