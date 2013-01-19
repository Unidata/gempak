	PROGRAM  GDTSER
C************************************************************************
C* PROGRAM GDTSER							*
C*									*
C* This program draws time series using grids in a grid file.		*
C*									*
C**									*
C* Log:									*
C* G. Huffman/GSC	 2/89	Based on GDPROF				*
C* K. Brill/GSC          3/90   Added TEXT				*
C* K. Brill/GSC          5/90   Added changes for IN_AXIS		*
C* S. Schotz/GSC	 7/90   Added changes for IN_PTYP		*
C* J. Whistler/SSAI	 4/91	Changed taxis and title to *48 from *72	*
C* J. Whistler/SSAI	 4/91	Changed GDNTIT to GR_TITL and made 	*
C*				trange an array of (2) for GR_TITL	*
C* M. desJardins/NMC	10/91	Changed panel to *48			*
C* S. Jacobs/EAI        11/92   Added call to GMESG and 'shrttl'        *
C* S. Jacbos/EAI	 9/93	Modified short title			*
C* S. Jacobs/EAI         2/94   Added COLADD flag to DG_OFIL            *
C* S. Jacobs/NMC         6/94   DEVICE*12 --> *72                       *
C* L. Williams/EAI	 7/94	Removed call to GDTUPD and added shrttl *
C* 				to the user input variables		*
C* S. Jacobs/NMC	 9/94	Moved the title plotting to the end	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in call to GDTDSP		*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* R. Curtis/EAI         8/00   Added calls to GSTANM and GENANM        *
C* S. Jacobs/NCEP	 3/01	Replaced DG_OFIL with DG_MFIL 		*
C* T. Lee/GSC		 8/01	Replaced GDTTIM with GR_FTIM		*
C* T. Lee/SAIC		10/01	Called GR_FTIM before DG_MFIL call	*
C* R. Tian/SAIC		10/02	Changed arg to be .true. in DG_MFIL	*
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* R. Tian/SAIC         11/03   Added nuflg to DG_INTL call             *
C* R. Tian/SAIC          2/04   Removed nuflg from DG_INTL call         *
C* R. Tian/SAIC         11/04   Changes for time/file management        *
C* S. Jacobs/NCEP	 5/06	Changed check on return from GDTDTA	*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER	gdfile*(LLMXLN), border*(LLMXLN), ptype*(LLMXLN),
     +			gdatim*(LLMXLN), gfunc*(LLMXLN), glevel*(LLMXLN),
     +			gvcord*(LLMXLN), title*(LLMXLN), yaxis*(LLMXLN),
     +			taxis*(LLMXLN), line*(LLMXLN), gpoint*(LLMXLN),
     +			device*(LLMXLN), marker*(LLMXLN), scale*(LLMXLN),
     +			panel*(LLMXLN), text*(LLMXLN), shrttl*(LLMXLN)
	LOGICAL		clear
C*
	PARAMETER	(MAXLBL = 50)
	REAL		x (100), y (100), xtlbl (MAXLBL), 
     +                  ylbl ( LLAXIS ), rmargn (4)
	INTEGER		level (2)
	CHARACTER	ttlstr*72, parm*12,
     +			trange (2)*40, ctlbl (MAXLBL)*20
	LOGICAL		respnd, done, proces
C
	PARAMETER	( MAXTIM = 100 )
	CHARACTER*20	timfnd (MAXTIM)
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDTSER', ier )
C
C*  Initialize GEMPLT.
C
	    CALL GG_INIT  ( 0, ier )
	    IF  ( ier .eq. 0 )  THEN
C
C*  Initialize grid library common area grdcmn.cmn.
C 
		CALL GD_INIT  ( ier )
C
C*  Initialize the DG library.
C
		CALL DG_INTL ( ier )
		done = .false.
	    ELSE
		done = .true.
	    END IF
	ELSE
	    done = .true.
	END IF
C
C*	Main loop to read in TAE parameters and draw profile.
C
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done.
C
	  proces = .true.
C
C*	  Read in the variables from the TAE.
C
	  CALL GDTINP  ( gdfile, gdatim, glevel, gvcord, gfunc, gpoint,
     +			 ptype,  yaxis,  taxis,  
     +			 border, line,   marker, title,
     +			 clear,  device, scale,  panel, text, iperr )
C
C*	  Exit if there is an error.
C
	  IF  ( iperr .ne. 0 )  THEN
	    done = .true.
	   ELSE
C
C*          Process the GDFILE input.
C
            CALL DG_NFIL ( gdfile, ' ', ier )
            IF ( ier .ne. 0 ) THEN
                CALL ER_WMSG ( 'DG', ier, ' ', irr )
                proces = .false.
            END IF
C
C*          Process the GDATTIM input; setup the time server.
C
            CALL DG_NDTM ( gdatim, ier )
            IF ( ier .ne. 0 ) THEN
                CALL ER_WMSG ( 'DG', ier, gdatim, irr )
                proces = .false.
            END IF
C
C*	    Set up the graphics device.
C
	    CALL GG_SDEV  ( device, iret )
	    IF  ( iret .ne. 0 )  proces = .false.
C
C*	    Set TEXT.
C
	    CALL IN_TEXT ( text, iret )
	    IF ( iret .ne. 0 ) proces = .false.
C
C*          GD_CPF checks if a file name was entered for the grid point
C
            CALL GD_CPF(gpoint, gpoint, ier)
	    IF ( ier .ne. 0 ) proces = .false.
C
	    IF  ( proces )  THEN
		CALL GDTDTA  ( glevel, gvcord, 
     +			       gfunc,  gpoint, npts,   
     +			       timfnd, rgx,    rgy,    rlat,   
     +			       rlon,   x,      y,      parm,   
     +			       level,  ivcord, iret )
		IF  ( iret .lt. 0 )  proces = .false.
	    END IF
C
	    IF  ( proces )  THEN
C
C*	        Get information about y axis.
C
		CALL GDTYAX  ( ptype,  yaxis,  ivcord, scale,  npts,
     +			       y,      iyaxis, ratio,  iscale, ystrt,  
     +			       ystop,  ylbl,   nylbl,  rmargn,
     +			       datmin, datmax, ilbfrq, iglfrq, itmfrq,
     +                         iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'GDTSER', iret, ' ', ier )
		    proces = .false.
		END IF
C
C*	        Get information about x axis.
C
		CALL IN_TAXS  ( taxis, MAXLBL, npts, timfnd, x, xstrt, 
     +			        xstop, xtlbl,  ctlbl, nxlbl,  xmndst,
     +			        itlbfr, itglfr, ittmfr, iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'GDTSER', iret, ' ', ier )
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
	        CALL DG_QTMS ( LLMXGT, .false., timfnd, ntime,
     +                         trange (1), iret )
		CALL GDTDSP  ( gdfile, gfunc,  gpoint, rgx,    rgy, 
     +			       rlat,   rlon,   datmin, datmax, iscale,
     +			       ntime,  trange (1), panel,  iret )
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
		CALL GDTPLT  ( border, line,   marker, iyaxis, ystrt, 
     +			       ystop,  ylbl,   nylbl,  xstrt,  xstop, 
     +			       xtlbl,  ctlbl,  nxlbl,  npts,   x, y,
     +                         xmndst,
     +			       ratio,  rmargn, itlbfr, itglfr, ittmfr,
     +                         ilbfrq, iglfrq, itmfrq, iret )
C
C*		Write title.
C
		CALL IN_TITL  ( title, 0, ititl, linttl, ttlstr, ier )
		trange (2) = ' '
		CALL GR_TITL  ( ttlstr, trange, .true., level,
     +				ivcord, parm, iscale, gpoint,
     +				ttlstr, shrttl, iret )
		IF  ( clear )  CALL GMESG ( shrttl, ier )
		IF  ( ititl .ne. 0 )  THEN
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
	IF (iperr .ne. 0) CALL ER_WMSG ( 'GDTSER', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the TAE.
C
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
