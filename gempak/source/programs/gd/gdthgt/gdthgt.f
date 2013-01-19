	PROGRAM  GDTHGT
C************************************************************************
C* PROGRAM GDTHGT							*
C*									*
C* This program creates time height sections through scalar grids	*
C*									*
C**									*
C* Log:									*
C* T.W. Barker/WR/SSD	 8/91	Created from GDCROSS			*
C* S. Jacobs/EAI	11/92	Added call to GMESG and 'shrttl'	*
C* M. desJardins/NMC	11/92	Eliminate unused variables		*
C* S. Jacobs/EAI         2/94   Added COLADD flag to DG_OFIL            *
C* L. Williams/EAI	 3/94	Clean up declarations of user input	*
C*				variables				*
C* S. Jacobs/NMC	 6/94	Fixed typo in declarations		*
C* L. Williams/EAI	 7/94	Added shrttl to the user input variables*
C* S. Jacobs/NMC	 9/94	Added GR_TITL				*
C* K. Tyle/GSC		 7/96	Eliminated call to GDTXUP		*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in call to GDTXSP		*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* T. Lee/GSC		 1/99	Increased LLMXTM to LLMXGT		*
C* S. Jacobs/NCEP	 3/01	Replaced DG_OFIL with DG_MFIL 		*
C* T. Lee/GSC		 8/01	Processed multiple files		*
C* T. Lee/SAIC		10/01	Called DG_CLAL				*
C* R. Tian/SAIC		10/02	Called DG_CXGP				*
C* K. Brill/HPC		12/02	Pass blank IJSKIP into DG_CXGP		*
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* R. Tian/SAIC         11/03   Added nuflg to DG_INTL call             *
C* R. Tian/SAIC         11/03   Removed nuflg from DG_INTL call         *
C* T. Piper/SAIC        06/04   Added call to GD_CPF                    *
C* K. Brill/HPC         10/04   Changes for time/file management        *
C* C. Bailey/HPC	 6/06	Added contour text labeling		*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C* S. Gilbert/NCEP	08/07	Redimensioned grid arrays		*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear
	CHARACTER	gdfile*(LLMXLN), border*(LLMXLN),
     +			ptype*(LLMXLN), gpoint*(LLMXLN),
     +			taxis*(LLMXLN), gdatim*(LLMXLN),
     +			gfunc*(LLMXLN), gvcord*(LLMXLN),
     +			title*(LLMXLN), yaxis*(LLMXLN),
     +			device*(LLMXLN), scale*(LLMXLN),
     +			panel*(LLMXLN), wind*(LLMXLN),
     +			cint*(LLMXLN), line*(LLMXLN),
     +			text*(LLMXLN), gvect*(LLMXLN), shrttl*(LLMXLN)
C*
	REAL 		grdout (LLMXLV * LLMXGT), 
     +			grdoutu (LLMXLV * LLMXGT),
     +			grdoutv (LLMXLV * LLMXGT)
	REAL		rlvld (LLMXLV), rlvlc (LLMXLV), rlvlv (LLMXLV)
	REAL	        xgrd (LLMXLV * LLMXGT), x (LLMXGT),
     +			ylbl (LLAXIS), xptsc (LLMXGT),
     +			vclsfc, ggrd (LLMXLV * LLMXGT), xtlbl (LLAXIS),
     +			grdt1 (LLMXLV * LLMXGT)
	CHARACTER	ttlstr*72, ctlbl (LLAXIS)*40, 
     +			trange (2)*40, timfnd (LLMXGT)*20,
     +			time (2)*20, clbl(LLCLEV)*24
	LOGICAL		respnd, done, proces, havsfc, havvec, havcon,
     +			scflag
C*
	REAL		clvl (LLCLEV), rmargn (4)
	INTEGER		icolor (LLCLEV), iline (LLCLEV),
     +			ilwid  (LLCLEV), labflg (LLCLEV),
     +			levels (LLMXLV), levl (2)
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDTHGT', ier )
C
C*  Initialize GEMPLT.
C
	    CALL GG_INIT  ( 1, ier )
	    IF ( ier .eq. 0 )  THEN
C
C*  Initialize grid library common area grdcmn.cmn
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
C*	Main loop to read in TAE parameters and draw time section.
C
	DO WHILE  ( .not. done )
C
C*	    Set flag to indicate processing will be done.
C
	    proces = .true.
C
C*	    Read in the variables from the TAE.
C
	    CALL GDTXIN  ( gdatim, gvcord, gfunc, gvect, gpoint,
     +			   gdfile, ptype, taxis, yaxis, border,
     +			   line, cint, wind, title, clear, scale,
     +			   panel, device, text, iperr )
C
C*	    Exit if there is an error.
C
	    IF ( iperr .ne. 0 )  THEN
		done = .true.
	      ELSE
C
C*		Process the GDFILE input.
C
		CALL DG_NFIL ( gdfile, ' ', ier )
		IF ( ier .ne. 0 ) THEN
		    CALL ER_WMSG ( 'DG', ier, ' ', irr )
		    proces = .false.
		END IF
C
C*		Process the GDATTIM input; setup the time server.
C
		CALL DG_NDTM ( gdatim, ier )
		IF ( ier .ne. 0 ) THEN
		    CALL ER_WMSG ( 'DG', ier, gdatim, irr )
		    proces = .false.
		END IF
C
C*		Get time information 
C
		IF ( proces ) THEN
		    CALL DG_QTMS ( LLMXGT, .false., timfnd, ntime,
     +				   trange (1), iret )
		    IF ( iret .ne. 0 ) proces = .false.
C
C*  		    Check for number of times greater than zero.
C
		    proces = ( ntime .gt. 0 )
		END IF
C*
		IF ( proces ) THEN
		    CALL ST_LCUC ( gvcord, gvcord, iret )
		    CALL LV_CORD ( gvcord, gvcord, ivcord, iret )
		    IF ( ( iret .ne. 0 ) .or. ( ivcord .eq. 0 ) )
     +			proces = .false.
		END IF
C
C*		Set up the graphics device.
C
		IF ( proces ) THEN
		    CALL GG_SDEV  ( device, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C
C*		Set text attributes.
C
		IF ( proces ) THEN
		    CALL IN_TEXT ( text, iret )
		    IF ( iret .ne. 0 ) proces = .false.
		END IF
C
C*		Get y-axis parameters
C
		IF ( proces ) THEN
		    CALL GDTXYY( ptype, yaxis, ivcord, iyaxis, ratio,
     +				 ystrt, ystop, ylbl, nylbl, rmargn,
     +				 ilbfrq, iglfrq, itmfrq, iret )
		    IF ( iret .ne. 0 ) THEN
			CALL ER_WMSG( 'GDTHGT', iret, ' ', ier )
			proces = .false.
		    END IF
		END IF
C
C*		Get levels.
C
		IF ( proces ) THEN
		    CALL GDTXGL ( 1, timfnd, 1, ystrt, ystop,
     +				  ivcord, levels, nlev, iret )
		    IF ( iret .ne. 0 ) proces = .false.
		END IF
		IF ( proces ) THEN
		    DO i = 1, ntime
			CALL TG_VALD ( timfnd(i), timfnd(i), iret )
			IF ( iret .ne. 0 ) proces = .false.
		    END DO
		END IF
C
C*		Get x-axis parameters
C
		IF ( proces ) THEN
		    CALL GDTXTA ( taxis, LLAXIS, ntime, timfnd, x,
     +				  xstrt, xstop, xtlbl, ctlbl, nxlbl,
     +				  xmndst, itlbfr, itglfr, ittmfr,
     +				  iret )
		    IF ( iret .ne. 0 ) proces = .false.
		END IF
C
C*		Open the grid file and set the grid navigation.  This
C*		will set the proper mode for the grid file.  The mode
C*		must be set to graph mode later.
C
C
C*		Get even levels
C
		IF ( proces ) THEN
		    DO i = 1, nlev
			rlvld (i) = float (levels (i))
		    END DO
		    CALL GDTXEV ( rlvld, nlev, iyaxis, rlvlc, nlvlc,
     +				  iret )
		    CALL GDTXEV ( rlvld, nlev, 1, rlvlv, nlvlv, iret )
		    IF ( iret .ne. 0 ) proces = .false.
		END IF
C
		IF ( proces ) THEN
		    CALL GDTXEV ( x, ntime, 1, xptsc, nptsc, iret)
		    IF ( iret .ne. 0 ) proces = .false.
		END IF
C
C*              GD_CPF checks if a file name was entered for the grid point
C
                CALL GD_CPF(gpoint, gpoint, ier)
                IF ( ier .ne. 0 ) THEN
                    iret = -14
                    CALL ER_WMSG  ( 'GDTHGT', iret, ' ', ier )
                    proces = .false.
                END IF
C
C*		Get data for this list of times and levels
C
		havcon = .false.
		IF ( proces ) THEN
		    havcon = .true.
		    CALL GDTXDA ( igdfln, timfnd, ntime, gvcord,
     +				  ystrt, ystop, iyaxis, gfunc, ivcord,
     +				  gpoint, levels, nlev, grdt1, rlvlc,
     +				  nlvlc, iret )
		    IF ( iret .eq. 7 ) havcon = .false.
		    IF ( iret .lt. 0 ) proces = .false.
		END IF
C
C*		Put contour data in evenly spaced grid
C
		IF ( proces .and. havcon ) THEN
		    CALL GDTXTI( grdt1, ntime, nlvlc, x, xptsc, nptsc,
     +				 grdout, iret)
		    IF ( iret .ne. 0) proces = .false.
		END IF
C
C*		Get Vector data for this list of times and levels
C
		havvec = .false.
		IF ( proces ) THEN
		    havvec = .true.
		    CALL GDTXVV ( igdfln, timfnd, ntime, gvcord,
     +				  ystrt, ystop, iyaxis, gvect, ivcord,
     +				  gpoint, levels, nlev, grdoutu,
     +				  grdoutv, rlvlv, nlvlv ,iret )
		    IF ( iret .eq. 7 ) havvec = .false.
		    IF ( iret .lt. 0 ) proces = .false.
		END IF
C
		IF ( proces .and. havcon ) THEN
		    CALL GDTXLV( line, cint, scale, nptsc, nlvlc, 1, 1,
     +				 nptsc, nlvlc, grdout, icolor, iline,
     +				 ilwid, labflg, iscale, dmin, dmax,
     +				 clvl, nlvl, clbl, scflag, iret )
		    IF ( iret .ne. 0 ) proces = .false.
		ELSE
		    dmin = 0.0
		    dmax = 0.0
		    nlvl = 0
		END IF
C
		IF ( proces ) THEN
		    CALL GDTXSP ( gdfile, gfunc, ntime, iscale,
     +				  gdatim, gvcord, nlvl, clvl, dmin,
     +				  dmax, icolor, iline, ilwid, labflg,
     +				  device, panel, gvect, wind, havcon,
     +				  havvec, iret )
		    IF ( iret .ne. 0 ) proces = .false.
		END IF

		IF ( proces ) THEN
		    CALL GQMODE ( mode, ier )
		    CALL GSMODE ( 2, ier )
		    IF ( clear ) CALL GCLEAR ( ier )
		    CALL GG_PANL ( panel, ier )
C
		    havsfc = .false.
		    CALL GDTXPL( border, iyaxis, ystrt, ystop, vclsfc,
     +				 havsfc, ylbl, nylbl, xstrt, xstop,
     +				 xtlbl, ctlbl, nxlbl, ntime, itlbfr,
     +				 itglfr, ittmfr, ratio, rmargn, ilbfrq,
     +				 iglfrq, itmfrq, iret )
C
		    CALL GSGGRF ( 1, iyaxis, nptsc, nlvlc, xptsc (1),
     +				  ystrt, xptsc (nptsc), ystop, iret )
		    IF ( havcon ) THEN
			CALL GCLGRN ( nptsc, nlvlc, grdout, 0, 0, 0,
     +				      nlvl, clvl, clbl, icolor, iline,
     +				      ilwid, labflg, scflag, iret )
		    END IF
		    IF ( havvec ) THEN
			indx = 1
			DO k = 1, nlvlv
			    DO i = 1, ntime 
				xgrd (indx) = x (i)
				ggrd (indx) = rlvlv (k)
				indx = indx + 1
			    END DO
			END DO
			CALL GDTXPV ( gvect, grdoutu, grdoutv, xgrd,
     +				      ggrd, ntime, nlvlv, wind,
     +				      points, ier )
		    END IF
C
C*		    Write the title.
C
		    CALL IN_TITL ( title, 0, ititl, linttl, ttlstr,
     +				   ier )
		    levl (1) = -1
		    levl (2) = -1
		    time (1) = trange (1)
		    time (2) = ' '
		    CALL GR_TITL  ( ttlstr, time, .false.,
     +				    levl, ivcord, gfunc, iscale,
     +				    gpoint, ttlstr, shrttl, iret )
		    IF ( clear ) CALL GMESG ( shrttl, ier )
		    IF ( ititl .ne. 0 ) THEN
			CALL GSCOLR ( ititl, ier )
			CALL GG_WSTR ( ttlstr, linttl, ier )
		    END IF
C
		    CALL GSMODE ( mode, ier )
		END IF
		CALL GEPLOT ( ier )
		CALL IP_DYNM ( done, ier )
	    END IF
	END DO
	CALL GENDP ( 0, iret )
	CALL IP_EXIT ( iret )

	END
