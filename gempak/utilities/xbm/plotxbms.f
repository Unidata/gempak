	PROGRAM PLOTXBMS
C************************************************************************
C* PLOTXBMS								*
C*									*
C* Program that plots GEMPAK symbols as X Bitmaps.			*
C*									*
C**									*
C* Log:									*
C* M. Linda/GSC		 6/97	Based on PLOTSYMS			*
C* T. Piper/SAIC	12/07	Removed unused variable str		*
C************************************************************************
C*
	CHARACTER	device*12, filnam*72
	REAL		x ( 2 ), y ( 2 )
C------------------------------------------------------------------------
C*	Intialize GEMPLT.
C
	CALL IN_BDTA ( iret )
	CALL ER_WMSG ( 'GEMPLT', iret, '', irt )
C
	CALL GINITP  ( 1, istat, iret )
	CALL ER_WMSG ( 'GEMPLT', iret, '', irt )
C
	device = 'XBM'
	iunit  = 1
	filnam = '999X;0167'
	itype  = 1
	xsize  = 0.
	ysize  = 0.
C
	CALL GSDEVA  ( device, iunit, filnam, itype, xsize, ysize, iret )
	CALL ER_WMSG ( 'GEMPLT', iret, '', irt )
C
C*	Loop until EXIT selected by user.
C
	iostat = 0
	DO  WHILE ( iostat .eq. 0 )
C
	    WRITE  ( 6, 10 )
10	    FORMAT (
     +      ' ' /
     +      ' 1 = Markers    2 = Weather Symbols    3 = Cloud Type    '/
     +      ' 4 = Sky Cover  5 = Pressure Tendency  6 = Past Weather  '/
     +      ' 7 = Icing      8 = Turbulence         9 = Special       '/
     +      '10 = Lines     11 = Fronts                               '/
     +      ' ' )
C
	    CALL TM_INT ( 'Select a symbol type number', .false.,
     +			  .false., 1, numsym, n, ier )
	    IF  ( ier .eq. 2 ) THEN
		iostat = -1
		numsym = -1
	    END IF
C
	    IF  ( ( numsym .ne. -1 ) .and. ( numsym .ne. 0 ) ) THEN
		CALL TM_INT ( 'Enter symbol number',
     +			      .false., .false., 1, inum, n, ier )
		IF  ( ier .eq. 2 ) THEN
		    iostat = -1
		    numsym = -1
		END IF
	    END IF
C
	    size = 1.1
	    lwid = 1
	    yc   = .5
	    xc   = .5
C------------------------------------------------------------------------
C
C*	    Execute the selected function.
C
C------------------------------------------------------------------------
	    IF  ( numsym .eq. 1 ) THEN
C
C*		Plot markers.
C
		size = 5.
		CALL GSMRKR ( inum, 1, size, lwid, iret )
		CALL GMARK  ( 'N', 1, xc, yc, iret )
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 2 ) THEN
C
C*		Plot weather symbols.
C
		CALL GSWTHR ( size, lwid, iret )
		CALL GWTHR  ( 'N', 1, float (inum), xc, yc, 0, 0, iret )
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 3 ) THEN
C
C*		Plot cloud type symbols.
C
		CALL GSCTYP ( size, lwid, iret )
		CALL GCTYP  ( 'N', 1, float (inum), xc, yc, 0, 0, iret )
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 4 ) THEN
C
C*		Plot sky cover symbols.
C
		CALL GSSKY  ( size, 1, lwid, iret )
		CALL GSKY   ( 'N', 1, float (inum), xc, yc, 0, 0, iret )
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 5 ) THEN
C
C*		Plot pressure tendency symbols.
C
		CALL GSPTND ( size, lwid, iret )
		CALL GPTND  ( 'N', 1, float (inum), xc, yc, 0, 0, iret )
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 6 ) THEN
C
C*		Plot past weather symbols.
C
		CALL GSPWTH ( size, lwid, iret )
		CALL GPWTH  ( 'N', 1, float (inum), xc, yc, 0, 0, iret )
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 7 ) THEN
C
C*		Plot icing symbols.
C
		CALL GSICNG ( size, lwid, iret )
		CALL GICNG  ( 'N', 1, float (inum), xc, yc, 0, 0, iret )
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 8 ) THEN
C
C*		Plot turbulence symbols.
C
		CALL GSTURB ( size, lwid, iret )
		CALL GTURB  ( 'N', 1, float (inum), xc, yc, 0, 0, iret )
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 9 ) THEN
C
C*		Plot special symbols.
C
		CALL GSSPCL ( size, lwid, iret )
		CALL GSPCL  ( 'N', 1, float (inum), xc, yc, 0, 0, iret )
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 10 ) THEN
C
C*		Plot line dash patterns.
C
		CALL GSLINE ( inum, 1, lwid, 1, iret )
		x(1) = 0.
		x(2) = 1.
		y(1) = yc
		y(2) = yc
		CALL GLINE  ( 'N', 2, x, y, iret )
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 11 ) THEN
C
C*		Plot fronts.
C
		ipipsz = NINT ( size )
		ipipst = NINT ( size )
		ipipdr = 1
		CALL GSFRNT ( inum, ipipsz, ipipst, ipipdr, iret )
		x(1) = 0.
		x(2) = 1.
		y(1) = yc
		y(2) = yc
		CALL GFRNT  ( 'N', 2, x, y, iret )
C------------------------------------------------------------------------
	    END IF
C
	    IF  ( numsym .gt. 0 ) THEN
C
C*		Finish up the plot.
C
	        CALL ER_WMSG ( 'GEMPLT', iret, '', irt )
C
c----		CALL GSSPCL  ( size, lwid, iret )
c----		CALL GSPCL   ( 'N', 1, float ( 0 ), xc, yc, 0, 0, iret )
	        CALL ER_WMSG ( 'GEMPLT', iret, '', irt )
C
	        CALL GEPLOT  ( iret )
	        CALL ER_WMSG ( 'GEMPLT', iret, '', irt )
C
	        CALL GCLOSP  ( iret )
	        CALL ER_WMSG ( 'GEMPLT', iret, '', irt )
	    END IF
	END DO
C
	CALL GENDP   ( 1, iret )
	CALL ER_WMSG ( 'GEMPLT', iret, '', irt )
C*
	END
