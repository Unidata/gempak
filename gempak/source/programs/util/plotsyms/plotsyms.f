	PROGRAM PLOTSYMS
C************************************************************************
C* PLOTSYMS								*
C*									*
C* Program to plot all GEMPAK symbols, markers, and line patterns.	*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/EAI	10/92	Copied from PLOTWSYM			*
C* K. Brill/NMC		12/92	Reformatted output for documentation	*
C* S. Jacobs/EAI	 2/93	More reformatting			*
C* S. Jacobs/NCEP	 5/96	Changed GSDEV to GSDEVA			*
C* M. Linda/GSC		 7/96	Added WTHR 201-203 & markers, revised	*
C* M. Linda/GSC		 8/96	Added ICNG, SPCL, and TURB symbols	*
C* M. Linda/GSC		 9/96	Added an option to frame each symbol	*
C* M. Linda/GSC		 3/97	Added fronts and frames around markers	*
C* S. Jacobs/NCEP	 3/97	Added value to code for turbulence syms	*
C* S. Jacobs/NCEP	 4/97	Removed value from code for turb syms	*
C* S. Jacobs/NCEP	 9/97	Added UTF device selection		*
C* S. Jacobs/NCEP	 9/97	Changed call to GSTEXT			*
C* S. Jacobs/NCEP	 1/98	Added FAX device selection		*
C* S. Jacobs/NCEP	 3/98	Added call to GSDASH for line types	*
C* S. Jacobs/NCEP	 3/98	Added plotting of special lines		*
C* S. Jacobs/NCEP	 4/98	Modified front plotting section		*
C* S. Jacobs/NCEP	 6/98	Changed call to GSFRNT to use REAL size	*
C* G. Krueger/EAI	 7/98	Added SLASH symbol			*
C* G. Krueger/EAI	 8/98	Add STMCNTR, TRPDPRSN, and TRPCYCLN	*
C* T. Lee/GSC		 8/98	Reformatted special lines display	*
C* A. Hardy/GSC		10/98	Added combination weather symbols       *
C* G. Krueger/EAI	10/98	Added fire special symbol		*
C* T. Lee/GSC		12/98	Fixed arg type in GSPCL			*
C* G. Krueger/EAI	 1/99	Add X and LowX specials			*
C* A. Hardy/GSC		 1/99	Added calls to GSTANM and GENANM        *
C* S. Jacobs/NCEP	 2/99	Added type 900 front for squall line	*
C* S. Jacobs/NCEP	 2/99	Added more combination symbols		*
C* S. Jacobs/NCEP	 2/99	Added TIFF device selection		*
C* D.W.Plummer/NCEP      2/99   Added blank background capability       *
C* S. Jacobs/NCEP	 3/99	Added number 22 to special lines	*
C* G. Krueger/EAI	 8/99	N & SH trop storm specials		*
C* D. Kidwell/NCEP	 4/00	Added number 22 to markers              *
C* S. Jacobs/NCEP	 1/01	Set background clr for lines and fronts	*
C* S. Jacobs/NCEP	 1/01	Set second clr for special lines	*
C* S. Jacobs/NCEP	 3/01	Added new front 809 for front w/ no pips*
C* S. Jacobs/NCEP	 9/01	Added new special sym - Nuclear Fallout	*
C* J. Wu/SAIC	 	10/01	Added number 23 to special lines	*
C* J. Wu/SAIC	 	10/01	Added number 24, 25 to special lines	*
C* S. Jacobs/NCEP	11/01	Fixed error in counting special lines	*
C* m.gamazaychikov/SAIC 04/03	Increased special symbols number to 49  *
C* m.gamazaychikov/SAIC 04/03	Increased combo symbols number to 28	*
C* A. Hardy/NCEP	12/03   Increased special symbols 49 -> 50	*
C* S. Gilbert/NCEP	 6/05	Added special line 26			*
C* T. Piper/SAIC	07/05	Added lbl and [c|h|l|m]char variables	*
C* L. Hinson/AWC        01/07   Increased special symbols 50 -> 54      *
C* S. Jacobs/NCEP	 5/09	Added special symbols 55, 56		*
C* S. Jacobs/NCEP	 5/09	Set XW window size to 1000x770		*
C* L. Hinson/AWC        07/09   Add special symbols 57 & 58             *
C* S. Jacobs/NCEP	 4/10	Use XW dimensions for the GF driver	*
C* S. Jacobs/NCEP	 2/14	Added GIF driver as an option		*
C************************************************************************
	CHARACTER	str*32, device*12, filnam*72
	CHARACTER	title*64, info*32, csize*12, cwidth*12
	CHARACTER	cchar*4, hchar*4, lchar*4, mchar*4, lbl(3)*13
	REAL		x(2), y(2)
	LOGICAL		baddev, plinfo
	REAL		xbg(4), ybg(4)
C*
	DATA		cchar / 'C' /, hchar / 'H' /, lchar / 'L' /
	DATA		mchar / 'M' /, plinfo / .TRUE. /
	DATA		lbl / 'LINE TYPES', 'FRONTS', 'SPECIAL LINES' /
	DATA		ytop / .70 /, xctr / .5 /, itxc / 1 /
	DATA		xbg / 0.0, 1.0, 1.0, 0.0 /
	DATA		ybg / 0.0, 0.0, 1.0, 1.0 /
C------------------------------------------------------------------------
C*	Intialize GEMPLT.
C
	CALL IN_BDTA ( iret )
	CALL GINITP ( 1, istat, iret )
C
C*	Loop until EXIT selected by user.
C
	ifirst = 1
	iostat = 0
	DO  WHILE ( iostat .eq. 0 )
C
C*	    Let user select a function.
C
	    IF ( ifirst .eq. 1 ) THEN
		numsym = 0
		ifirst = 0
	    ELSE
		WRITE ( 6, 10 )
10		FORMAT (
     +     ' ' /
     +     '  0 = Change device      1 = Markers     2 = Line Patterns'/
     +     '  3 = Weather Symbols    4 = Cloud Type  5 = Past Weather '/
     +     '  6 = Pressure Tendency  7 = Sky Cover   8 = Icing        '/
     +     '  9 = Special Symbols   10 = Turbulence 11 = Fronts       '/
     +     ' 12 = Special Lines     13 = Combo Syms '/
     +     ' 99 = Set Attribute Plotting '/
     +     ' ' )
		CALL TM_INT ( 'Select a symbol type number', .false.,
     +			      .false., 1, numsym, n, ier )
		IF  ( ier .eq. 2 )  THEN
		    iostat = -1
		    numsym = -1
		END IF
C
C*		First check attribute option.
C
		IF  ( numsym .eq. 99 )  THEN
		    WRITE ( 6, * ) 'Do you want attributes (T or F):'
		    READ  ( 5, * ) plinfo
		    numsym = -1
		END IF
C
C*		If symbols selected, input symbol attributes.
C
		IF  ( numsym .gt. 0 )  THEN
		    CALL TM_INT ( 'Enter symbol color' , 
     +				  .false., .false., 1, icolr, n, ier )
		    IF  ( ier .eq. 2 )  THEN
			iostat = -1
			numsym = -1
		      ELSE
			IF  ( ( numsym .eq. 11 ) .or.
     +			      ( numsym .eq. 12 ) )  THEN
			    CALL TM_INT ( 'Enter second front color' , 
     +					  .false., .false., 1,
     +					  iclr2, n, ier )
			    IF  ( ier .eq. 2 )  THEN
				iostat = -1
				numsym = -1
			    END IF
			END IF
		    END IF
		END IF
C
		IF  ( numsym .gt. 0 )  THEN
		    CALL TM_REAL ( 'Enter symbol size' , 
     +				   .false., .false., 1, szsym, n, ier )
		    IF  ( ier .eq. 2 )  THEN
			iostat = -1
			numsym = -1
		    ELSE
			CALL ST_RLCH ( szsym, 1, csize, iret )
			CALL ST_LSTR ( csize, lensiz, iret )
		    END IF
		END IF
C
		IF  ( numsym .gt. 0 )  THEN
		    CALL TM_INT ( 'Enter symbol line width' , 
     +				  .false., .false., 1, lwidth, n, ier )
		    IF  ( ier .eq. 2 )  THEN
			iostat = -1
			numsym = -1
		    ELSE
			CALL ST_INLN ( lwidth, cwidth, lenwid, iret )
		    END IF
		END IF
C
		IF  ( ( numsym .gt.  0 ) .and.
     +		      ( numsym .ne.  2 ) .and. ( numsym .ne. 11 ) .and.
     +		      ( numsym .ne. 12 ) .and. ( numsym .ne. 99 ) ) THEN
		    CALL TM_INT ( 'Enter frame color, 0 for none,', 
     +				  .false., .false., 1, ifrmc, n, ier )
		    IF  ( ier .eq. 2 )  THEN
			iostat = -1
			numsym = -1
		    END IF
		END IF
C
		IF  ( ( numsym .gt.  0 ) .and.
     +		      ( numsym .ne. 99 ) ) THEN
		    CALL TM_INT ( 'Enter background color, 0 for none,', 
     +				  .false., .false., 1, ibgcol, n, ier )
		    IF  ( ier .eq. 2 )  THEN
			iostat = -1
			numsym = -1
		    END IF
		END IF
C
		IF  ( numsym .gt. 0 )  THEN
		    CALL GCLEAR ( iret )
C
C*		    If background color requested, plot it now.
C
		    IF ( ibgcol .ne. 0 )  THEN
			CALL GSCOLR ( ibgcol, ier )
			CALL GFILL ( 'N', 4, xbg, ybg, ier )
		    END IF
		    CALL GSCOLR ( itxc, iret )
		    CALL GSTEXT ( 0, 1, 0, lwidth, 0, 0, 0, iret )
C
		    IF ( plinfo )  THEN
		        info = ' ( SIZE=' // csize(:lensiz) // 
     +			        ', WIDTH=' // cwidth(:lenwid) // ' )'
		    ELSE
		        info = ' '
		    END IF
C
		END IF
	    END IF
C
C*	    Execute the selected function.
C
C------------------------------------------------------------------------
	    IF ( numsym .eq. 0 )  THEN
C
C*		Let user specify the device driver to be used.
C
		WRITE ( 6, * ) 'Enter device:'
C
		READ  ( 5, 100 ) device
100		FORMAT ( A )
C
		CALL ST_LCUC ( device, device, ier )
		CALL ST_RMBL ( device, device, ilen, ier )
		iunit  = 1
		itype  = 1
		xsize  = -9999.0
		ysize  = -9999.0
		baddev = .false.
		IF  ( device .eq. 'XW' )  THEN
		    filnam = 'GEMPAK'
		    xsize  = 1000
		    ysize  =  770
		  ELSE IF ( device .eq. 'PS' )  THEN
		    filnam = 'ps.plt'
		    itype  = 2
		    xsize  = 11.0
		    ysize  = 8.5
		  ELSE IF ( device .eq. 'NC' )  THEN
		    filnam = 'Nmeta'
		  ELSE IF ( device .eq. 'GF' )  THEN
		    filnam = 'gempak.gif'
		    xsize  = 1000
		    ysize  =  770
		  ELSE IF ( device .eq. 'GIF' )  THEN
		    filnam = 'gempak.gif'
		    xsize  = 1000
		    ysize  =  770
		  ELSE IF ( device .eq. 'UTFN' )  THEN
		    device = 'UTF'
		    filnam = 'T02'
		    itype  = 2
		  ELSE IF ( device .eq. 'UTFA' )  THEN
		    device = 'UTF'
		    filnam = 'T02'
		  ELSE IF ( device .eq. 'FAX' )  THEN
		    filnam = '999X;0167'
		  ELSE IF ( device .eq. 'TIFF' )  THEN
		    filnam = 'AAAA00'
		  ELSE
		    WRITE (*,*) 'Invalid device'
		    ifirst = 1
		    baddev = .true.
		END IF
		IF  ( .not. baddev )  THEN
		    CALL GSDEVA ( device, iunit, filnam, itype,
     +				  xsize, ysize, iret )
                    CALL GSTANM ( iret )
		END IF
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 1 )  THEN
C
C*		Plot markers.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		title = 'MARKERS' // info
		CALL ST_LSTR ( title, lens, ier )
		CALL GTEXT ( 'N', xctr, ytop, title, 0, -lens, 0, ier )
		CALL GSTEXT ( 0, 1, 0, lwidth, 0, 0, 0, iret )
		DO  i = 1, 3
		  DO  j = 1, 10
		    inum = ( i - 1 ) * 10 + j
		    IF ( inum .le. 22 ) THEN
			xc = xctr - .205 + ( i - 1 ) * .17
			yc = ytop - j * .05
			CALL ST_INLN ( inum, str, lens, iret )
			CALL GSCOLR ( itxc, iret )
			ixoff = 0
			IF ( inum .gt. 9 ) ixoff = -2
		        CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
			CALL GTEXT ( 'N', xc, yc, str(1:lens), 0, ixoff,
     +				     0, iret )
		        CALL GSTEXT ( 0, 1, 0, lwidth, 0, 0, 0, iret )
			IF  ( ifrmc .ne. 0 ) THEN
			    CALL GSCOLR ( ifrmc, iret )
			    CALL GSSPCL ( szsym, 1, iret )
			    CALL GSPCL  ( 'N', 1, 0., xc+.06, yc, 0, 0,
     +					  iret )
			END IF
			CALL GSCOLR ( icolr, iret )
C			IF ( inum .eq. 15 ) THEN
C		            idotsz = lwidth * ( INT ( szsym ) + 1 )
C			    CALL GSMRKR ( inum, 2, 1., idotsz, iret )
C			ELSE
			    CALL GSMRKR ( inum, 2, szsym, lwidth, iret )
C			END IF
			CALL GMARK  ( 'N', 1, xc+.06, yc, iret )
		    END IF
		  END DO
		END DO
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 2 )  THEN
C
C*		Plot line dash patterns.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		CALL GTEXT ( 'N', xctr, ytop,
     +			     lbl(1), 0, -10, 0, ier )
		DO  i = 1, 3
		    DO  j = 1, 10
			xc = xctr - .275 + ( i - 1 ) * .2
			yc = ytop - j * .05
			inum = i * 10 + j - 1
			CALL ST_INLN ( inum, str, lens, iret )
			CALL GSCOLR ( itxc, iret )
		        CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
			CALL GTEXT ( 'N', xc, yc, str(1:lens), 0,
     +				     -4, 0, iret )
		        CALL GSTEXT ( 0, 1, 0, lwidth, 0, 0, 0, iret )
			CALL GSCOLR ( icolr, iret )
			CALL GSLINE ( inum, 0, lwidth, 0, iret )
			CALL GSDASH ( szsym, iret )
			x(1) = xc + .004
			x(2) = xc + .15
			y(1) = yc
			y(2) = yc
			CALL GLINE ( 'N', 2, x, y, iret )
		    END DO
		END DO
		CALL GSDASH ( 1.0, iret )
C------------------------------------------------------------------------
	    ELSE IF  ( numsym .eq. 3 )  THEN
C
C*		Plot weather symbols.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		title = 'WEATHER SYMBOLS' // info
		CALL ST_LSTR ( title, lens, ier )
		CALL GTEXT ( 'N', xctr, ytop, title, 0, -lens, 0, ier )
		DO  i = 1, 10
		    xc = xctr - .25 + i * .05
		    yc = ytop - .05
		    CALL ST_INLN ( i - 1, str, lens, iret )
		    CALL GTEXT ( 'N', xc, yc, str(1:lens), 0, 0, 0, iret)
		END DO
		CALL GSTEXT ( 0, 1, 0, lwidth, 0, 0, 0, iret )
C
		DO  j = 1, 12
		    xc = xctr - .25
		    yc = ytop - .05 * ( j + 1 )
		    inum = ( j - 1 ) * 10
		    IF ( j .eq. 12 ) inum = 200
		    ixoff = 0
		    IF ( inum .gt.  9 ) ixoff = -2
		    IF ( inum .gt. 99 ) ixoff = -4
		    CALL ST_INLN ( inum, str, lens, iret )
		    CALL GSCOLR ( itxc, iret )
		    CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		    CALL GTEXT ( 'N', xc, yc, str(1:lens), 0, ixoff, 0,
     +				 iret )
		    CALL GSTEXT ( 0, 1, 0, lwidth, 0, 0, 0, iret )
		    DO  i = 1, 10
			xc = xctr - .25 + i * .05
			IF  ( ifrmc .ne.   0 ) THEN
			    IF  ( inum .le. 99 .or.
     +				  inum .eq. 103 .or.
     +				  inum .eq. 104 .or.
     +				  inum .eq. 105 .or.
     +				  inum .eq. 107 .or.
     +				  inum .eq. 201 .or.
     +				  inum .eq. 202 .or.
     +				  inum .eq. 203 ) THEN
				CALL GSCOLR ( ifrmc, iret )
				CALL GSSPCL ( szsym, 1, iret )
				CALL GSPCL  ( 'N', 1, 0., xc, yc, 0, 0,
     +					      iret )
			    END IF
			END IF
			CALL GSCOLR ( icolr, iret )
			CALL GSWTHR ( szsym, lwidth, iret )
			CALL GWTHR  ( 'N', 1, float ( inum ), xc, yc,
     +				      0, 0, iret )
			inum = inum + 1
		    END DO
		END DO
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 4 )  THEN
C
C*		Plot cloud type symbols.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		title = 'CLOUD TYPE SYMBOLS' // info
		CALL ST_LSTR ( title, lens, ier )
		CALL GTEXT ( 'N', xctr, ytop, title, 0, -lens, 0, ier )
		yc = ytop - .05
		xc = xctr - .175
		CALL GTEXT ( 'N', xc, yc, cchar, 0, 0, 0, ier )
		CALL GTEXT ( 'N', xc, yc, lchar, 0, 2, -1, ier )
		xc = xc + .17
		CALL GTEXT ( 'N', xc, yc, cchar, 0, 0, 0, ier )
		CALL GTEXT ( 'N', xc, yc, mchar, 0, 2, -1, ier )
		xc = xc + .17
		CALL GTEXT ( 'N', xc, yc, cchar, 0, 0, 0, ier )
		CALL GTEXT ( 'N', xc, yc, hchar, 0, 2, -1, ier )
		DO  i = 1, 9
		    DO  j = 1, 3
			xc = xctr - .205 + ( j - 1 ) * .17
			yc = ytop  - ( i + 1 ) * .05
			inum = ( j - 1 ) * 10 + i
			ipnum = i
			CALL ST_INLN ( ipnum, str, lens, iret )
			CALL GSCOLR ( itxc, iret )
		        CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
			CALL GTEXT ( 'N', xc, yc, str(1:lens),
     +				     0, 0, 0, iret )
		        CALL GSTEXT ( 0, 1, 0, lwidth, 0, 0, 0, iret )
			IF  ( ifrmc .ne. 0 ) THEN
			    CALL GSCOLR ( ifrmc, iret )
			    CALL GSSPCL ( szsym, 1, iret )
			    CALL GSPCL  ( 'N', 1, 0., xc+.06, yc, 0, 0,
     +					  iret )
			END IF
			CALL GSCOLR ( icolr, iret )
			CALL GSCTYP ( szsym, lwidth, iret )
			CALL GCTYP ( 'N', 1, float (inum), xc+.06, yc,
     +				     0, 0, iret )
		    END DO
		END DO
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 5 )  THEN
C
C*		Plot past weather symbols.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		title = 'PAST WEATHER SYMBOLS' // info
		CALL ST_LSTR ( title, lens, ier )
		CALL GTEXT ( 'N', xctr, ytop, title, 0, -lens, 0, ier )
		DO  i = 1, 10
		    xc = xctr - .05
		    yc = ytop - i * .05
		    inum = i - 1
		    CALL ST_INLN ( inum, str, lens, iret )
		    CALL GSCOLR ( itxc, iret )
		    CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		    CALL GTEXT ( 'N', xc, yc, str(1:lens), 0, 0, 0, ire )
		    CALL GSTEXT ( 0, 1, 0, lwidth, 0, 0, 0, iret )
		    xc = xc + .1
		    IF  ( ( ifrmc .ne. 0 ) .and. ( inum .ge. 3 ) ) THEN
			CALL GSCOLR ( ifrmc, iret )
			CALL GSSPCL ( szsym, 1, iret )
			CALL GSPCL  ( 'N', 1, 0., xc, yc, 0, 0, iret )
		    END IF
		    CALL GSCOLR ( icolr, iret )
		    CALL GSPWTH ( szsym, lwidth, iret )
		    CALL GPWTH ( 'N', 1, float(inum), xc, yc, 0, 0, ire )
		END DO
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 6 )  THEN
C
C*		Plot pressure tendency symbols.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		title = 'PRESSURE TENDENCY SYMBOLS' // info
		CALL ST_LSTR ( title, lens, ier )
		CALL GTEXT ( 'N', xctr, ytop, title, 0, -lens, 0, ier )
		DO  i = 1, 9
		    xc = xctr - .05
		    yc = ytop - i * .05
		    inum = ( i - 1 ) * 1000 + 5
		    CALL ST_INLN ( i - 1, str, lens, iret )
		    CALL GSCOLR ( itxc, iret )
		    CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		    CALL GTEXT ( 'N', xc, yc, str(1:lens), 0, 0, 0, ire )
		    CALL GSTEXT ( 0, 1, 0, lwidth, 0, 0, 0, iret )
		    xc = xc + .1
		    IF  ( ifrmc .ne. 0 ) THEN
			CALL GSCOLR ( ifrmc, iret )
			CALL GSSPCL ( szsym, 1, iret )
			CALL GSPCL  ( 'N', 1, 0., xc, yc, 0, 0, iret )
		    END IF
		    CALL GSCOLR ( icolr, iret )
		    CALL GSPTND ( szsym, lwidth, iret )
		    CALL GPTND ( 'N', 1, float(inum), xc, yc, 0, 0, ire )
		END DO
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 7 )  THEN
C
C*		Plot sky cover symbols.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		title = 'SKY COVER SYMBOLS' // info
		CALL ST_LSTR ( title, lens, ier )
		CALL GTEXT ( 'N', xctr, ytop, title, 0, -lens, 0, ier )
		DO  i = 1, 11
		    xc = xctr - .05
		    yc = ytop - i * .05
		    inum = i - 1
		    CALL ST_INLN ( inum, str, lens, iret )
		    CALL GSCOLR ( itxc, iret )
		    ixoff = 0
		    IF ( inum .gt. 9 ) ixoff = -2
		    CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		    CALL GTEXT ( 'N', xc, yc, str(1:lens), 0, ixoff, 0,
     +				 iret )
		    CALL GSTEXT ( 0, 1, 0, lwidth, 0, 0, 0, iret )
		    xc = xc + .1
		    IF  ( ifrmc .ne. 0 ) THEN
			CALL GSCOLR ( ifrmc, iret )
			CALL GSSPCL ( szsym, 1, iret )
			CALL GSPCL  ( 'N', 1, 0., xc, yc, 0, 0, iret )
		    END IF
		    CALL GSCOLR ( icolr, iret )
		    CALL GSSKY ( szsym, 1, lwidth, iret )
		    CALL GSKY  ( 'N', 1, float(inum), xc, yc, 0, 0, ire )
		END DO
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 8 )  THEN
C
C*		Plot icing symbols.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		title = 'ICING SYMBOLS' // info
		CALL ST_LSTR ( title, lens, ier )
		CALL GTEXT ( 'N', xctr, ytop, title, 0, -lens, 0, ier )
		DO  i = 1, 11
		    xc = xctr - .05
		    yc = ytop - i * .05
		    inum = i - 1
		    CALL ST_INLN ( inum, str, lens, iret )
		    CALL GSCOLR ( itxc, iret )
		    CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		    CALL GTEXT ( 'N', xc, yc, str(1:lens), 0, 0, 0, ire )
		    CALL GSTEXT ( 0, 1, 0, lwidth, 0, 0, 0, iret )
		    xc = xc + .1
		    IF  ( ifrmc .ne. 0 ) THEN
			CALL GSCOLR ( ifrmc, iret )
			CALL GSSPCL ( szsym, 1, iret )
			CALL GSPCL  ( 'N', 1, 0., xc, yc, 0, 0, iret )
		    END IF
		    CALL GSCOLR ( icolr, iret )
		    CALL GSICNG ( szsym, lwidth, iret )
		    CALL GICNG ( 'N', 1, float(inum), xc, yc, 0, 0, ire )
		END DO
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 9 )  THEN
C
C*		Plot special symbols.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		title = 'SPECIAL SYMBOLS' // info
		CALL ST_LSTR ( title, lens, ier )
		CALL GTEXT ( 'N', xctr, ytop, title, 0, -lens, 0, ier )
		DO  i = 1, 6 
		    DO  j = 1, 11
			xc = xctr - .29 + ( i - 1 ) * .11
			yc = ytop - j * .05
			inum = ( i - 1 ) * 11 + j - 1
			IF  ( inum .le. 58 ) THEN
			    ixoff = 0
			    IF ( inum .gt. 9 ) ixoff = -2
			    CALL ST_INLN ( inum, str, lens, iret )
			    CALL GSCOLR ( itxc, iret )
		            CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
			    CALL GTEXT ( 'N', xc, yc, str(1:lens), 0,
     +					 ixoff, 0, iret )
		            CALL GSTEXT ( 0, 1, 0, lwidth, 0,0,0, iret )
			    IF  ( ifrmc .ne. 0 ) THEN
				CALL GSCOLR ( ifrmc, iret )
				CALL GSSPCL ( szsym, 1, iret )
				CALL GSPCL  ( 'N', 1, 0.,
     +					      xc+.0375, yc,
     +					      0, 0, iret )
			    END IF
			    CALL GSCOLR ( icolr, iret )
			    CALL GSSPCL ( szsym, lwidth, iret )
			    CALL GSPCL ( 'N', 1, float(inum),
     +					 xc+.0375, yc,
     +					 0, 0, iret )
			END IF
		    END DO
		END DO
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 10 )  THEN
C
C*		Plot turbulence symbols.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		title = 'TURBULENCE SYMBOLS' // info
		CALL ST_LSTR ( title, lens, ier )
		CALL GTEXT ( 'N', xctr, ytop, title, 0, -lens, 0, ier )
		DO  i = 1, 9
		    xc = xctr - .05
		    yc = ytop - i * .05
		    inum = i - 1
		    CALL ST_INLN ( inum, str, lens, iret )
		    CALL GSCOLR ( itxc, iret )
		    CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		    CALL GTEXT ( 'N', xc, yc, str(1:lens), 0, 0, 0, ire )
		    CALL GSTEXT ( 0, 1, 0, lwidth, 0,0,0, iret )
		    xc = xc + .1
		    IF  ( ifrmc .ne. 0 ) THEN
			CALL GSCOLR ( ifrmc, iret )
			CALL GSSPCL ( szsym, 1, iret )
			CALL GSPCL  ( 'N', 1, 0., xc, yc, 0, 0, iret )
		    END IF
		    CALL GSCOLR ( icolr, iret )
		    CALL GSTURB ( szsym, lwidth, iret )
		    CALL GTURB ( 'N', 1, float(inum), xc, yc, 0, 0, ire )
		END DO
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 11 )  THEN
C
C*		Plot weather fronts.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		CALL GTEXT ( 'N', xctr, ytop,
     +			     lbl(2), 0, -6, 0, ier )
		DO  i = 1, 16
		    IF      ( i .le.  6 ) THEN
			xc = xctr - .275
			yc = ytop - .05 - (i - 1 ) * .05
		    ELSE IF ( i .le. 12 ) THEN
			xc = xctr + .025
			yc = ytop - .05 - (i - 7 ) * .05
		    ELSE
			xc = xctr - .125
			yc = ytop - .05 - (i - 7 ) * .05
		    END IF
C
		    IF ( i .eq.  1 ) ifcod = 000
		    IF ( i .eq.  2 ) ifcod = 005
		    IF ( i .eq.  3 ) ifcod = 008
		    IF ( i .eq.  4 ) ifcod = 200
		    IF ( i .eq.  5 ) ifcod = 205
		    IF ( i .eq.  6 ) ifcod = 208
		    IF ( i .eq.  7 ) ifcod = 400
		    IF ( i .eq.  8 ) ifcod = 405
		    IF ( i .eq.  9 ) ifcod = 408
		    IF ( i .eq. 10 ) ifcod = 600
		    IF ( i .eq. 11 ) ifcod = 605
		    IF ( i .eq. 12 ) ifcod = 608
		    IF ( i .eq. 13 ) ifcod = 700
		    IF ( i .eq. 14 ) ifcod = 800
		    IF ( i .eq. 15 ) ifcod = 809
		    IF ( i .eq. 16 ) ifcod = 900
		    ifcod = ifcod + lwidth * 10
C
		    ixoff = -6
		    CALL ST_INLN ( ifcod, str, lens, iret )
		    CALL GSCOLR ( itxc, iret )
		    CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		    CALL GTEXT ( 'N', xc, yc, str(1:lens), 0,
     +			  	 ixoff, 0, iret )
		    CALL GSTEXT ( 0, 1, 0, lwidth, 0,0,0, iret )
C
		    CALL GSCLR2 ( icolr, iclr2, iret )
		    CALL GSFRNT ( ifcod, szsym, 1, 1, iret )
		    x(1) = xc + .004
		    x(1) = xc + .006
		    x(2) = xc + .15
		    x(2) = xc + .25
		    y(1) = yc
		    y(2) = yc
		    CALL GFRNT ( 'N', 2, x, y, iret )
		END DO
C------------------------------------------------------------------------
	    ELSE IF ( numsym .eq. 12 )  THEN
C
C*		Plot special lines.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		CALL GTEXT ( 'N', xctr, ytop,
     +			     lbl(3), 0, -13, 0, ier )
		sp = .02 
		DO  j = 1, 3
		    IF  ( j .eq. 1 )  xinc = -xctr + sp
		    IF  ( j .eq. 2 )  xinc = -0.14 - sp
		    IF  ( j .eq. 3 )  xinc =  0.14 + sp
		    DO  i = 1, 9
			xc = xctr + xinc
			yc = ytop - .06 - ( i - 1 ) * .07
C
			ixoff = 0
			inum = ( j - 1 ) * 9 + i
			IF  ( inum .le. 26 )  THEN
			    CALL ST_INLN ( inum, str, lens, iret )
			    CALL GSCOLR ( itxc, iret )
		            CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
			    CALL GTEXT ( 'N', xc, yc, str(1:lens), 0,
     +					 ixoff, 0, iret )
		            CALL GSTEXT ( 0, 1, 0, lwidth, 0,0,0, iret )
C
			    CALL GSCLR2 ( icolr, iclr2, iret )
			    CALL GSSPLN ( inum, 1, 1, szsym, lwidth,
     +					  iret )
			    x(1) = xc + .025
			    y(1) = yc
			    x(2) = xc + .275
			    y(2) = yc
			    CALL GSPLN ( 'N', 2, x, y, iret )
			END IF
		    END DO
		END DO
C------------------------------------------------------------------------
	    ELSE IF  ( numsym .eq. 13 )  THEN
C
C*		Plot combination weather symbols.
C
		CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		title = 'COMBINATION WEATHER SYMBOLS' // info
		CALL ST_LSTR ( title, lens, ier )
		CALL GTEXT ( 'N', xctr, ytop, title, 0, -lens, 0, ier )
		DO  i = 1, 5 
		    DO  j = 1, 6
		        xc = xctr - .39 + ( i - 1 ) * .17
		        yc = ytop - j * .10
		        inum =  ( i - 1  ) * 6 + j
		        IF ( inum .le. 28 ) THEN
		            ixoff = 0 
		            IF ( inum .gt.  6 ) ixoff = -2
		            CALL ST_INLN ( inum, str, lens, iret )
		            CALL GSCOLR ( itxc, iret )
		            CALL GSTEXT ( 0, 1, 0, 1, 0, 0, 0, iret )
		            CALL GTEXT ( 'N', xc, yc, str(1:lens), 0, 
     +			    	          ixoff, 0, iret )
		            CALL GSTEXT ( 0, 1, 0, lwidth, 0,0,0, iret )
			    IF  ( ifrmc .ne. 0 ) THEN
			        CALL GSCOLR ( ifrmc, iret )
			        CALL GSSPCL ( szsym*2.4, 1, iret )
			        CALL GSPCL  ( 'N', 1, 0., xc+0.06, yc, 
     +					       0, 0, iret )
		    	    END IF
			    CALL GSCOLR ( icolr, iret )
			    CALL GSCMBO ( szsym, lwidth, iret )
			    CALL GCMBO  ( 'N', 1, float ( inum ), 
     +				          xc+0.06, yc, 0, 0, iret )
                        END IF
		    END DO
		END DO
C------------------------------------------------------------------------
	    END IF
	    CALL GEPLOT ( iret )
            CALL GENANM ( iret )
	END DO
C*
	END
