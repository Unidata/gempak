	SUBROUTINE DTEXT ( iwndw, x, y, cchar, lens, rotat, ixoff, iyoff,
     +			   iret )
C************************************************************************
C* DTEXT								*
C*									*
C* This subroutine plots a text string.  The reference point		*
C* (X,Y) determines the center of the first character.  The text	*
C* string may be rotated from horizontal at the reference point and	*
C* offset along the rotated X and Y coordinates.  Positive X offsets	*
C* are toward the right;  positive Y offsets are toward the top.  The	*
C* text is drawn using attributes defined by GSTEXT.			*
C*									*
C* The text string may contain carriage returns and/or line feed	*
C* characters.  A carriage return/line feed will terminate one line of	*
C* text and begin a new line.  Each new line will be offset normal to	*
C* the rotation from the previous line and oriented along the direction	*
C* of rotation.  All resulting lines of text will be placed so that	*
C* they are centered normal to the rotation.				*
C*									*
C* DTEXT ( IWNDW, X, Y, CCHAR, LENS, ROTAT, IXOFF, IYOFF, IRET )	*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	X		REAL		X coordinate in device units	*
C*	Y		REAL		Y coordinate in device units	*
C*	CCHAR		CHAR*		Text string to plot		*
C*	LENS		INTEGER		Length of string		*
C*	ROTAT		REAL		Rotation angle in degrees	*
C*	IXOFF		INTEGER		X offset in half characters	*
C*	IYOFF		INTEGER		Y offset in half characters	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* S. Schotz/GSC	 1/90	Update for text width addition		*
C* M. Linda/GSC		 1/97	Changed X and Y to reals		*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* S. Jacobs/NCEP	 7/97	Added ability to plot multiple lines	*
C* S. Jacobs/NCEP	 9/97	Added justification from DTEXTC		*
C* S. Jacobs/NCEP	12/97	Changed to MOD justification for SW text*
C* S. Jacobs/NCEP	 1/98	Added call to ITXBOX			*
C* S. Jacobs/NCEP	 2/98	Do not rotate box if HW font; Misc fix	*
C* S. Jacobs/NCEP	 3/98	Added check for bold SW fonts		*
C* S. Jacobs/NCEP	 7/98	Changed ttxsz to txsize			*
C* S. Jacobs/NCEP	 2/99	Increased size of tchar from 160 to 400	*
C* B. Yin/SAIC		 1/04	Added ability to draw degree sign       *
C* B. Yin/SAIC           3/04   Modified lens to a local variabe ilens  *
C* B. Yin/SAIC           3/04   Modified to display degree sign for HP  *
C* B. Yin/SAIC           3/04   Removed the code for HP XW fonts        *
C* R. Jones/NCEP	10/06	Save and restore active color		*
C* S. Jacobs/NCEP	10/07	Added s/w fonts 3 and above - ITEXT3	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*)	cchar
C*
	INTEGER		icrloc (50), icntr, itmlns, icode, ilens, ier
	CHARACTER	tchar*400, tmpchr*400
C------------------------------------------------------------------------
	iret  = NORMAL
	ilens = lens
        ksvcolr = mcolr
C
C*	If the driver is VG, send the points directly to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HTEXT ( x, y, cchar, ilens, ixoff, iyoff, rotat, iret )
	    RETURN
	END IF
C
C*	Check escape sequence 
C*	Backslash followed by three number are special characters
C*	Keep one backslash if there are two
C*	For device RBK, skip backslash followed by three numbers
C
	icntr = 1
	itmlns = 0
	DO WHILE ( icntr .le. ilens )
	   IF  ( ICHAR(cchar( icntr : icntr )) .eq. 92 )  THEN
	       IF ( ICHAR(cchar( icntr+1 : icntr+1 )) .eq. 92 ) THEN
	           tmpchr( itmlns + 1 : ) = cchar( icntr:icntr )
	           icntr = icntr + 2
	         ELSE
	           CALL ST_NUMB( cchar( icntr+1 : icntr+3 ), 
     +                          icode, ier )
	           IF  ( ier .eq. 0 )  THEN
                       IF  ( ddev .eq. 'RBK' )  THEN
                           itmlns = itmlns - 1
                         ELSE
	                   icode = ( icode / 100 * 64 
     +	                         + ( icode - icode /100*100) / 10 * 8 
     +	                         + ( icode - icode / 10 * 10 ) )
                           tmpchr( itmlns + 1 : ) = CHAR( icode )
                       END IF
	               icntr = icntr + 4
	             ELSE
	               tmpchr( itmlns + 1 : ) =  
     +                         cchar( icntr : icntr )
	               icntr = icntr + 1
	           END IF
	       END IF
	     ELSE
	       tmpchr( itmlns + 1 : ) = cchar( icntr : icntr )
	       icntr = icntr + 1
	   END IF
	   itmlns = itmlns + 1
	END DO

	IF  ( itmlns .ne. 0 ) THEN 
	    cchar = tmpchr( 1 : itmlns )
	    ilens = itmlns
	  ELSE
	    cchar = ' '
	    ilens  = 1
	END IF
C
C*	If bold text is requested for the software fonts, increase
C*	the line width. Only if the user requests font number 1 or 2.
C
	ilwid = mtxwid
	IF  ( mtxhw .ne. 2 )  THEN
	    ifont = MOD ( mtxfn, 10 )
	    istyl = mtxfn / 10
	    IF  ( ( istyl .ge. 2 ) .and. ( ifont .lt. 3 ) )  THEN
	      	ilwid = ilwid + 2
	    END IF
	END IF
C
C*	Save line type and line width.
C
	IF  ( ( mltyp .ne. 1 ) .or. ( mlwid .ne. ilwid ) ) THEN
	    jltyp = mltyp
	    jlwid = mlwid
	    CALL DSLINE ( 1, 0, ilwid, 0, i1, i2, i3, i4, ier )
	  ELSE
	    jltyp = 0
	    jlwid = 0
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP ( iwndw, ier )
C
C*	Compute text size.
C
	size = txsize * bscalc
C
C*	Draw text if size is greater than 0.
C
	IF  ( size .gt. 0. ) THEN
C
C*	    If a bounding box is requested, add text for the Low, High,
C*	    and Zero degree types.
C
	    tchar = cchar
	    ibr = mbrdr / 100
	    ifl = MOD ( mbrdr, 100 ) / 10
	    itp = MOD ( mbrdr, 10 )
	    IF  ( itp .eq. 2 )  THEN
		tchar = cchar(1:ilens) // CHCR // 'L'
	      ELSE IF  ( itp .eq. 3 )  THEN
		tchar = 'H' // CHCR // cchar(1:ilens)
	      ELSE IF  ( itp .eq. 4 )  THEN
C
C*		Add degree sign in the freezing level box
C
		   tchar = '0' // CHAR(176) // ': ' // cchar(1:ilens)
	    END IF
C
C*	    Get the length of the final string.
C
	    CALL ST_LSTR ( tchar, lent, ier )
C
C*	    Find all carriage returns and line feeds in the string.
C
	    icrs  = 0
	    mxlen = 0
	    nlen  = 0
	    DO  i = 1, lent
		IF  ( ( tchar(i:i) .eq. CHCR ) .or.
     +		      ( tchar(i:i) .eq. CHLF ) )  THEN
		    icrs  = icrs + 1
		    icrloc(icrs) = i
		    mxlen = MAX ( mxlen, nlen )
		    nlen  = 0
		  ELSE
		    nlen = nlen + 1
		END IF
	    END DO
	    icrs  = icrs + 1
	    icrloc(icrs) = lent + 1
	    mxlen = MAX ( mxlen, nlen )
C
C*	    Draw any bounding box or background fill, if requested.
C
	    IF  ( ( ibr .eq. 2 ) .or. ( ifl .eq. 2 ) )  THEN
C
C*		If plotting hardware fonts, do not rotate the bouding
C*		box and background fill, since hardware fonts do not
C*		rotate.
C
		IF  ( mtxhw .ne. 2 )  THEN
		    rot2 = rotat
		  ELSE
		    rot2 = 0.0
		END IF
		CALL ITXBOX ( x, y, ixoff, iyoff, mxlen, icrs,
     +			      rot2, ierr )
	    END IF
C
C*	    Loop over all carriage returns adding one for the end
C*	    of the string.
C
	    iyoff2 = iyoff + icrs - 1
	    istart = 1
	    DO  i = 1, icrs
		iend   = icrloc (i) - 1
		lensub = iend - istart + 1
C
C*		If the substring has a length of 0, leave a blank line.
C
		IF  ( lensub .gt. 0 )  THEN
C
C*	    	    Check for software text.
C
		    IF  ( mtxhw .ne. 2 ) THEN
C
			ix = NINT ( x )
			iy = NINT ( y )
C
C*			Reset the X offset for the requested
C*			justification: left, right or centered.
C
			ij = MOD ( mjust, 10 )
			IF  ( ij .eq. 1 )  THEN
			    ixoff2 = ixoff
			  ELSE IF  ( ij .eq. 3 )  THEN
			    ixoff2 = ixoff - 2 * lensub + 2
			  ELSE
			    ixoff2 = ixoff - lensub + 1
			END IF
C
C*			Get the font number from the font code.
C
			IF  ( ifont .eq. 1 ) THEN
			    CALL ITEXT1 ( ix, iy, tchar (istart:iend),
     +					  ixoff2, iyoff2,
     +					  rotat, size, ier )
			  ELSE IF ( ifont .eq. 2 ) THEN
			    CALL ITEXT2 ( ix, iy, tchar (istart:iend),
     +					  ixoff2, iyoff2,
     +					  rotat, size, ier )
			  ELSE IF ( ifont .ge. 3 ) THEN
			    CALL ITEXT3 ( mtxfn, ix, iy,
     +					  tchar (istart:iend),
     +					  ixoff2, iyoff2,
     +					  rotat, size, ier )
			END IF
		      ELSE
C
C*	      		Must be hardware text.
C
			CALL HTEXT ( x, y, tchar (istart:iend),
     +				     lensub, ixoff, iyoff2,
     +				     rotat, ier )
		    END IF
		END IF
C
C*		Move to the next line.
C
		istart = iend + 2 
		iyoff2 = iyoff2 - 2
	    END DO 
 	END IF
C
C*	Restore line type and line width.
C
	IF  ( ( jltyp .ne. 0 ) .or. ( jlwid .ne. 0 ) )
     +	    CALL DSLINE ( jltyp, 0, jlwid, 0, i1, i2, i3, i4, ier )
C
C*      Restore active color
C
        CALL DSCOLR ( ksvcolr, imclr, iret )
C*
	RETURN
	END
