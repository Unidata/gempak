	SUBROUTINE DTEXTC ( iwndw, x, y, cchar, lens, rotat,
     +			    ixoff, iyoff, iret )
C************************************************************************
C* DTEXTC								*
C*									*
C* This subroutine plots a text string in any coordinate system.  Text  *
C* may be plotted in the margins, that is no clipping is done. The text *
C* string may be rotated from horizontal at the reference point and     *
C* offset along the rotated X and Y coordinates.  Positive X offsets    *
C* are toward the right;  positive Y offsets are toward the top.  The   *
C* text is drawn using attributes defined by GSTEXT.                    *
C*									*
C* The text string may contain carriage returns and/or line feed        *
C* characters.  A carriage return/line feed will terminate one line of  *
C* text and begin a new line.  Each new line will be offset normal to   *
C* the rotation from the previous line and oriented along the direction *
C* of rotation.  All resulting lines of text will be placed so that     *
C* they are centered normal to the rotation.                            *
C*									*
C* DTEXTC ( IWNDW, X, Y, CCHAR, LENS, ROTAT, IXOFF, IYOFF, IRET )	*
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
C* A. Chang/EAI		 8/94	Modified from DTEXT			*
C* M. Linda/GSC		 1/97	Changed X and Y to reals		*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* E. Safford/GSC	 4/97	Modified to handle multiple lines	*
C* E. Safford/GSC	 5/97 	Cleaned up and added more documentation *
C* S. Jacobs/NCEP	 6/97	Changed y-offset calc for multi lines	*
C* S. Jacobs/NCEP	 7/97	Added software fonts			*
C* S. Jacobs/NCEP	 7/98	Removed ITYPE from calling sequence	*
C* S. Jacobs/NCEP	 7/98	Made consistent with DTEXT		*
C* S. Jacobs/NCEP	 7/98	Changed ttxsz to txsize			*
C* B. Yin/SAIC           3/04   Added ability to draw degree sign       *
C* B. Yin/SAIC           3/04   Modified LENS to local variabe ilens  	*
C* B. Yin/SAIC           3/04   Modified to display degree sign for HP  *
C* B. Yin/SAIC           3/04   Removed the code for HP XW              *
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
	CHARACTER       tmpchr*400
C------------------------------------------------------------------------
	iret  = NORMAL
	ilens = lens
C
C*	If the driver is VG, send the points directly to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HTEXTC ( x, y, cchar, ilens, ixoff, iyoff, rotat, iret )
	    RETURN
	END IF

C
C*      Check escape sequence
C*      Backslash followed by three number are special characters
C*      Keep one backslash if there are two
C*      For device RBK, skip backslash followed by three numbers
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
     +                           + ( icode - icode/100*100 ) / 10 * 8
     +                           + ( icode - icode / 10 * 10 ) )
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
C*	    Find all carriage returns and line feeds in the string.
C
	    icrs = 0
	    DO  i = 1, ilens
		IF  ( ( cchar(i:i) .eq. CHCR ) .or.
     +		      ( cchar(i:i) .eq. CHLF ) )  THEN
		    icrs = icrs + 1
		    icrloc(icrs) = i
		END IF
	    END DO
C
C*	    Loop over all carriage returns adding one for the end
C*	    of the string.
C
	    icrs         = icrs + 1
	    icrloc(icrs) = ilens + 1
	    iyoff2       = iyoff + icrs - 1
	    istart       = 1
	    DO  i = 1, icrs
		iend   = icrloc(i) - 1
		lensub = iend - istart + 1
C
C*		If the substring has a length of 0, leave a blank line.
C
		IF  ( lensub .gt. 0 )  THEN
C
C*		    Check for software text.
C
		    IF  ( mtxhw .ne. 2 )  THEN
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
			IF  ( ifont .eq. 1 )  THEN
			    CALL ITEXT1 ( ix, iy, cchar(istart:iend),
     +					  ixoff2, iyoff2,
     +					  rotat, size, ier )
			  ELSE IF  ( ifont .eq. 2 )  THEN
			    CALL ITEXT2 ( ix, iy, cchar(istart:iend),
     +					  ixoff2, iyoff2,
     +					  rotat, size, ier )
			  ELSE IF  ( ifont .ge. 3 )  THEN
			    CALL ITEXT3 ( mtxfn, ix, iy,
     +					  cchar(istart:iend),
     +					  ixoff2, iyoff2,
     +					  rotat, size, ier )
			END IF
		      ELSE
			CALL HTEXTC ( x, y, cchar(istart:iend),
     +				      lensub, ixoff, iyoff2,
     +				      rotat, ier )
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
C*
	RETURN
	END
