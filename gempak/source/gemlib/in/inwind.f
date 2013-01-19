	SUBROUTINE IN_WIND  ( wind, wintyp, winuni, iwnclr, iret )
C************************************************************************
C* IN_WIND								*
C*									*
C* This subroutine decodes the input for WIND.  The variable has	*
C* two parts separated by a slash.  The first part contains the		*
C* wind type (B for barb, A for arrow), the wind units (K for knots,	*
C* M for meters/sec, and the color number.  There should be no slashes  *
C* in this part.  The second part contains the size, width, type of the *
C* arrow or barb, and the arrowhead size separated by slashes.  The     *
C* arrow/barb size is a multiple of the base size.  Type 1 plots a      *
C* circle or an	arrowhead for calm winds.  Type 2 does not plot         *
C* anything for calm winds.  The arrowhead size is a multiple of the    *
C* base arrowhead size. 						*
C*									*
C* An example of the wind string is: BM/1.0/5/2				*
C*									*
C* IN_WIND  ( WIND, WINTYP, WINUNI, IWNCLR, IRET )			*
C*									*
C* Input parameters:							*
C*	WIND		CHAR*		Wind input			*
C*									*
C* Output parameters:							*
C*	WINTYP		CHAR*1		Wind type			*
C*					  B = wind barb 		*
C*					  A = wind arrow		*
C*	WINUNI		CHAR*		Wind units			*
C*					  K = knots 			*
C*					  M = meters/second 		*
C*	IWNCLR		INTEGER		Wind color 			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84	Original source IP_WIND			*
C* M. desJardins/GSFC	 4/85	Changed default to MKS; changed code.	*
C* I. Graffman/RDS	 5/86	Renamed IN_WIND				*
C* M. desJardins/GSFC	 6/88	Added size				*
C* S. Schotz/GSC	 1/90	Added width				*
C* M. desJardins/GSFC	 5/90	Fixed problem with width but no size	*
C* S. Schotz/GSC	 5/90	Added type				*
C* S. Schotz/GSC	 8/90	Added arrow head size			*
C* K. Brill/NMC		01/93	Return N as output in WINUNI		*
C* T. Piper/GSC		01/01	Removed N as output in WINUNI		*
C************************************************************************
	CHARACTER* (*)	wind, wintyp, winuni
C*
	CHARACTER	win*24, win2*24, w*1
	REAL		rwind (4)
C------------------------------------------------------------------------
	iret  = 0
C
C*	Check for size.
C
	ibreak = INDEX  ( wind, '/' )
	IF  ( ibreak .eq. 0 )  THEN
	    win    = wind
	    size   = 0.
	    iwidth = 0
   	    itype = 0
            sizehd = 0.
	  ELSE
	    win  = wind ( : ibreak-1 )
	    win2 = wind ( ibreak+1: )
	    CALL ST_RLST  ( win2, '/', 0., 4, rwind, n, ier )
	    size   = rwind (1)
	    iwidth = NINT ( rwind (2) )
	    itype  = NINT ( rwind (3) )
            sizehd = rwind (4)
	END IF
C
C*	Convert string to upper case.
C
	CALL ST_LCUC  ( win, win, ier )
	CALL ST_LSTR  ( win, len, ier )
C
C*	Read string from the end to the beginning.  This will pick up
C*	"B" if the user enters "BARB",...
C
	wintyp = 'B'
	winuni = 'M'
	iwnclr = 1
	i      = len
	DO WHILE  ( i .gt. 0 )
	    w = win ( i:i )
C
C*	    Check for "B", "A", "M" or "K".
C
	    IF  ( w .eq. 'B' ) THEN
		wintyp = 'B'
	      ELSE IF ( w .eq. 'A' ) THEN
		wintyp = 'A'
	      ELSE IF ( w .eq. 'M' ) THEN
		winuni = 'M'
	      ELSE IF ( w .eq. 'K' ) THEN
		winuni = 'K'
	    END IF
C
C*	    Check for number.
C
	    IF  ( ( w .ge. '0' ) .and. ( w .le. '9' ) ) THEN
		iend = i
		istart = i
		DO WHILE ( (win ( i-1:i-1 ) .ge. '0') .and. 
     +			   (win ( i-1:i-1 ) .le. '9') .and. (i .gt. 1) )
		    i = i - 1
		    istart = i
		END DO
		CALL ST_ILST (win (istart:iend), '/', 0, 1, iwnclr, 
     +                        n, ier)
	    END IF
	    i = i - 1
	END DO
C
C*	Now, set size, width, type, and/or arrow head size
C
	IF  ( ( size .gt. 0. ) .or. ( iwidth .ne. 0 ) .or. 
     +        ( itype .ne. 0 ) .or. ( sizehd. gt. 0 ) )  THEN
	    IF  ( wintyp .eq. 'B' )  THEN
		CALL GSBARB  ( size, iwidth, itype, ier )
	      ELSE
		CALL GSARRW  ( size, sizehd, iwidth, itype, ier )
	    END IF
	END IF
C*
	RETURN
	END
