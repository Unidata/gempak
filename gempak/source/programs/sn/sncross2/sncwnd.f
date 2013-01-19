	SUBROUTINE SNCWND  ( wind, wintyp, winuni, iwnclr, nwc, iret )
C************************************************************************
C* IN_WIND								*
C*									*
C* This subroutine decodes the input for WIND.  The variable has	*
C* two parts separated by a slash.  The first part contains the		*
C* wind type (B for barb, A for arrow), the wind units (K for knots,	*
C* M for meters/sec, N for meters/sec with no reference arrow) and	*
C* the color number.  There should be no slashes in this part.  The	*
C* second part contains the size, width, type of the arrow or barb,	*
C* and the arrowhead size separated by slashes.  The arrow/barb size	*
C* is a multiple of the base size.  Type 1 plots a circle or an		*
C* arrowhead for calm winds.  Type 2 does not plot anything for calm	*
C* winds.  The arrowhead size is a multiple of the base arrowhead size. *
C*									*
C* An example of the wind string is: BM/1.0/5/2				*
C*									*
C* SNCWND  ( WIND, WINTYP, WINUNI, IWNCLR, NWC, IRET )			*
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
C*					  N = m/s, no ref. arrow	*
C*	IWNCLR		INTEGER(*)	Wind color array		*
C*      NWC             INTEGER         number of colors in array
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
C* P. Neilley/NCAR      11/94   Adapted from IN_WIND for color array    *
C************************************************************************
	CHARACTER* (*)	wind, wintyp, winuni
        integer         iwnclr(40)
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
	iwnclr(1) = 1
        nwc = 1

C Find the first number in wind string as indicator of start of
C color specification

        i = 1
        DO WHILE ( (i.le.len) .and. 
     +             ( (win(i:i).lt.'0') .or. (win(i:i).gt.'9') ) )
           i = i + 1
        END DO
        nwc = 40
        CALL IN_COLR( win(i:len), nwc, iwnclr, iret)

C Get wind symbol types and units

	i      = i - 1
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
	      ELSE IF ( w .eq. 'N' ) THEN
		winuni = 'N'
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
