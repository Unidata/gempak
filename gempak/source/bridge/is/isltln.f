	SUBROUTINE IS_LTLN ( string, xlat, xlon, iret )
C************************************************************************
C* IS_LTLN 								*
C*									*
C* This subroutine decodes a latitude/longitude string of the form      *
C* latHlonH, where H is a hemisphere indicator (N, S, E, W) and lat and *
C* lon are the values in degrees and (optionally) tenths of a degree or *
C* minutes.  If tenths of a degree are specified, they are preceded by  *
C* a decimal point.  The format HlatHlon is also allowed, with degrees  *
C* and (optionally) minutes.  Tenths of a degree are not allowed for	*
C* this format.                                                         *
C*                                                                      *
C* IS_LTLN ( STRING, XLAT, XLON, IRET )                                 *
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Latitude/longitude string       *
C*									*
C* Output parameters:							*
C*	XLAT		REAL  		Signed latitude value           *
C*	XLON		REAL   		Signed longitude value          *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = invalid format            *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	11/99	Added check for valid substring length  *
C* D. Kidwell/NCEP	11/99	Allowed degrees and minutes             *
C* D. Kidwell/NCEP	 1/00	Checked length, checked for long. 180, 0*
C* D. Kidwell/NCEP	 3/00	Refined length checks                   *
C* D. Kidwell/NCEP	10/01	Allow HlatHlon w/degrees or degrees/mins*
C* F. J. Yen/NCEP	 6/02	Corrected spelling in prologue.	 Checked*
C*				for '.' at end of string.		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string
C*
	LOGICAL		newfmt
C*
	CVTMIN (ddmm) = INT ( ddmm/100. ) + AMOD ( ddmm, 100. ) / 60.
C------------------------------------------------------------------------
	iret = 0
	xlat = RMISSD
	xlon = RMISSD
C
	CALL ST_LSTR ( string, lens, ier )
C
C*	Ignore false decimal points at end of string
C 
	IF ( string ( lens:lens ) .eq. '.' ) lens = lens - 1
	IF ( ( lens .lt. 3 ) .or. ( lens .gt. 11 ) ) THEN
	    iret = -5
	    RETURN
	END IF
C
C*	Determine N or S hemisphere.
C
	in = INDEX ( string ( :lens ), 'N' )
	IF ( in .ne. 0 ) THEN
	    ilat = 1
	    is   = in
	  ELSE
	    is = INDEX ( string ( :lens ) , 'S' )
	    IF ( is .ne. 0 ) THEN 
		ilat = -1
	      ELSE
		iret = -5
	    END IF
	END IF
C
C*	Determine old or new format.
C
	IF ( is .eq. 1 ) THEN
	    newfmt = .true.
	  ELSE
	    newfmt = .false.
	END IF
C
C*	Determine W or E hemisphere.
C
	iw = INDEX ( string ( :lens ), 'W' )
	IF ( iw .ne. 0 ) THEN
	    ilon = -1
	    ie   = iw
	  ELSE
	    ie = INDEX ( string ( :lens ) , 'E' )
	    IF ( ie .ne. 0 ) THEN 
		ilon = 1
	      ELSE IF ( .not. newfmt ) THEN
		IF ( ( string ( lens - 2:lens ) .eq. '180' ) .or.
     +		     ( ( string ( lens:lens ) .eq. '0' ) .and.
     +		       ( lens .eq. ( is + 1 ) ) ) )  THEN
		    ilon = 1
		    ie   = lens + 1
	          ELSE
		    iret = -5
		END IF
	      ELSE
		iret = -5
	    END IF
	END IF
C
	IF ( ie .lt. ( is + 2 ) ) iret = -5
	IF ( iret .lt. 0 ) RETURN
C
C*	Get the numeric values.
C
	IF ( .not. newfmt ) THEN
	    CALL ST_CRNM ( string ( :is - 1 ), xlat, ier1 )
	    CALL ST_CRNM ( string ( is + 1:ie - 1 ), xlon, ier2 )
	    IF ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) ) THEN
	        IF ( INDEX ( string ( :is - 1 ), '.' ) .eq. 0 ) THEN
		    IF ( is .ge. 4 ) xlat = CVTMIN ( xlat )
	        END IF
	        IF ( INDEX ( string ( is+1:ie-1 ), '.' ) .eq. 0 ) THEN
		    IF ( ( ( ie-is ) .ge. 5 ) .or. ( xlon .gt. 180. ) ) 
     +		         xlon = CVTMIN ( xlon )
	        END IF
	      ELSE
	        iret = -5
	    END IF
	  ELSE IF ( lens .ge. 6 ) THEN
	    CALL ST_CRNM ( string ( 2:ie - 1 ) , xlat, ier1 )
	    CALL ST_CRNM ( string ( ie + 1:lens), xlon, ier2 )
	    IF ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) ) THEN	
	        IF ( lens .ge. 10 ) THEN
		    xlat = CVTMIN ( xlat )
		    xlon = CVTMIN ( xlon )
		  ELSE IF ( lens .ge. 8 ) THEN
		    IF ( ie .eq. 6 ) THEN
		        xlat = CVTMIN ( xlat )
		      ELSE IF ( ie .eq. 4 ) THEN
		        xlon = CVTMIN ( xlon )
		      ELSE
			iret = -5
		    END IF
		END IF
	      ELSE
		iret = -5
	    END IF
	  ELSE
	    iret = -5
	END IF
C
	IF ( iret .eq. 0 ) THEN
	    xlat = ilat * xlat
	    xlon = ilon * xlon
	END IF
C*
	RETURN
	END
