	SUBROUTINE IS_LORL ( string, xlat, xlon, iret )
C************************************************************************
C* IS_LORL 								*
C*									*
C* This subroutine decodes a latitude or a longitude string of the form *
C* latlonH, where H is a hemisphere indicator (N, S, E, W) and latlon	*
C* is the value in degrees and (optionally) tenths of a degree or	*
C* minutes.  If tenths of a degree are specified, they are preceded by  *
C* a decimal point.  The format Hlatlon is also allowed, with degrees	*
C* and (optionally) minutes.  Tenths of a degree are not allowed for	*
C* this format.                                                         *
C*                                                                      *
C* IS_LORL ( STRING, XLAT, XLON, IRET )                                 *
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Latitude or longitude string    *
C*									*
C* Output parameters:							*
C*	XLAT		REAL  		Signed latitude value           *
C*	XLON		REAL   		Signed longitude value          *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = invalid format            *
C*									*
C**									*
C* F. J. Yen/NCEP	 6/02	Created by modification of IS_LTLN.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string
C*
	LOGICAL		newfmt, latf, lonf
C*
	CVTMIN (ddmm) = INT ( ddmm/100. ) + AMOD ( ddmm, 100. ) / 60.
C------------------------------------------------------------------------
	iret = 0
	xlat = RMISSD
	xlon = RMISSD
C
	CALL ST_LSTR ( string, lens, ier )
C
C*      Ignore false decimal points at end of string
C
        IF ( string ( lens:lens ) .eq. '.' ) lens = lens - 1
	IF ( ( lens .lt. 2 ) .or. ( lens .gt. 6 ) ) THEN
	    iret = -5
	    RETURN
	END IF
C
C*	Determine if N or S hemisphere.
C
	latf = .false.
	lonf = .false.
	ins = INDEX ( string ( :lens ), 'N' )
	IF ( ins .ne. 0 ) THEN
	    ilat = 1
	    latf = .true.
	  ELSE
	    ins = INDEX ( string ( :lens ) , 'S' )
	    IF ( ins .ne. 0 ) THEN 
		ilat = -1
	        latf = .true.
	    END IF
	END IF
	IF ( latf ) THEN
C
C*	    Determine old or new format.
C
	    IF ( ins .eq. 1 ) THEN
	        newfmt = .true.
	      ELSE
	        newfmt = .false.
	    END IF
	  ELSE
C
C*	    Not N or S hemisphere, so determine if W or E hemisphere.
C
	    iwe = INDEX ( string ( :lens ), 'W' )
	    IF ( iwe .ne. 0 ) THEN
	        ilon = -1
		lonf = .true.
	      ELSE
	        iwe = INDEX ( string ( :lens ) , 'E' )

	        IF ( iwe .ne. 0 ) THEN 
		    ilon = 1
		    lonf = .true.
	          ELSE
		    iret = -5
		    RETURN
	        END IF
	    END IF
C
C*	    Determine old or new format.
C
	    IF ( iwe .eq. 1 ) THEN
	        newfmt = .true.
	      ELSE
	        newfmt = .false.
	    END IF
	END IF
C
C*	Get the numeric value.
C
	ier1 = 0
	ier2 = 0
	IF ( .not. newfmt ) THEN
	    IF ( latf ) THEN
 		CALL ST_CRNM ( string ( :ins - 1 ), xlat, ier1 )
	        IF ( ier1 .eq. 0 .and.
     +			  INDEX ( string ( :ins-1 ), '.' ) .eq. 0 ) THEN
		    IF ( ins .ge. 4 ) xlat = CVTMIN ( xlat )
	        END IF
	      ELSE IF ( lonf ) THEN
		CALL ST_CRNM ( string ( :iwe - 1 ), xlon, ier2 )
	        IF ( ier2 .eq. 0 .and.
     +		          INDEX ( string ( :iwe-1 ), '.' ) .eq. 0 ) THEN
		    IF ( ( iwe .ge. 5 ) .or. ( xlon .gt. 180. ) ) 
     +		              xlon = CVTMIN ( xlon )
	        END IF
	    END IF
	    IF ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) ) THEN
	        iret = -5
	    END IF
	  ELSE IF ( lens .ge. 5 ) THEN
	    IF ( latf ) THEN
      		CALL ST_CRNM ( string ( 2:lens ) , xlat, ier1 )
	      ELSE IF ( lonf ) THEN
      		CALL ST_CRNM ( string ( 2:lens), xlon, ier2 )
	      ELSE
		iret = -5
	    END IF
	    IF ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) ) THEN	
	        IF ( lens .ge. 5 .and. xlon .ne. RMISSD ) THEN
		    xlon = CVTMIN ( xlon )
		  ELSE IF ( lens .ge. 4 .and. xlat .ne. RMISSD ) THEN
		    xlat = CVTMIN ( xlat )
		  ELSE
		    iret = -5
		END IF
	      ELSE
		iret = -5
	    END IF
	  ELSE
	    iret = -5
	END IF
C
	IF ( iret .eq. 0 ) THEN
	    IF ( xlat .ne. RMISSD ) xlat = ilat * xlat
	    IF ( xlon .ne. RMISSD ) xlon = ilon * xlon
	END IF
C*
	RETURN
	END
