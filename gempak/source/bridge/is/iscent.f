	SUBROUTINE IS_CENT ( report, lenr, clat, clon, iptr, iret )
C************************************************************************
C* IS_CENT 								*
C*									*
C* This subroutine decodes the center location of a phenomenon (e.g.,   *
C* tropical storm or hurricane) from an international sigmet report.    *
C*                                                                      *
C* IS_CENT ( REPORT, LENR, CLAT, CLON, IPTR, IRET )                     *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
C*									*
C* Output parameters:							*
C*	CLAT 		REAL	 	Latitude of center              *
C*	CLON 		REAL	 	Longitude of center             *
C*	IPTR		INTEGER		Pointer to location after flds  *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-10 = center not found          *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	 1/00	Added check for longitude 180 or 0      *
C* D. Kidwell/NCEP      10/01   Allow HlatHlon w/degrees or degrees/mins*
C* F. J. Yen/NCEP	 9/03	Fixed false longitude caused by "WINDS".*
C*			     	Fixed pointer value for HlatHlon format.*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
C*
	CHARACTER	center*15
	LOGICAL		done, newfmt
C------------------------------------------------------------------------
	iret = 0
	clat = RMISSD
	clon = RMISSD
	iptr = 0
C
C*	Find the first numeric character, or 'N' or 'S' followed by a
C*	digit.  This will be the beginning of the latitude.
C
	len    = MIN ( 26, lenr ) 
	done   = .false.
	iloc   = 1
	newfmt = .true.
	DO WHILE ( .not. done )
	    CALL ST_ALNM ( report ( iloc:iloc ), ityp, ier )
	    IF ( ityp .eq. 1 ) THEN
C
C*		This is the old format, with hemisphere indicator last.
C
		done   = .true.
		newfmt = .false.
	      ELSE IF ( ityp .eq. 2 ) THEN
		IF ( ( report ( iloc:iloc ) .eq. 'N' ) .or.
     +		     ( report ( iloc:iloc ) .eq. 'S' ) ) THEN
		    jloc = iloc + 1
		    IF ( jloc .le. len ) THEN
	    	        CALL ST_ALNM ( report ( jloc:jloc ), jtyp, ier )
C
C*			Check to see if this is the new format.
C
     			IF ( jtyp .eq. 1 )  done = .true.
		    END IF
		END IF
	    END IF
	    IF ( done ) THEN
		ibeg = iloc
	      ELSE
		iloc = iloc + 1
	        IF ( iloc .gt. len ) THEN
	            iret = -10
	            RETURN
	        END IF
	    END IF
	END DO
C
C*	Find 'E' or 'W' or the last digit after 'E' or 'W'.  This will 
C*	be the end of the longitude.
C
	iend = MIN ( ibeg + 21, lenr )
	ilon = INDEX ( report ( ibeg:iend ), 'W' )
C
C*	Possible false longitude detected in "WINDS", so use the closest.
C
	ilone = INDEX ( report ( ibeg:iend ), 'E' )
	IF ( ilon .ne. 0 ) THEN
	    IF ( ilone .ne. 0 ) ilon = MIN ( ilon, ilone )
	  ELSE
	    ilon = ilone
	END IF	
	IF ( ilon .eq. 0 ) ilon = ilone
	IF ( ( ilon .eq. 0 ) .and. .not. newfmt ) THEN
C
C*	    Allow missing 'E' or 'W' if old format and longitude is
C*	    180 or 0.
C
	    ilon = INDEX ( report ( ibeg:iend ), '180 ' )
	    IF ( ilon .gt. 0 ) THEN
		ilon = ilon  + 3
	      ELSE
		ilon1 = INDEX ( report ( ibeg:iend ), 'N0 ' )
		ilon2 = INDEX ( report ( ibeg:iend ), 'S0 ' )
		ilon = MAX ( ilon1, ilon2 )
		IF ( ilon .gt. 0 ) ilon = ilon + 2
	    END IF
	END IF
C
	IF ( ilon .gt. 0 ) THEN
C
C*	    Check for new format, with hemisphere indicator first.
C
	    IF ( newfmt ) THEN
		done = .false.
		iloc = ibeg + ilon
		DO WHILE ( .not. done ) 
	    	    CALL ST_ALNM ( report ( iloc:iloc ), ityp, ier )
	    	    IF ( ityp .ne. 1 ) THEN
			done = .true.
			ilon = iloc - 1 
		      ELSE
			iloc = iloc + 1
	        	IF ( iloc .gt. iend ) THEN
	            	    iret = -10
	            	    RETURN
	        	END IF
		    END IF
		END DO
		iend = ilon
	      ELSE
		iend = ibeg + ilon - 1
	    END IF
C
	    CALL ST_RMBL ( report ( ibeg:iend ), center, len, ier )
C
C*	    Get the center point.
C	
	    CALL IS_LTLN ( center, clat, clon, ier )
	    IF ( ier .lt. 0 ) iret = -10
	END IF
C
	IF ( iret .eq. 0 ) THEN
	    iptr = iend + 1
	END IF
C*
	RETURN
	END
