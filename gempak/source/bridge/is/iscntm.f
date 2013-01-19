	SUBROUTINE IS_CNTM ( timstr, iotarr, strtim, iret )
C************************************************************************
C* IS_CNTM 								*
C*									*
C* This subroutine decodes the cancellation time string.		*
C*                                                                      *
C* IS_CNTM ( TIMSTR, IOTARR, STRTIM, IRET )				*
C*									*
C* Input parameters:							*
C*	TIMSTR		CHAR*		Bulletin time string		*
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*									*
C* Output parameters:							*
C*	STRTIM		CHAR*		GEMPAK time			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = format error		*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 2/02	Created from part of IS_PRMB.		*
C************************************************************************
C*
	CHARACTER*(*)	timstr, strtim
	INTEGER  	iotarr (*)
C*
	INTEGER  	itarr (5)
C------------------------------------------------------------------------
	iret  = 0
	CALL ST_LSTR ( timstr, lens, ier ) 
	IF ( ( lens .eq. 5 ) .or. ( lens .eq. 7 ) ) THEN

	    IF ( timstr ( lens:lens ) .eq. 'Z' .or.
     +		    timstr ( lens:lens ) .eq. '=' .or.
     +		    timstr ( lens:lens ) .eq. '.' ) 
     +			lens = lens - 1
	  ELSE IF ( lens .eq. 8 ) THEN
	    IF ( timstr ( lens - 1:lens ) .eq. 'Z.' )
     +			lens = lens - 2
	END IF
C
	IF ( ( lens .eq. 4 ) .or. ( lens .eq. 6 ) ) THEN
	    DO i = 1, 3
	        itarr ( i ) = iotarr ( i )
	    END DO
	    CALL ST_INTG ( timstr ( lens - 3:lens ),
     +			   ihhmm, ier )
	    IF ( ier .lt. 0 ) THEN
		iret = -2
		RETURN
	    END IF
C
	    itarr ( 4 ) = ihhmm / 100
	    itarr ( 5 ) = MOD ( ihhmm, 100 )
	    CALL TI_MDIF ( itarr, iotarr, nmin, ier )
	    IF ( nmin .lt. ( -10 ) ) THEN
		CALL TI_ADDD ( itarr, itarr, ier )
	      ELSE IF ( nmin .gt. 1380 ) THEN
		CALL TI_SUBD ( itarr, itarr, ier )
	    END IF
	    IF ( ier .lt. 0 ) THEN
		iret = -2
		RETURN
	    END IF
C
	    CALL TI_ITOC ( itarr, strtim, ier )
	    IF ( ier .lt. 0 ) THEN
		iret = -2
		RETURN
	    END IF
	  ELSE
	    iret = -2
	    RETURN
	END IF
C*
	RETURN
	END
