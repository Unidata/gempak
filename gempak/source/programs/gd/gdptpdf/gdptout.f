	SUBROUTINE GDPTOUT  ( x, y, npts, ttlstr, 
     +                       rlat, rlon, iscale, output, iret )
C************************************************************************
C* GDPTOUT								*
C*									*
C* This subroutine writes out probability data to terminal, file or	*
C* printer.								*
C*									*
C* GDPTOUT  ( X, Y, NPTS, TTLSTR RLAT, RLON, ISCALE, 			*
C*            OUTPUT,	IRET )						*
C*									*
C* Input parameters:							*
C*      X (NPTS)        REAL            Array of scalar values		*
C*      Y (NPTS)        REAL            Y values for X			*
C*      NPTS            INTEGER         Number of X and Y values	*
C*      TTLSTR          CHAR* 		Title string			*
C*      RLAT            REAL		Latitude of profile		*
C*      RLON            REAL		Longitude of profile		*
C*      ISCALE          INTEGER         Power of 10 used for scaling	*
C*      OUTPUT          CHAR*           User input for OUTPUT		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* M. Li/SAIC           08/07  						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	output, ttlstr
	REAL		x (*), y (*)
C*
	INTEGER         lun ( MMFILE )
	CHARACTER*12    dev ( MMFILE )
C*
	LOGICAL         efrmt
	INCLUDE		'ERMISS.FNC'
C*
C------------------------------------------------------------------------
	iret = 0
C*
	CALL IN_OUTT ( output, 'gdptpdf', lun, nlun, dev, iret )
C*
	DO i = 1, nlun
	  WRITE ( lun (i), 1000 ) 
1000	  FORMAT ( /////, 36X, 'PROBABILITY' )
	  CALL ST_LSTR ( ttlstr, len, ier )
	  WRITE ( lun (i), 2000 ) ttlstr (1:len)
2000	  FORMAT ( ///, 7X, A, // )
	  WRITE ( lun (i), 2500 ) rlat, rlon
2500	  FORMAT ( 7X, 'LATITUDE, LONGITUDE = ( ', F6.2, ', ', F7.2,
     +             ' )', //) 
	    IF ( iscale .ne. 0 ) WRITE ( lun (i), 2700 ) iscale
2700        FORMAT ( 7X, 'Scale factor = 10 ** ', I3, // )
	    WRITE ( lun (i), 3000) 'FUNCTION VALUE', 'PROBABILITY' 
3000	    FORMAT ( A23, A21  )
	    avg = .5 * ( ABS ( x (1) ) + ABS ( x ( npts ) ) )
	    IF ( avg .lt. .001 .or. avg .gt. 99999999.99 ) THEN
	      efrmt = .true.
	    ELSE
	      efrmt = .false.
	    END IF
	    DO k = 1, npts
	      IF ( efrmt ) THEN    
	        IF ( ERMISS ( x (k) ) ) THEN
                  WRITE ( lun (i), 4007) y (k), x (k) 
	        ELSE
	          WRITE ( lun (i), 4000) y (k), x (k) 
	        END IF
	      ELSE 
	        WRITE ( lun (i), 4007) y (k), x (k) 
	      END IF
4000	      FORMAT ( 13X, F10.2, 4X, 2 ( 5X, 1PE12.5 ) )
4007          FORMAT ( 13X, F10.2, 4X, 2 ( 5X, F12.3 ) )
	    END DO
C*
	END DO
	RETURN
	END
