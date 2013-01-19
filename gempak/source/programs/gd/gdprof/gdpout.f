	SUBROUTINE GDPOUT  ( havscl, havwnd, x, y, npts, u, v, yv,
     +                       nuv, ttlstr, parm, parmu, parmv, 
     +                       rlat, rlon, iscale, gvcord, output, iret )
C************************************************************************
C* GDPOUT								*
C*									*
C* This subroutine writes out profile data to terminal, file or		*
C* printer.								*
C*									*
C* GDPOUT  ( HAVSCL, HAVWND, X, Y, NPTS, U, V, YV, NUV, TTLSTR,		*
C*           PARM, PARMU, PARMV, RLAT, RLON, ISCALE, GVCORD, OUTPUT,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*      HAVSCL          LOGICAL         Flag for scalar in X		*
C*      HAVWND          LOGICAL         Flag for vector in U, V		*
C*      X (NPTS)        REAL            Array of scalar values		*
C*      Y (NPTS)        REAL            Y values for X			*
C*      NPTS            INTEGER         Number of X and Y values	*
C*      U   (NUV)       REAL            Array of u components		*
C*      V   (NUV)       REAL            Array of v components		*
C*      YV  (NUV)       REAL            Y values for U, V		*
C*      NUV             INTEGER         Number of U, V, YV values	*
C*      TTLSTR          CHAR* 		Title string			*
C*      PARM            CHAR*           X parm name			*
C*      PARMU           CHAR*           U parm name			*
C*      PARMV           CHAR*           V parm name			*
C*      RLAT            REAL		Latitude of profile		*
C*      RLON            REAL		Longitude of profile		*
C*      ISCALE          INTEGER         Power of 10 used for scaling	*
C*      GVCORD		CHAR*		Vertical coordinate		*
C*      OUTPUT          CHAR*           User input for OUTPUT		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/GSC          5/90  						*
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	output, ttlstr, parm, parmu, parmv, gvcord
	REAL		x (*), y (*), u (*), v (*), yv (*)
	LOGICAL         havscl, havwnd
C*
	INTEGER         lun ( MMFILE )
	CHARACTER*12       dev ( MMFILE )
C*
	LOGICAL         efrmt
	CHARACTER       cvc*4, cu*1, cv*1
	INCLUDE		'ERMISS.FNC'
	DATA    	cu, cv / 'U', 'V'/
C*
C------------------------------------------------------------------------
	iret = 0
C*
	CALL IN_OUTT ( output, 'gdprof', lun, nlun, dev, iret )
C*
	CALL ST_LCUC ( gvcord, cvc, ier )
C*
	DO i = 1, nlun
	  WRITE ( lun (i), 1000 ) 
1000	  FORMAT ( /////, 36X, 'PROFILE' )
	  CALL ST_LSTR ( ttlstr, len, ier )
	  WRITE ( lun (i), 2000 ) ttlstr (1:len)
2000	  FORMAT ( ///, 7X, A, // )
	  WRITE ( lun (i), 2500 ) rlat, rlon
2500	  FORMAT ( 7X, 'LATITUDE, LONGITUDE = ( ', F6.2, ', ', F7.2,
     +             ' )', //) 
	  IF ( havscl ) THEN
	    IF ( iscale .ne. 0 ) WRITE ( lun (i), 2700 ) iscale
2700        FORMAT ( 7X, 'Scale factor = 10 ** ', I3, // )
	    CALL ST_LSTR ( parm, len, ier )
	    WRITE ( lun (i), 3000) cvc, parm (1:len)
3000	    FORMAT ( //, 18X, A4, 5X, 2( 5X, A12 ) )
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
	  END IF
C*
	  IF ( havwnd ) THEN
	    CALL ST_LSTR ( parmu, len, ier )
	    CALL ST_LSTR ( parmv, lnv, ier )
	    WRITE ( lun (i), 3500 ) cvc, cu, parmu (1:len), cv,
     +                              parmv (1:lnv)
3500	    FORMAT ( //, 18X, A4, 5X,
     +               2 ( 3X, A1, 1X, A12 ) )
	    avg = .5 * ( ABS ( u (1) ) + ABS ( u ( nuv ) ) )
	    IF ( avg .lt. .001 .or. avg .gt. 99999999.99 ) THEN
	      efrmt = .true.
	    ELSE
	      efrmt = .false.
	    END IF
	    DO k = 1, nuv
	      IF ( efrmt ) THEN    
	        IF ( ERMISS ( u (k) ) ) THEN
                  WRITE ( lun (i), 4007) yv (k), u (k), v (k) 
	        ELSE
	          WRITE ( lun (i), 4000) yv (k), u (k), v (k) 
	        END IF
	      ELSE 
	        WRITE ( lun (i), 4007) yv (k), u (k), v (k) 
	      END IF
	    END DO
	  END IF
	END DO
	RETURN
	END
