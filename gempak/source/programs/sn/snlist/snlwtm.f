	SUBROUTINE SNLWTM  ( dattrp, nlevt, ihmt, datmxw, nlevw, ihmw,
     +			     above, luns, nlun, iret )
C************************************************************************
C* SNLWTM								*
C*									*
C* This subroutine lists tropopause and max wind level data for SNLIST.	*
C*									*
C* SNLWTM  ( DATTRP, NLEVT, IHMT, DATMXW, NLEVW, IHMW, ABOVE, LUNS,     *
C*	     NLUN, IRET )						*
C*									*
C* Input parameters:							*
C*	DATTRP(5,NLEVT)	REAL		Tropopause data			*
C*	NLEVT		INTEGER		Number of tropopause levels	*
C*	IHMT		INTEGER		Tropopause report time		*
C*	DATMXW(3,NLEVW)	REAL		Max wind data			*
C*	NLEVW		INTEGER		Number of max wind levels	*
C*	IHMW		INTEGER		Max wind report time		*
C*	ABOVE		LOGICAL		Above 100 mb flag		*
C*	LUNS (NLUN)	INTEGER		Logical unit numbers		*
C*	NLUN		INTEGER		Number of output devices	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/01						*
C************************************************************************
	REAL		dattrp (5,*), datmxw (3,*)
	LOGICAL		above
	INTEGER		luns (*)
C*
	CHARACTER	part*4
C------------------------------------------------------------------------
	iret = 0
C
C*	Do tropopause data.
C
	IF  ( nlevt .gt. 0 )  THEN
	    IF ( above ) THEN
		part = 'TRPC'
	      ELSE
		part = 'TRPA'
	    END IF
C
C*	    Loop through output devices.
C
	    DO  i = 1, nlun
C
C*	        Write data type.
C
	        WRITE  ( luns (i), 1000 ) part, ihmt
1000	        FORMAT ( / 4X, A, 2X, I5 )
C
C*	        Write parameter names.
C
	        WRITE  ( luns (i), 1001 )
1001		FORMAT ( 10X, 'PRES', 6X, 'TMPC', 6X, 'DWPC',
     +                   6X, 'DRCT', 6X, 'SPED' )
C
C*	        Write data.
C
	        WRITE (luns (i),1002) ((dattrp (j,k), j=1,5), k=1,nlevt)
1002            FORMAT ( 4X, 5F10.2 )
	    END DO
	END IF
C
C*	Do max wind data.
C
	IF  ( nlevw .gt. 0 )  THEN
	    IF ( above ) THEN
		part = 'MXWC'
	      ELSE
		part = 'MXWA'
	    END IF
C
C*	    Loop through output devices.
C
	    DO  i = 1, nlun
C
C*	        Write data type.
C
	        WRITE  ( luns (i), 1000 ) part, ihmw
C
C*	        Write parameter names.
C
	        WRITE  ( luns (i), 2001 )
2001		FORMAT ( 10X, 'PRES', 6X, 'DRCT', 6X, 'SPED' )
C
C*	        Write data.
C
	        WRITE (luns (i),2002) ((datmxw (j,k), j=1,3), k=1,nlevw)
2002            FORMAT ( 4X, 3F10.2 )
	    END DO
	END IF
C*
	RETURN
	END
