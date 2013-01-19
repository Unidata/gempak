	SUBROUTINE SNLWSG  ( datsgt, nlevt, ihmt, datsgw, nlevw, ihmw,
     +			     zwind, above, luns, nlun, iret )
C************************************************************************
C* SNLWSG								*
C*									*
C* This subroutine lists significant level data for SNLIST.		*
C*									*
C* SNLWSG  ( DATSGT, NLEVT, IHMT, DATSGW, NLEVW, IHMW, ZWIND,		*
C*           ABOVE, LUNS, NLUN, IRET )					*
C*									*
C* Input parameters:							*
C*	DATSGT(3,NLEVT)	REAL		Significant temperature data	*
C*	NLEVT		INTEGER		Number of temp levels		*
C*	IHMT		INTEGER		Temp report time		*
C*	DATSGW(3,NLEVW)	REAL		Significant wind data		*
C*	NLEVW		INTEGER		Number of wind levels		*
C*	IHMW		INTEGER		Wind report time		*
C*	ZWIND		LOGICAL		Wind type flag			*
C*	ABOVE		LOGICAL		Above 100 mb flag		*
C*	LUNS (NLUN)	INTEGER		Logical unit numbers		*
C*	NLUN		INTEGER		Number of output devices	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89						*
C* M. desJardins/GSFC	11/89	Added changes for station time		*
C* D. Kidwell/NCEP	 2/01	Removed unused arg. dattim; cleaned up  *
C************************************************************************
	REAL		datsgw (3,*), datsgt (3,*)
	LOGICAL		above, zwind
	INTEGER		luns (*)
C*
	CHARACTER	part*4
C------------------------------------------------------------------------
	iret = 0
C
C*	Do significant temperature data first.
C
	IF  ( nlevt .gt. 0 )  THEN
	    IF  ( above )  THEN
		part = 'TTDD'
	      ELSE
		part = 'TTBB'
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
1001	        FORMAT ( 10X, 'PRES', 6X, 'TMPC', 6X, 'DWPC' )
C
C*	        Write data.
C
	        WRITE (luns (i),1002) ((datsgt (j,k), j=1,3), k=1,nlevt)
1002	        FORMAT ( 4X, 3F10.2 )
	    END DO
	END IF
C
C*	Do significant wind data.
C
	IF  ( nlevw .gt. 0 )  THEN
	    IF  ( above )  THEN
		part = 'PPDD'
	      ELSE
		part = 'PPBB'
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
		IF  ( zwind )  THEN
	            WRITE  ( luns (i), 3001 )
3001		    FORMAT ( 10X, 'HGHT', 6X, 'DRCT', 6X, 'SPED' )
		  ELSE
	            WRITE  ( luns (i), 2001 )
2001		    FORMAT ( 10X, 'PRES', 6X, 'DRCT', 6X, 'SPED' )
		END IF
C
C*	        Write data.
C
	        WRITE (luns (i),1002) ((datsgw (j,k), j=1,3), k=1,nlevw)
	    END DO
	END IF
C*
	RETURN
	END
