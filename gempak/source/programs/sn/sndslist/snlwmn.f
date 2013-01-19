	SUBROUTINE SNLWMN  ( datman, nlev, ihm, datmnw, nlevw, ihmw,
     +			     above, luns, nlun, iret )
C************************************************************************
C* SNLWMN								*
C*									*
C* This subroutine lists mandatory data for SNLIST.			*
C*									*
C* SNLWMN  ( DATMAN, NLEV, IHM, DATMNW, NLEVW, IHMW, ABOVE, LUNS, NLUN, *
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	DATMAN (6,NLEV)	REAL		Mandatory data (TT)             *
C*	NLEV		INTEGER		Number of levels		*
C*	IHM		INTEGER		Report time			*
C*	DATMNW (3,NLEV)	REAL		Mandatory wind data (PP)	*
C*	NLEVW		INTEGER		Number of wind levels		*
C*	IHMW		INTEGER		Wind report time		*
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
C* M. desJardins/GSFC	11/89	Changes for station time		*
C* D. Kidwell/NCEP	 2/01	Added parts PPAA and PPCC               *
C************************************************************************
	REAL		datman (6,*), datmnw (3,*)
	INTEGER		luns (*)
	LOGICAL		above
C*
	CHARACTER	part*4
C------------------------------------------------------------------------
	iret = 0
C
C*	Do mandatory temperature and wind data (TT) first.
C
	IF ( nlev .gt. 0 ) THEN
	    IF  ( above )  THEN
	        part = 'TTCC'
	      ELSE
	        part = 'TTAA'
	    END IF
C
C*	    Loop through output devices.
C
	    DO  i = 1, nlun
C
C*	        Write data type.
C
	        WRITE  ( luns (i), 1000 ) part, ihm
1000	        FORMAT ( / 4X, A, 2X, I5 )
C
C*	        Write parameter names.
C
	        WRITE  ( luns (i), 1001 )
1001	        FORMAT ( 10X, 'PRES', 6X, 'TMPC', 6X, 'DWPC', 6X,
     +                        'DRCT', 6X, 'SPED', 6X, 'HGHT' )
C
C*	        Write data.
C
	        WRITE  (luns (i),1002) ((datman (j,k), j=1,6), k=1,nlev)
1002	        FORMAT ( 4X, 6F10.2 )
	    END DO
	END IF
C
C*	Do mandatory wind only data (PP).
C
	IF ( nlevw .gt. 0 ) THEN
	    IF  ( above )  THEN
	        part = 'PPCC'
	      ELSE
	        part = 'PPAA'
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
2001	        FORMAT ( 10X, 'PRES', 6X, 'DRCT', 6X, 'SPED' )
C
C*	        Write data.
C
	        WRITE (luns (i),2002) ((datmnw (j,k), j=1,3), k=1,nlevw)
2002	        FORMAT ( 4X, 3F10.2 )
	    END DO
	END IF
C*
	RETURN
	END
