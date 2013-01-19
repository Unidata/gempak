	SUBROUTINE ND_GSSG ( igxold, ksbx, ksby, iskip, grid, 
     +							igx, igy, iret )
C************************************************************************
C* ND_GSSG								*
C*									*
C* This subroutine subsets and applies a skip factor to a grid using 	*
C* the subset coordinates in KSBX and KSBY and the skip factor in ISKIP.*
C* The new grid dimensions are returned in IGX and IGY.			*
C*									*
C* ND_GSSG ( IGXOLD, KSBX, KSBY, ISKIP, GRID, IGX, IGY, IRET )		*
C*									*
C* Input parameters:							*
C*	IGXOLD  	INTEGER		Maximum grid size    		*
C*	KSBX (2)	INTEGER		Subset X coordinates		*
C*	KSBY (2)	INTEGER		Subset Y coordinates		*
C*	ISKIP		INTEGER		Skip factor ( 0 = no skip )	*
C*									*
C* Input and output parameters:						*
C*	GRID (*)	REAL		Grid of data			*
C*	IGX		INTEGER		New X grid dimension		*
C*									*
C* Output parameters:							*
C*	IGY		INTEGER		New Y grid dimension		*
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal			*
C*									*
C**									*
C* Log:									*
C* T. Piper/SAIC	10/02	Modified from NAGSSG for GRIB2		*
C* T. Piper/SAIC	04/03	Added skip factor			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid(*)
	INTEGER		ksbx(2), ksby(2)
C*
C------------------------------------------------------------------------
	io = 0
	iret = 0
	jskip = iskip + 1
	DO j = ksby(1), ksby(2), jskip
	    DO i = ksbx(1), ksbx(2), jskip
		k = i
		IF ( k .gt. igxold ) k = k - igxold
		indx = ( j - 1 ) * igx + k
		io = io + 1
		IF ( grid(indx) .eq. 9999.0 ) THEN
		    grid(io) = RMISSD
		ELSE
		    grid(io) = grid(indx)
		END IF
	    END DO
	END DO
        igx = ((ksbx(2) - ksbx(1))/jskip) + 1
        igy = ((ksby(2) - ksby(1))/jskip) + 1
C*
	RETURN
	END
