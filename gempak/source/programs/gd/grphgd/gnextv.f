	SUBROUTINE GNEXTV ( i, j, n, grid, hist, kx, ky,
     +		    	    fi, fj, val, iret )
C************************************************************************
C* GLASTV                                                               *
C*                                                                      *
C* This subroutine returns the next available value in a given          *
C* direction.                                                           *
C*                                                                      *
C* GLASTV ( I, J, N, GRID, HIST, KX, KY, FI, FJ, VAL, IRET )            *
C*                                                                      *
C* Input parameters:                                                    *
C*      I               INTEGER         I location on grid              *
C*      J               INTEGER         J location on grid              *
C*      N               INTEGER         Direction (1 thru 8)            *
C*      GRID (KX,KY)    REAL            Grid to calculate point values  *
C*      HIST (KX,KY)    REAL            Grid to keep history            *
C*      KX              INTEGER         Number of grid points in X      *
C*      KY              INTEGER         Number of grid points in Y      *
C*                                                                      *
C* Output parameters:                                                   *
C*      FI             REAL             I location of last value        *
C*      FJ             REAL             J location of last value        *
C*      VAL             REAL            Value                           *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP      9/98                                           *
C* D.W.Plummer/NCEP      8/05	Ensure fi anf fj are always returned	*
C************************************************************************
C
	INCLUDE		'GEMPRM.PRM'
C
	REAL	grid(kx,ky), hist(kx,ky)
C
	INTEGER	kinc(8,2)
C
	DATA	kinc/  1,  1,  1,  0, -1, -1, -1,  0,
     +                 1,  0, -1, -1, -1,  0,  1,  1/
C
	iret = 0
	fi  = i
	fj  = j
	val = RMISSD
C
	ii = i + kinc(n,1)
	jj = j + kinc(n,2)
	IF ( ii .gt. 0 .and. ii .lt. kx+1 .and.
     +	     jj .gt. 0 .and. jj .lt. ky+1 )  THEN
C
	    fi = ii
	    fj = jj
	    IF ( hist(ii,jj).eq.INIT .or. hist(ii,jj).eq.BOUNDED )  THEN
C
		RETURN
C
	    ELSE
C
		val = grid(ii,jj)
C
	    END IF
C
	    ii = ii + kinc(n,1)
	    jj = jj + kinc(n,2)
C
	END IF
C
	RETURN
	END
