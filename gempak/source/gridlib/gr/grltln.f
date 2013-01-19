	SUBROUTINE GR_LTLN  ( kx, ky, rlat, rlon, iret )
C************************************************************************
C* GR_LTLN								*
C*									*
C* This subroutine computes the latitude and longitude at each grid 	*
C* point.  The grid must be defined in GEMPLT before this subroutine	*
C* is called.								*
C*									*
C* GR_LTLN  ( KX, KY, RLAT, RLON, IRET )				*
C*									*
C* Input parameters:							*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*									*
C* Output parameters:							*
C*	RLAT (KX,KY)	REAL		Latitudes in degrees		*
C*	RLON (KX,KY)	REAL		Longitudes in degrees		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = grid projection error	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	From DG_LTLN				*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		rlat (*), rlon (*)
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Fill x, y arrays with grid point values. 
C
	k = 0
	DO  i = 1, ky
	    fi =  FLOAT (i)
	    DO  j = 1, kx
	        k = k + 1
	        rlon (k) = fi
	    END DO
	END DO
C
	DO  j = 1, kx
	    fj = FLOAT (j)
	    k  = j
	    DO  i = 1, ky
	        rlat (k) = fj
	        k = k + kx
	    END DO
	END DO
C
C*	Transform the points.
C
	kxy = kx * ky
	CALL GTRANS  ( 'G', 'M', kxy, rlat, rlon, rlat, rlon, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
	    iret = -6
	    RETURN
	END IF
C*
	RETURN
	END
