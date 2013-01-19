	SUBROUTINE GDTXTI ( gridin, nx, nv, xpts, xptso, nptso, grdout,
     +	                    iret )
C************************************************************************
C* GDTXTI								*
C*									*
C* This subroutine horizontally interpolates to an even grid		*
C*									*
C* GDTXTI ( GRIDIN, NX, NV, XPTS, XPTSO, NPTSO, GRDOUT, IRET )		*
C*									*
C* Input parameters:							*
C*	GRIDIN (NX, NV) REAL	Input cross section array		*
C*      NX		INTEGER	Number of points in horz		*
C*      NV		INTEGER	Number of points in vert		*
C*	XPTS(*)		REAL	X-positions of input data		*
C*	XPTSO(*)	REAL	X-positions of output data		*
C*	NPTSO		INTEGER	number of output x-positions		*
C*									*
C* Output parameters:							*
C*	GRDOUT(NPTSO,NV)REAL	Output data				*
C*	IRET		INTEGER	Return code				*
C*					  0 = normal return		*
C*					 -7 = invalid vert coord	*
C**									*
C* Log:									*
C* K. F. Brill/GSC       7/89  						*
C* K. Brill/GSC          8/89           Interpolate to exact levels	*
C* K. Brill/GSC          1/90           Set lower/upper lvls for int	*
C* K. Brill/NMC		 1/91		Check for equality between 	*
C*        				input and output levels		*
C* K. Brill/NMC		11/92		Remove print *,			*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
        REAL 		gridin (nx, nv), grdout (*), xpts (*), xptso (*)
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C*
        istp = nptso * nv
	DO i = 1, istp
	    grdout ( i ) = RMISSD
	END DO
C
C*	Do the interpolation to the regularly spaced grid.
C
	DO ko = 2, nx
	    DO kn = 1, nptso
		indx = nx * ( kn - 1 )
		q = xptso ( kn )
C
C*		Compute interpolation weights.
C
		IF ( q .eq. xpts ( ko - 1 ) ) THEN
		    wt = 0.
		ELSE IF ( q .eq. xpts ( ko ) ) THEN
		    wt = 1.0
		ELSE IF ( ( q .gt. xpts ( ko - 1 ) .and.
     +			    q .lt. xpts ( ko ) ) .or.
     +			  ( q .lt. xpts ( ko - 1 ) .and.
     +			    q .gt. xpts ( ko ) ) ) THEN
		    wt = ( q - xpts ( ko - 1 ) ) / 
     +                   ( xpts ( ko ) - xpts ( ko - 1 ) )
		ELSE
		    wt = -9999.
		END IF
C
C*		If the weight is positive, apply it.
C
		IF ( wt .ge. 0.0 ) THEN
		    wb = 1.000 - wt
		    DO i = 1, nv
			indx = ((i-1)*nptso)+kn
			IF ( ERMISS (gridin ( ko, i ) ) .OR.
     +			     ERMISS (gridin ( ko-1, i ) ) ) THEN
			    grdout ( indx ) = RMISSD
			ELSE
			    grdout ( indx ) = gridin (ko, i) * wt +
     +					      gridin (ko-1, i ) * wb
			END IF
		    END DO
		END IF
	    END DO
	END DO
C*
	RETURN
	END
