	SUBROUTINE VC_QUAD  ( p1, p2, p3, pp, v1, v2, v3, nxy, vv, iret )
C************************************************************************
C* VC_QUAD								*
C*									*
C*  This subroutine performs quadratic interpolation.			*
C*									*
C* VC_QUAD ( P1, P2, P3, PP, V1, V2, V3, NXY, VV, IRET )		*
C*									*
C* Input parameters:							*
C*	P1 (NXY)	REAL		Ln (p) bottom			*
C*	P2 (NXY)	REAL		Ln (p) intermediate		*
C*	P3 (NXY)	REAL		Ln (p) top			*
C*	PP (NXY)	REAL		Ln (p) at level of interest	*
C*	V1 (NXY)	REAL		Value at bottom			*
C*	V2 (NXY)	REAL		Value at intermediate level	*
C*	V3 (NXY)	REAL		Value at top			*
C*	NXY		INTEGER		Number of grid points		*
C*									*
C* Output parameters:							*
C*	VV (NXY)	REAL		Value on level of interest	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC		07/92						*
C* K. Brill/NMC		08/92	Add missing value check			*
C* K. Brill/NMC		11/92	Add last icnt increment			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		p1 (*), p2 (*), p3 (*), pp (*),
     +			v1 (*), v2 (*), v3 (*), vv (*)
C*
	LOGICAL		betwen
	INCLUDE		'ERMISS.FNC'
C*
	BETWEN ( rlev, rlo, rhi ) = 
     +     ( ( rlev .ge. rlo .and. rlev .le. rhi )   .or.
     +       ( rlev .le. rlo .and. rlev .ge. rhi ) )
C-----------------------------------------------------------------------
	iret = 0
	icnt = 0
C
C*	Interpolate only at points between the two bounding surfaces.
C*	Store missing values elsewhere.
C
	DO ij = 1, nxy
	    IF ( BETWEN ( pp (ij), p1 (ij), p3 (ij) ) .and.
     +		 .not. ( ERMISS ( p1 (ij) ) .or.
     +			 ERMISS ( p2 (ij) ) .or.
     +			 ERMISS ( p3 (ij) ) .or.
     +		         ERMISS ( pp (ij) ) .or.
     +			 ERMISS ( v1 (ij) ) .or.
     +			 ERMISS ( v2 (ij) ) .or.
     +			 ERMISS ( v3 (ij) )      ) .and.
     +			 p1 (ij) .ne. p2 (ij) .and.
     +			 p2 (ij) .ne. p3 (ij) .and.
     +			 p1 (ij) .ne. p3 (ij) ) THEN
C
C*		Do quadratic interpolation if values are not missing.
C
		a = p1 (ij) * p1 (ij)
		b = p1 (ij)
		d = p2 (ij) * p2 (ij)
		e = p2 (ij)
		g = p3 (ij) * p3 (ij)
		h = p3 (ij)
		f = d - g
		ff = a - d
		IF ( f .ne. 0.0 .and. ff .ne. 0.0 ) THEN
		    c = ( e - h ) / f
		    cc =  ( v2 (ij) - v3 (ij) ) / f
		    y = ( v1 (ij) - v2 (ij) ) / ff - cc
		    y = y / ( (b-e)/ff - c )
		    x = cc - c * y
		    z = v3 (ij) - g * x - h * y
C*
		    vv ( ij ) = z + pp (ij) * ( y + x * pp (ij) )
		ELSE
		    vv ( ij ) = RMISSD
		    icnt = icnt + 1
		END IF
C*
	    ELSE
		vv (ij) = RMISSD
		icnt = icnt + 1
	    END IF
	END DO
	IF ( icnt .eq. nxy ) iret = +1
C*
	RETURN
	END
