	SUBROUTINE VC_LNIN  ( pb, pt, pp, vb, vt, nxy, vv, iret )
C************************************************************************
C* VC_LNIN								*
C*									*
C*  This subroutine performs linear interpolation.			*
C*									*
C* VC_LNIN ( PB, PT, PP, VB, VT, NXY, VV, IRET )			*
C*									*
C* Input parameters:							*
C*	PB (NXY)	REAL		Ln (p) bottom			*
C*	PT (NXY)	REAL		Ln (p) top			*
C*	PP (NXY)	REAL		Ln (p) at level of interest	*
C*	VB (NXY)	REAL		Value at bottom			*
C*	VT (NXY)	REAL		Value at top			*
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
C* K. Brill/NMC	     	08/92	Added all missing error			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		pb (*), pt (*), pp (*), vb (*), vt (*), vv (*)
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
	    IF ( BETWEN ( pp (ij), pb (ij), pt (ij) ) .and.
     +		 .not. ( ERMISS ( pb (ij) ) .or.
     +			 ERMISS ( pt (ij) ) .or.
     +		         ERMISS ( pp (ij) ) .or.
     +			 ERMISS ( vb (ij) ) .or.
     +			 ERMISS ( vt (ij) )      ) .and.
     +			 pt (ij) .ne. pb (ij) ) THEN
C
C*		Do linear interpolation if values are not missing.
C
		vv (ij) = vb (ij) + ( pp (ij) - pb (ij) ) *
     +		       ( ( vt (ij) - vb (ij) ) / ( pt (ij) - pb (ij) ) )
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
