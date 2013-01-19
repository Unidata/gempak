	SUBROUTINE GDXSFM ( ivcord, xgrd, rlvl, nhxs, nvxs,	
     +			    vclsfc, iret)			
C************************************************************************
C* GDXSFM								*
C*									*
C* This subroutine sets subsurface values to missing in a cross		*
C* section grid.							*
C*									*
C* GDXSFM  ( IVCORD, XGRD, RLVL, NHXS, NVXS, VCLSFC, IRET )		*
C*									*
C* Input parameters:							*
C*									*
C*	IVCORD		  INTEGER	Vertical coordinate type	*
C*					  1 = PRES			*
C*				          2 = THTA			*
C*					  3 = HGHT			*
C*	XGRD (NHXS, NVXS) REAL		Array of cross section values	*
C*      RLVL (NVXS)	  REAL		Vertical levels in grid		*	
C*	NHXS		  INTEGER	Number of xsect pts in horiz.	*
C*      NVXS              INTEGER       Number of xsect pts in vert.	*
C*      VCLSFC (NHXS)     REAL 		Vert coord location of sfc	*
C*									*
C* Output parameters:							*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. F. Brill/GSC       7/89						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		xgrd (nhxs, nvxs), vclsfc (nhxs), rlvl (nvxs)
	LOGICAL		done
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C* 	Set all values below the surface to missing.
C
	done = .false.
	i = 1
	DO WHILE ( .not. done )
	  test = rlvl ( i )
	  icnt = 0
	  DO ij = 1, nhxs
	    IF ( ERMISS ( vclsfc ( ij ) ) ) THEN
	    ELSE
	      IF ( ivcord .eq. 1 ) THEN
	        IF ( test .gt. vclsfc ( ij ) ) THEN
	          xgrd ( ij, i ) = RMISSD
	          icnt = icnt + 1
	        END IF
	      ELSE
	        IF ( test .lt. vclsfc ( ij ) ) THEN
	 	  xgrd ( ij, i ) = RMISSD
		  icnt = icnt + 1
	        END IF
	      END IF
	    END IF
	  END DO
	  IF ( icnt .eq. 0 ) done = .true.
	  i = i + 1
	  IF ( i .gt. nvxs ) done = .true.
        END DO  
C*
	RETURN
	END
