	SUBROUTINE OA_WFSR  ( deltan, search, weight, srad, iret )
C************************************************************************
C* OA_WFSR								*
C*									*
C* This subroutine computes the weighting factor and search radius to	*
C* be used in the Barnes analysis.  The weighting factor is computed	*
C* using the formula:							*
C*									*
C*        WEIGHT = [ 5.051457 * ( DELTAN * 2. / PI ) ** 2 ]		*
C*									*
C* The search radius, SRAD, is computed as SEARCH * WEIGHT.  This	*
C* limits the search area to stations whose weights will be larger	*
C* than  [ EXP ( -SEARCH ) ] .  If SEARCH is non-positive, a value	*
C* of 20 will be used.  Both the weighting factor and search radius	*
C* should be multiplied by GAMMA for the second pass analysis.		*
C*									*
C* OA_WFSR  ( DELTAN, SEARCH, WEIGHT, SRAD, IRET )			*
C*									*
C* Input parameters:							*
C*	DELTAN		REAL		Station spacing			*
C*	SEARCH		REAL		User input search condition	*
C*									*
C* Output parameters:							*
C*	WEIGHT		REAL		Weighting factor		*
C*	SRAD		REAL		Search radius			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/86						*
C* M. desJardins/GSFC	10/88	Documentation				*
C* K. Brill/GSC          4/90   Changed default SEARCH  to 7.		*
C* K. Brill/NMC          9/90   Changed default SEARCH  back to 20.	*
C* M. Linda/GSC		 9/97	Corrected right border of prologue	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C------------------------------------------------------------------------
	iret = 0
C
C*	Compute the weighting factor using S. Koch's formula (documented
C*	in his tech memo and paper).
C
	weight = 5.051457 * ( deltan * 2. / PI ) **2
C
C*	The search radius is set to 20. * weight if no value is entered.
C
	IF ( search .le. 0. ) THEN
	    srad = 20. * weight
	  ELSE
	    srad = search * weight
	END IF
C*
	RETURN
	END
