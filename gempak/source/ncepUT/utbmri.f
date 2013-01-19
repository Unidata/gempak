	REAL FUNCTION UT_BMRI  ( r8val )
C************************************************************************
C* UT_BMRI								*
C*									*
C* This function takes as input a REAL*8 BUFR value and then returns it	*
C* as an interface value.  If the BUFR "missing" value is input, then	*
C* RMISSD is returned.							*
C*									*
C* UT_BMRI ( R8VAL )							*
C*									*
C* Input parameters:							*
C*	R8VAL		REAL*8		BUFR value			*
C*									*
C* Output parameters:							*
C*	UT_BMRI		REAL		Interface value			*
C**									*
C* Log:									*
C* J. Ator/NCEP		04/98						*
C* J. Ator/NCEP		02/00	Remove includes of 'pfcmn_bufr.cmn'	*
C*				and 'ERMISS.FNC'			*
C* J. Ator/NCEP		09/00	PF_BMRI -> UT_BMRI, modify R8BFMS test	*
C* J. Ator/NCEP		06/01	Use 'BUFR.CMN' to define R8BFMS		*
C* J. Ator/NCEP		07/02	Use UT_BFMS				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		UT_BFMS
C*
	REAL*8		r8val
C*-----------------------------------------------------------------------
	IF  ( UT_BFMS ( r8val ) )  THEN
	    UT_BMRI = RMISSD
	ELSE
	    UT_BMRI = r8val
	END IF
C*
	RETURN
	END
