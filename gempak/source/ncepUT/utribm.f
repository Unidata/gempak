	REAL*8 FUNCTION UT_RIBM  ( rval )
C************************************************************************
C* UT_RIBM								*
C*									*
C* This function takes as input an interface value and then returns it	*
C* as a REAL*8 BUFR value.  If RMISSD is input, then the BUFR "missing"	*
C* value is returned.							*
C*									*
C* UT_RIBM  ( RVAL )							*
C*									*
C* Input parameters:							*
C*	RVAL		REAL		Interface value			*
C*									*
C* Output parameters:							*
C*	UT_RIBM		REAL*8		BUFR value			*
C**									*
C* Log:									*
C* J. Ator/NCEP		04/98						*
C* J. Ator/NCEP		03/00	Clean up function declarations		*
C* J. Ator/NCEP		10/00	PF_RIBM -> UT_RIBM			*
C* J. Ator/NCEP		06/01	Use 'BUFR.CMN' to define R8BFMS		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BUFR.CMN'
C*
	INCLUDE		'ERMISS.FNC'
C*-----------------------------------------------------------------------
	iret = 0
C
	IF  ( ERMISS ( rval ) )  THEN
	    UT_RIBM = R8BFMS
	ELSE
	    UT_RIBM = rval
	END IF
C*
	RETURN
	END
