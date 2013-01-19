	LOGICAL FUNCTION UT_BFMS  ( r8val )
C************************************************************************
C* UT_BFMS								*
C*									*
C* This function returns .TRUE. iff the input REAL*8 value R8VAL is	*
C* "missing".  It is modeled after the GEMPAK function ERMISS but is	*
C* designed to work with REAL*8 values and compare them to the		*
C* BUFR "missing" value.						*
C*									*
C* UT_BFMS ( R8VAL )							*
C*									*
C* Input parameters:							*
C*	R8VAL		REAL*8		Value to be checked 		*
C*					for "missing"			*
C**									*
C* Log:									*
C* J. Ator/NCEP		07/02						*
C************************************************************************
	INCLUDE		'BUFR.CMN'
C*
	REAL*8		r8val
C*-----------------------------------------------------------------------
	IF  ( ABS ( r8val - R8BFMS ) .lt. R8DIFD )  THEN
	    UT_BFMS = .true.
	ELSE
	    UT_BFMS = .false.
	END IF
C*
	RETURN
	END
