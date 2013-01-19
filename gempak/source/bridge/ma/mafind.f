	LOGICAL FUNCTION MA_FIND  ( string, stlist, nstr )
C************************************************************************
C* MA_FIND								*
C*									*
C* This function tests for a particular string in a list of strings.	*
C*									*
C* LOGICAL MA_FIND  ( STRING, STLIST, NSTR )				*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*	STLIST (NSTR)	CHAR*		List of strings			*
C*	NSTR		INTEGER		Number of strings in list	*
C*									*
C* Output parameters:							*
C*	MA_FIND		LOGICAL		.true. if string is in stlist	*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 4/01	Created					*
C************************************************************************
	CHARACTER*(*)	string, stlist (*)
C-------------------------------------------------------------------------
	CALL ST_FIND ( string, stlist, nstr, ipos, iret )
	MA_FIND = ( ipos .ne. 0 )
C*
	RETURN
	END
