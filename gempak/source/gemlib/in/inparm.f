	SUBROUTINE IN_PARM  ( nexp, parms, prmlst, nparm, iret )
C************************************************************************
C* IN_PARM								*
C*									*
C* This subroutine processes the input variable PARMS where the		*
C* input string contains a list of parameter names separated by		*
C* semicolons.  All spaces are eliminated from the input string.	*
C* If two consecutive semicolons are found, the parameter BLNK will	*
C* be inserted.								*
C*									*
C* IN_PARM  ( NEXP, PARMS, PRMLST, NPARM, IRET )			*
C*									*
C* Input parameters:							*
C*	NEXP		INTEGER		Maximum number of parameters	*
C*	PARMS		CHAR*		Parameter string		*
C*									*
C* Output parameters:							*
C*	PRMLST (NPARM)	CHAR*		Parameter array			*
C*	NPARM		INTEGER		Number of parameters		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/84	Original source code IP_PARMS		*
C* I. Graffman/RDS	 5/86	Renamed IN_PARM				*
C* M. desJardins/GSFC	 6/88	Renamed					*
C* T. Lee/SAIC		12/01	Increased prmstr string length to LLMXLN*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	parms, prmlst (*)
C*
	CHARACTER	prmstr* (LLMXLN)
C------------------------------------------------------------------------
	iret = 0
	CALL ST_LCUC  ( parms, prmstr, ier )
C
C*	Break single string into array of strings.
C
	CALL ST_CLST  ( prmstr, ';', 'BLNK', nexp, prmlst, nparm, ier )
C*
	RETURN
	END
