	SUBROUTINE IN_PRMC  ( nexp, parms, prmlst, prmcnd, nparm, iret )
C************************************************************************
C* IN_PRMC								*
C*									*
C* This subroutine processes the input variable PARMS where the		*
C* input string contains a list of parameter names separated with	*
C* semicolons.  All spaces are eliminated from the input string.	*
C* If two consecutive semicolons are found, the parameter BLNK will	*
C* be inserted.  Any characters after the fourth character in the	*
C* parameter name are returned as conditions.  This subroutine is the	*
C* same as IN_PARM except that the condition array is returned here.	*
C*									*
C* IN_PRMC  ( NEXP, PARMS, PRMLST, PRMCND, NPARM, IRET )		*
C*									*
C* Input parameters:							*
C*	NEXP		INTEGER		Maximum number of parameters	*
C*	PARMS		CHAR*		Parameter string		*
C*									*
C* Output parameters:							*
C*	PRMLST (NPARM)	CHAR*4		Parameter array			*
C*	PRMCND (NPARM)	CHAR*		Parameter condition array	*
C*	NPARM		INTEGER		Number of parameters		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/84	Original source code IP_PARMS		*
C* I. Graffman/RDS	 5/86	Renamed IN_PARM				*
C* M. desJardins/GSFC	 6/88	GEMPAK 4				*
C* M. desJardins/GSFC	11/89	Added conditions			*
C* C. Lin/Eai		 4/98   Changed prmstr dimension to LLMXLN	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms, prmlst (*), prmcnd (*)
C*
	CHARACTER	prmstr*(LLMXLN), ppp (MMPARM)*24
C------------------------------------------------------------------------
	iret = 0
	CALL ST_LCUC  ( parms, prmstr, ier )
C
C*	Break single string into array of strings.
C
	CALL ST_CLST  ( prmstr, ';', 'BLNK', nexp, ppp, nparm, ier )
C
C*	Separate parameter name from conditions.
C
	DO  i = 1, nparm
	    prmlst (i) = ppp (i) (1:4)
	    prmcnd (i) = ppp (i) (5: )
	END DO
C*
	RETURN
	END
