	SUBROUTINE ST_FIND  ( string, stlist, nstr, ipos, iret )
C************************************************************************
C* ST_FIND								*
C*									*
C* This subroutine searches for a particular string in a list of	*
C* strings.  The position in the array is returned in IPOS.  If the	*
C* string is not found, IPOS is set to 0.				*
C*									*
C* ST_FIND  ( STRING, STLIST, NSTR, IPOS, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*	STLIST (NSTR)	CHAR*		List of strings			*
C*	NSTR		INTEGER		Number of strings in list	*
C*									*
C* Output parameters:							*
C*	IPOS		INTEGER		Position of string in list	*
C*				 	  0 = not found			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	8/84						*
C* M. Goodman/RDS	11/84   Changed name from PC_FPRM to ST_FIND	*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	CHARACTER*(*)	string, stlist (*)
C-------------------------------------------------------------------------
	ipos = 0
	iret = 0
	knt  = 1
C
C*	Check each item to see if the string is in the list.
C
	DO WHILE  ( ( knt .le. nstr ) .and. (ipos .eq. 0 ) )
	    IF  ( string .eq. stlist (knt) )  ipos = knt
	    knt = knt + 1
	END DO
C*
	RETURN
	END
