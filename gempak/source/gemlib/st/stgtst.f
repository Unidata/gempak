	SUBROUTINE ST_GTST  ( template, substr, delim, string, 
     +			      outstr, len, iret )
C************************************************************************
C* ST_GTST								*
C*									*
C* This subroutine finds a substring within a template and returns 	*
C* the corresponding value of the substring within a given string.	*
C* The substring must be completely isolated using delim.		*
C* The number of delimiters in template and string must be equal.	*
C*									*
C* ST_GTST  ( TEMPLATE, SUBSTR, DELIM, STRING, OUTSTR, LEN, IRET )	*
C*									*
C* Input parameters:							*
C*	TEMPLATE	CHAR*		Template String			*
C* 	SUBSTR		CHAR*		Substring 			*
C* 	DELIM 		CHAR*1		Separation delimiter		*
C* 	STRING 		CHAR* 		String to extract substr from	*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		Output string 			*
C* 	LEN		INTEGER		Length of output string		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = string pttns do not match	*
C*					 -2 = substr not found in tmplt	*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 3/00						*
C* S. Jacobs/NCEP	10/13	Increased interal strings size: 32->40	*
C************************************************************************
	PARAMETER	( NF = 20 )
C
	CHARACTER*(*)	template, string, substr, delim, outstr
C*
	CHARACTER	tmplt*256, str*256, sep*1
	CHARACTER*40	ctmplt(NF), cstr(NF)
C------------------------------------------------------------------------
	iret = 0
	tmplt  = template
	str  = string
	sep = delim(1:1)
C
C*	Split each string (template and string) into substrings using sep
C
	CALL ST_CLST ( tmplt, sep, ' ', NF, ctmplt, ntmplt, ier )
	CALL ST_CLST (   str, sep, ' ', NF,   cstr,   nstr, ier )
C
	IF ( ntmplt .ne. nstr )  THEN
	    iret = -1
	    RETURN
	END IF
C
	DO  ii = 1, ntmplt
	    IF ( ctmplt(ii) .eq. substr )  THEN
		outstr = cstr(ii)
		CALL ST_LSTR ( outstr, len, ier )
		RETURN
	    END IF
	END DO
C
	iret = -2
C*
	RETURN
	END 
