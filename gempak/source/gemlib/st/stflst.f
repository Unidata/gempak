	SUBROUTINE ST_FLST  ( string, sep, cdef, nexp, carr, num, iret )
C************************************************************************
C* ST_FLST								*
C*									*
C* This subroutine breaks a string containing a list of file names into	*
C* an array of file names.  The separator for the strings is input as	*
C* SEP.	If the separator is a blank, multiple blanks will be changed to	*
C* single blanks before the string is processed.  If null strings	*
C* are encountered or fewer than NEXP strings are found in the		*
C* string, the appropriate CARR locations are set to CDEF.		*
C*									*
C* If the first file name has two "\"s in it, the subsequent file names	*
C* are used to replace the text delineated by the "\"s in order to 	*
C* generate new file names.						*
C*									*
C* ST_FLST  ( STRING, SEP, CDEF, NEXP, CARR, NUM, IRET )		*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*	SEP		CHAR*1		Separator			*
C*	CDEF		CHAR*		Default string 			*
C*	NEXP		INTEGER 	Number of expected values 	*
C*									*
C* Output parameters:							*
C*	CARR  (NUM)	CHAR*		Array of file names 		*
C*	NUM		INTEGER 	Number of names returned	*
C*	IRET		INTEGER 	Return code			*
C*				   	  1 = more than NEXP values	*
C*				   	  0 = normal return		*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 8/94		Copied from ST_CLST		*
C************************************************************************
	CHARACTER*(*) 	string, sep, carr (*), cdef
C*
	CHARACTER	cchar*1, tmpstr*72, farr (3)*72
C------------------------------------------------------------------------
	iret = 0
	num  = 0
C
C*	Separate the file names.
C
	CALL ST_CLST ( string, sep, cdef, nexp, carr, num, iret )
	IF  ( num .eq. 0 )  RETURN
C
C*	Set cchar to backslash (\).
C*	Separate the first file name into three subparts.
C
	cchar = CHAR (92)
	CALL ST_CLST ( carr(1), cchar, ' ', 3, farr, num2, iret )
C
C*	If there is only one item in the list, return with the 
C*	list of file names from the first separation.
C
	IF  ( num2 .eq. 1 )  RETURN
C
C*	Reset the first file name to the middle subpart.
C
	carr (1) = farr (2)
C
C*	Loop over all of the file name replacement parts.
C
	DO  i = 1, num
C
C*	    Add the first subpart to the temp string.
C
	    tmpstr = farr(1)
	    CALL ST_LSTR ( tmpstr, lent, ier )
C
C*	    Add the replacement string to the temp string.
C
	    tmpstr (lent+1:) = carr(i)
	    CALL ST_LSTR ( tmpstr, lent, ier )
C
C*	    Add the third subpart to the temp string.
C
	    tmpstr (lent+1:) = farr (3)
C
C*	    Set the output string to the temp string.
C
	    carr(i) = tmpstr
	END DO
C*
	RETURN
	END
