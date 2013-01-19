	SUBROUTINE ST_NULL  ( string, outstr, lens, iret )
C************************************************************************
C* ST_NULL								*
C*									*
C* This subroutine appends a NULL character to the end of a string.	*
C* The input and output strings may be the same variable.  		*
C* If the string's length is equal to its dimensioned length, the last	*
C* character will be replaced by the NULL character.			*
C* The output length of the string does not include trailing blanks,	*
C* tabs, or nulls.							*
C*									*
C* ST_NULL  ( STRING, OUTSTR, LENS, IRET )				*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		String with NULL character	*
C*	LENS		INTEGER		Length of output string		*
C*	IRET		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C**									*
C* Log:									*
C* K. Tyle/GSC		12/96	Based on ST_LSTR 			*
C* S. Jacobs/NCEP	 6/00	Icreased internal string from 160 to 256*
C* S. Jacobs/NCEP	 2/01	Added check for blank string		*
C* T. Piper/SAIC	05/06	Modified to use st_lstr			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	CHARACTER*(*)	string, outstr
C
C------------------------------------------------------------------------
	lens = 0
	iret = 0
	lenout = LEN(outstr)
	IF  ( lenout .le. 0 )  RETURN
C
C*  Get length of input string.
C
	CALL ST_LSTR( string, lens, izero )
C
C*  Append NULL.
C
	IF  ( lens .eq. 0 )  THEN
	    outstr(1:1) = CHNULL
	ELSE IF  ( lens .lt. lenout )  THEN
	    outstr = string(:lens)
	    outstr(lens+1:lens+1) = CHNULL
	ELSE IF  ( lens .ge. lenout )  THEN
	    print *, 
     +"WARNING:  ST_NULL - Output string too small;",
     +" input string truncated!"
	    lens = lenout - 1
	    outstr = string(:lens)
	    outstr(lens+1:lens+1) = CHNULL
	END IF
C
	RETURN
	END
