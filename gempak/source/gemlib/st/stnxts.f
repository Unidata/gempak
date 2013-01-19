 	SUBROUTINE ST_NXTS  ( string, ifirst, ilast, stlist, ilens, 
     +			      nstr,   ipos,   istrg, iret ) 
C************************************************************************
C* ST_NXTS								*
C*									*
C* This subroutine returns a pointer to the first occurrence of any	*
C* of a list of substrings within a given string.			*
C*									*
C* ST_NXTS  ( STRING, IFIRST, ILAST, STLIST, ILENS, NSTR, IPOS, 	*
C*            ISTRG,  IRET )						*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Input string			*
C*	IFIRST		INTEGER		First position to check		*
C*	ILAST		INTEGER		Last position to check		*
C*	STLIST (NSTR)	CHAR*		List of substrings 		*
C*	ILENS  (NSTR)	INTEGER		Lengths of substrings		*
C*	NSTR		INTEGER		Number of substrings		*
C*									*
C* Output parameters:							*
C*	IPOS		INTEGER		Position of first substring 	*
C*	ISTRG		INTEGER		Array element number of string	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = substring not found	*
C**									*
C* Log:									*
C* B. Doty/RDS		 9/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	CHARACTER*(*)	string, stlist (*)
	INTEGER		ilens (*)
C------------------------------------------------------------------------
	iret = 0 
C
C*  	Set the initial minimum position, then use INDEX to search for 
C*  	each substring.  When it is found, update the minimum position.
C
	ipos = ilast + 999
C*
	DO  i = 1, nstr
	    ilen = ilens (i)
	    iptr = INDEX ( string (ifirst:ilast), stlist (i) (:ilen) )
	    IF  ( iptr .ne. 0 )  THEN 
		iptr = iptr + ifirst - 1
		IF  ( iptr .lt. ipos )  THEN
		    ipos  = iptr
		    istrg = i
		ENDIF
	    ENDIF
	ENDDO
C
C*  	Check to see if we found anything.
C
	IF  ( ipos .gt. ilast )  THEN
	    istrg = 0 
	    ipos  = 0 
	    iret  = -3
	END IF
C*
	RETURN
	END
