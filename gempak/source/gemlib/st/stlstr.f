	SUBROUTINE ST_LSTR  ( string, lens, iret )
C************************************************************************
C* ST_LSTR								*
C*									*
C* This subroutine returns the number of characters in a string 	*
C* disregarding trailing null characters, tabs and spaces.		*
C*									*
C* ST_LSTR  ( STRING, LENS, IRET )					*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*									*
C* Output parameters:							*
C*	LENS		INTEGER 	Length of string		*
C*	IRET		INTEGER		Return code			*
C*				 	 0 = normal return 		*
C**									*
C* Log:									*
C* J. Woytek/GSFC	 6/82 	STR_LNSTR				*
C* I. Graffman/RDS	 2/84 	Fix zero length string handling		*
C* M. desJardins/GSFC	 6/88	Rewritten				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string
C*
	CHARACTER*1	ch
C*------------------------------------------------------------------------
	iret = 0
C
C*  Get the actual length of the string.
C
	lens = LEN  ( string )
	IF  ( lens .eq. 0 )  RETURN
C
C*  Start at last character and loop backwards.
C
	ip = lens
	DO WHILE  ( ip .gt. 0 )
C
C*  Get current value of string and check for space, null, tab.
c
	    ch = string ( ip : ip )
	    IF  ( ( ch .eq. CHSPAC ) .or. ( ch .eq. CHNULL ) .or.
     +		  ( ch .eq. CHTAB  ) )  THEN
		lens = lens - 1
		ip   = ip - 1
	      ELSE
		ip   = 0
	    END IF
	END DO
C*
	RETURN
	END
