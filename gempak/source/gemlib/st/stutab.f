	SUBROUTINE ST_UTAB  ( string, nchar, outstr, iret )
C************************************************************************
C* ST_UTAB								*
C*									*
C* This subroutine substitutes spaces for tabs in a string.  Spaces	*
C* are added for each tab found so that the character after the tab	*
C* appears at the next tab stop.  Tab stops are assumed to be at 	*
C* positions 9, 17, 25, ....  The input and output strings may		*
C* be the same variable.						*
C*									*
C* ST_UTAB  ( STRING, NCHAR, OUTSTR, IRET )				*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Input string			*
C*	NCHAR		INTEGER		Number of characters		*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		Output string			*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	11/89	Fix so STRING & OUTSTR can be the same	*
C* M. desJardins/GSFC	 3/90	Initialize output to blank		*
C* L. Sager/NCEP         2/96   Increased size of strtmp                *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, outstr
C*
	CHARACTER	strtmp*160
C*
	INTEGER		tabs (129)
	DATA		tabs /8*0, 1, 7*0, 1, 7*0, 1, 7*0, 1, 7*0, 1, 
     +	                      7*0, 1, 7*0, 1, 7*0, 1, 7*0, 1, 7*0, 1, 
     +	                      7*0, 1, 7*0, 1, 7*0, 1, 7*0, 1, 7*0, 1,
     +	                      7*0, 1/
C------------------------------------------------------------------------
	iret = 0
	ilen = 1
C
C*	Loop through each character checking for tabs.
C
	i = 1
	DO WHILE  ( ( i .le. nchar ) .and. ( ilen .le. 160 ) )
	    IF  ( string (i:i) .ne. CHTAB )  THEN
		strtmp (ilen:ilen) = string (i:i)
		ilen = ilen + 1
	      ELSE
		strtmp (ilen:ilen) = CHSPAC
	        ilen = ilen + 1
	        DO WHILE  ( tabs (ilen) .eq. 0 )
		    strtmp (ilen:ilen) = CHSPAC
		    ilen = ilen + 1
		END DO
	    END IF
	    i = i + 1
	END DO
C
C*	Transfer to output string.
C
	IF  ( ilen .lt. 160 )  strtmp (ilen : ) = ' '
	outstr = strtmp
C*
	RETURN
	END
