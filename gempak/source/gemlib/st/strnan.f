	SUBROUTINE ST_RNAN  ( string, outstr, length, iret )
C************************************************************************
C* ST_RNAN                                     				*
C* 									*
C* This subroutine replaces non-alphanumeric characters with spaces 	*
C* and removes the extra spaces from a character string.  The		*
C* characters period (.), plus sign (+), minus sign (-) and asterisk	*
C* (*) are not removed.							*
C* 									*
C* ST_RNAN  ( STRING, OUTSTR, LENGTH, IRET )				*
C*									*
C* Input parameters:                                                   	*
C*	STRING		CHAR*		String 				*
C*									*
C* Output parameters:                                                  	*
C*	OUTSTR		CHAR*		Converted string          	*
C*	LENGTH		INTEGER		Length of output string		*
C*	IRET		INTEGER 	Return code  			*
C*				   	 0 = normal return 		*
C** 									*
C* I. Graffman/RDS	 2/84						*
C* M. desJardins/GSFC	 4/84	remove plus signs (+)			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Sager/NCEP         2/96   Increased size of strbuf to 160         *
C* S. Jacobs/NCEP	11/96	Added temporary variable ttt;		*
C*				Added ST_RXBL to remove extra blanks	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, outstr
C*
	CHARACTER	strbuf*160, ttt*160
C------------------------------------------------------------------------
	length = 0
	iret   = 0
	strbuf = string
	ttt    = ' '
C
C*	Remove leading spaces and tabs.
C
	CALL ST_LDSP  ( strbuf, ttt, isiz, iret )
	IF  ( isiz .le. 0 )  RETURN
C
C*	Check each character in string.
C
	DO  i = 1, isiz
C
C*	    Find non alphanumerics.
C
	    CALL ST_ALNM  ( ttt (i:i), itype, iret )
	    IF  ( itype .eq. 0 )  THEN
C
C*	        Check for period, minus sign, plus sign or asterisk.
C*
	        IF  ( ( ttt (i:i) .ne. '.' ) .and.
     +		      ( ttt (i:i) .ne. '-' ) .and.
     +		      ( ttt (i:i) .ne. '+' ) .and.
     +		      ( ttt (i:i) .ne. '*' ) )   THEN
		    ttt (i:i) = CHSPAC
		END IF
	    END IF
	END DO
C
C*	Remove extra spaces.
C
	CALL ST_RXBL  ( ttt, ttt, length, ier )
C
C*	Transfer to output string.
C
	IF  ( length .gt. 0 )  outstr = ttt (1:length)
C*
	RETURN
	END
