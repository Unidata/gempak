	SUBROUTINE ST_RXBL  ( string, outstr, length, iret )
C************************************************************************
C* ST_RXBL								*
C*									*
C* This subroutine removes extra spaces and tabs from a string.  Only	*
C* single blanks will separate substrings.  The input and output 	*
C* strings may be the same variable.					*
C*									*
C* ST_RXBL  ( STRING, OUTSTR, LENGTH, IRET )				*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		String without blanks		*
C*	LENGTH		INTEGER		Length of output string		*
C*	IRET		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/88						*
C* D. Keiser/GSC	 8/96		Add temp variables sss and ttt	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, outstr
C*
	CHARACTER	c*1, sss*160, ttt*160
C-----------------------------------------------------------------------
	length = 0
	iret   = 0
	sss    = string
	ttt    = ' '
C
C*	Remove leading spaces and tabs.
C
	CALL ST_LDSP  ( sss, ttt, lens, iret )
	IF  ( lens .le. 0 )  RETURN
C
C*	Remove extra spaces.
C
	ispac  = 0
	length = 0
	DO  j = 1, lens
	    c = ttt (j:j)
	    IF  ( ( c .ne. CHSPAC ) .and. ( c .ne. CHTAB  ) ) THEN
	        length = length + 1
	        ttt (length:length) = c 
	        ispac = 0
	      ELSE
	        IF  ( ispac .eq. 0 )  THEN
	            length = length + 1
	            ttt (length:length) = ' '
	            ispac = 1
	        END IF
	    END IF
	END DO
C
C*	Make sure the end of the string is blank.
C
	lens = LEN ( ttt )
	IF  ( lens .gt. length )  ttt ( length+1 : ) = ' '
C*
	outstr = ttt
C*
	RETURN
	END
