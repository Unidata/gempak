	SUBROUTINE ST_C2I  ( string, nexp, intarr, num, iret )
C************************************************************************
C* ST_C2I								*
C*									*
C* This subroutine breaks a string to an array of integers.  The	*
C* integers may be separated by any non-alphanumeric character 		*
C* except a minus sign (-), a plus sign (+), a period (.) or an 	*
C* asterisk (*).							*
C*									*
C* ST_C2I  ( STRING, NEXP, INTARR, NUM, IRET )				*
C*									*
C* Input parameters:                                                   	*
C*	STRING		CHAR*		String 				*
C*	NEXP		INTEGER 	Maximum number of integers	*
C*									*
C* Output parameters:                                                  	*
C*	INTARR (*)	INTEGER 	Integer array			*
C*	NUM		INTEGER		Number of integers		*
C*	IRET 		INTEGER		Return code  			*
C*				 	  1 = more than NEXP integers	*
C*				 	  0 = normal return		*
C*					 -1 = invalid string		*
C*					 -2 = conversion error		*
C** 									*
C* Log:									*
C* I. Graffman/RDS	 3/84 						*
C* M. desJardins/GSFC	 3/84	added plus sign exception		*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Sager/NCEP         2/96   Increased size of strbuf                *
C************************************************************************
	CHARACTER*(*)	string
	INTEGER		intarr (*)
C*
	CHARACTER	strbuf*160
C------------------------------------------------------------------------
	num  = 0
	iret = 0
C
C*	Remove separators.
C
	CALL ST_RNAN  ( string, strbuf, lens, iret )
	IF  ( lens .le. 0 )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Set up pointers and parse the string.
C
	ip = 1                              
	DO WHILE  ( ip .le. lens )
	    ie = INDEX  ( strbuf (ip:lens), ' ' )
	    IF  ( ie .eq. 0 )  THEN
		ie = lens + 1
	      ELSE
		ie = ip + ie - 1
	    ENDIF
C
C*	    Check for too many strings.
C
	    IF  ( num .ge. nexp )  THEN
	        iret = 1
		RETURN
	      ELSE
		num = num + 1
C
C*		Convert.
C
		CALL ST_NUMB  ( strbuf (ip:ie), intarr (num), ier )
		IF  ( ier .lt. 0 )  iret = -2
		ip = ie + 1
	    END IF
	END DO
C*
	RETURN
	END
