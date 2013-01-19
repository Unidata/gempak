	SUBROUTINE ST_C2C  ( string, nexp, strarr, num, iret )
C************************************************************************
C* ST_C2C								*
C*									*
C* This subroutine breaks a string to an array of strings.  The		*
C* string separators may be any non-alphanumeric character except 	*
C* a minus sign (-), plus sign (+), asterisk (*) or period (.).		*
C*									*
C* ST_C2C  ( STRING, NEXP, STRARR, NUM, IRET )				*
C*									*
C* Input parameters:                                                   	*
C*	STRING		CHAR*		String 				*
C*	NEXP		INTEGER 	Maximum number of strings	*
C*									*
C* Output parameters:                                                  	*
C*	STRARR (*) 	CHAR*		String array			*
C*	NUM		INTEGER		Number of strings		*
C*	IRET 		INTEGER		Return code  			*
C*				   	  1 = more than NEXP strings 	*
C*				   	  0 = normal return		*
C*				  	 -1 = no strings input		*
C** 									*
C* Log:									*
C* I. Graffman/RDS    	 2/84   					*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Sager/NCEP	 2/96	Increased size of strbuf		*
C************************************************************************
	CHARACTER*(*)	string, strarr (*)
C*
	CHARACTER  	strbuf*160
C------------------------------------------------------------------------
	iret = 0
	num  = 0
C
C*	Remove separators.
C
	CALL ST_RNAN  ( string, strbuf, lnstr, iret )
C
C*	Check for blank string.
C
	IF  ( lnstr .eq. 0 )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Set up pointer.
C
	ip = 1                        
	DO WHILE  ( ip .le. lnstr )
C
C*	    Check for blank.
C
	    ie = INDEX  ( strbuf ( ip : lnstr ), ' ' )
	    IF  ( ie .eq. 0 )  THEN
		ie = lnstr + 1
	      ELSE
		ie = ie + ip - 1
	    ENDIF
C
C*	    If there are too many strings, flag and exit.
C
	    IF  ( num .ge. nexp )  THEN
	        iret = 1
	        RETURN
	      ELSE
		num = num + 1
		strarr ( num ) = strbuf ( ip : ie )
	    END IF
C
C*	    Increment pointer.
C
	    ip = ie + 1
	END DO
C*
	RETURN
	END
