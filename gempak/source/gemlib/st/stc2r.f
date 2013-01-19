	SUBROUTINE ST_C2R  ( string, nexp, rarr, num, iret )
C************************************************************************
C* ST_C2R								*
C*									*
C* This subroutine converts a string into an array of real numbers. 	*
C* The numbers may be separated by any non-alphanumeric character	*
C* except a period (.), a plus sign (+), a minus sign (-), or an	*
C* asterisk (*).							*
C*									*
C* ST_C2R  ( STRING, NEXP, RARR, NUM, IRET )				*
C*									*
C* Input parameters:                                                   	*
C*	STRING		CHAR*		String 				*
C*	NEXP		INTEGER		Maximum number of reals		*
C*									*
C* Output parameters:							*
C*	RARR (NUM)	REAL		Converted real array		*
C*	NUM		INTEGER		Number of converted reals	*
C*	IRET		INTEGER		Return code  			*
C*				 	  1 = more than NEXP reals 	*
C*				 	  0 = normal return		*
C*					 -1 = invalid string 		*
C*					 -2 = conversion error		*
C** 									*
C* Log:									*
C* I. Graffman/CSC	 7/82	STR_CHRL				*
C* I. Graffman/RDS	 2/84	Use new GEMPAK routines			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Sager/NCEP	 2/96   Increased size of strbuf		*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	CHARACTER*(*)	string
	REAL		rarr (*)
C*
	CHARACTER	strbuf*160
C------------------------------------------------------------------------
	num    = 0
	iret   = 0
	strbuf = string
C
C*	Remove separators.
C
	CALL ST_RNAN  ( strbuf, strbuf, lens, iret )
	IF  ( lens .le. 0 )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Set up pointers.
C
	ip = 1                              
	ie = 1                              
C
C*	Parse the string.
C
	DO WHILE  ( ip .le. lens )
	    ie = INDEX  ( strbuf (ip:lens), ' ' )
	    IF  ( ie .eq. 0 )  THEN
		ie = lens + 1
	      ELSE
		ie = ie + ip - 1
	    ENDIF
C
C*	    Check that num is not too large.
C
	    IF  ( num .ge. nexp )  THEN
	        iret = 1
	        RETURN
	      ELSE
		num = num + 1
C
C*		Convert.
C
		CALL ST_CRNM  ( strbuf (ip:ie-1), rarr (num), ier )
		IF ( ier .lt. 0 )  iret = -2
		ip = ie + 1
	    END IF
	END DO
C*
	RETURN
	END
