	SUBROUTINE WO_WFO ( string, len, wfostn, iret )
C************************************************************************
C* WO_WFO 								*
C*									*
C* This subroutine finds the wfo station information and replaces the   *
C* station separator '...' with ';'.					*
C*                                                                      *
C* WO_WFO ( STRING, LEN, WFOSTN, IRET )               			*
C*									*
C* Input parameters:	                                                *
C*  	STRING          CHAR*           WFO string			*
C*  	LEN          	INTEGER         Length of WFO string		*
C*									*
C* Output parameters:							*
C*	WFOSTN          CHAR*    	String of activated WFO stns    *
C*	IRET  	  	INTEGER	 	Return code			*
C*					 -10 = length of WFO not 3	*
C*					 -13 = len of input string > 160*
C*					 -14 = len of output string >120*
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC		08/02						*
C* A. Hardy/NCEP	 4/04	Added removal of '$$' from final msg	*
C* F. J. Yen/NCEP	10/06	Check valid length of string, WFOs,wstr;*
C* F. J. Yen/NCEP	10/07	Remove blanks in wstr (length of ATTN	*
C*				line greater than 1 text line;watch#724)* 
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	string,  wfostn
C*
	CHARACTER	wstr*160
C------------------------------------------------------------------------
	iret  = 0
	wfostn = ' '
        ii = 0 
C
C*	Get the length of the wfo string.
C
      	CALL ST_LSTR ( string(14:len), leng, ier )
	IF ( leng .gt. 160 ) THEN
	    iret = -13
	    RETURN
	END IF
        wstr = string(14:len)
	CALL ST_RMBL ( wstr, wstr, lens, ier )
C
C* 	Replace the '...' with ';'.
C
	jj = 0
	iposv = 0
        DO WHILE ( ii .le. lens )
	    CALL ST_RPST ( wstr(:lens), '...',';', ipos, wstr, ier)
C
C*	    Verify that each WFO is of length 3 by testing 
C*	    IF (iposv - ipos .ne. 6) since position includes '...'.
C
	    IF ( ier .eq. 0 .and. ipos .gt. 0 ) THEN
	        IF ( ipos - iposv .ne. 4 ) THEN
		    iret = -10
      		    RETURN
		END IF
	        iposv = ipos
		ii = ii + 6
	      ELSE
		IF ( iposv .eq. 0 ) THEN
		    iret = -10
      		    RETURN
		END IF
		ii = lens + 1
	    END IF
        END DO
C
C*      Save the string with the ';' separators. Remove the last one.
C
        IF ( ier .eq. 0 ) THEN
	    CALL ST_LSTR ( wstr, lens, ier )
            IF ( wstr(lens-1:lens) .eq. '$$' ) THEN
                lens = lens - 3
            END IF
	    CALL ST_LSTR ( wstr(:lens-1), leng, ier )
	    IF ( leng .gt. 120 ) THEN
	        iret = -14
		RETURN
	    END IF
	    wfostn = wstr(:lens-1)
        END IF
C*
	RETURN
	END
