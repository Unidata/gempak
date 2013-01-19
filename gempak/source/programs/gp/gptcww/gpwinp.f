	SUBROUTINE GPWINP ( strmid, iret )
C************************************************************************
C* GPWINP								*
C*									*
C* This subroutine gets the input for GPTCWW.				*
C*									*
C* GPWINP  ( STRMID, IRET )						*
C*                                                                      *
C* Input parameters:                                                    *
C*	STRMID		CHAR*		Ocean/storm identifier          *
C*                                                                      *
C* Output parameters:                                                   *
C*	IRET		INTEGER		Return code			*
C*                                                                      *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/01	From GPPINP     	       	        *
C* m.gamazaychikov/SAIC	03/04	From GPKINP     	       	        *
C************************************************************************
	CHARACTER*(*)	strmid
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'STRMID', strmid,  ier1 )
C
	iret =  ier1
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
