	SUBROUTINE GPPINP ( strmid, iret )
C************************************************************************
C* GPPINP								*
C*									*
C* This subroutine gets the input for GPTPC.				*
C*									*
C* GPPINP  ( STRMID, IRET )						*
C*									*
C* Input parameters:                                                    *
C*	STRMID		CHAR*		Ocean/storm identifier		*
C*									*
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                                                      *
C**									*
C* Log:									*
C* A. Hardy/GSC		 2/01	Copied from GPMINP			*
C* D. Kidwell/NCEP	 6/01	Renamed from GPTINP to GPPINP           *
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
