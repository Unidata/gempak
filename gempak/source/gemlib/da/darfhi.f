	SUBROUTINE DA_RFHI ( iflno, fhdnam, mxword,
     +			     iheadr, nword, iret )
C************************************************************************
C* DA_RFHI								*
C*									*
C* This subroutine reads integer header info from a non-GEMPAK		*
C* data source.								*
C*									*
C* DA_RFHI  ( IFLNO, FHDNAM, MXWORD, IHEADR, NWORS, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	FHDNAM		CHAR*4		Header name			*
C*	MXWORD		INTEGER		Maximum words to return		*
C*									*
C* Output parameters:							*
C*	IHEADR (NWORD)	INTEGER		File header			*
C*	NWORD		INTEGER		Header length 			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 8/13	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'../dm/dmcmn.cmn'
C*
	CHARACTER*(*)	fhdnam
	INTEGER		iheadr (*)
C*
	CHARACTER	fhdn*5
C------------------------------------------------------------------------
	iret = 0
C
C*	Request the data
C
	CALL ST_NULL ( fhdnam, fhdn, lenf, ier )
	CALL DA_GETFILHDI ( iflno, fhdn, mxword, iheadr, nword, iret )
C*
	RETURN
	END
