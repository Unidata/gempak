	SUBROUTINE DA_RFHR ( iflno, fhdnam, mxword,
     +			     rheadr, nword, iret )
C************************************************************************
C* DA_RFHR								*
C*									*
C* This subroutine reads real header info from a non-GEMPAK		*
C* data source.								*
C*									*
C* DA_RFHR  ( IFLNO, FHDNAM, MXWORD, RHEADR, NWORS, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	FHDNAM		CHAR*4		Header name			*
C*	MXWORD		INTEGER		Maximum words to return		*
C*									*
C* Output parameters:							*
C*	RHEADR (NWORD)	REAL		File header			*
C*	NWORD		INTEGER		Header length 			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 8/13	Created					*
C* S. Gilbert/NCEP	 6/14	Fixed calling sequence for da_getfilhdr     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'../dm/dmcmn.cmn'
C*
	CHARACTER*(*)	fhdnam
	REAL		rheadr (*)
C*
	CHARACTER	fhdn*5
C------------------------------------------------------------------------
	iret = 0
C
C*	Request the data
C
	CALL ST_NULL ( fhdnam, fhdn, lenf, ier )
	CALL DA_GETFILHDR ( iflno, fhdn, mxword, rheadr, nword, iret )
C*
	RETURN
	END
