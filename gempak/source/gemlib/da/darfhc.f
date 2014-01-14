	SUBROUTINE DA_RFHC ( iflno, fhdnam, mxword,
     +			     cheadr, nword, iret )
C************************************************************************
C* DA_RFHC								*
C*									*
C* This subroutine reads character header info from a non-GEMPAK	*
C* data source.								*
C*									*
C* DA_RFHC  ( IFLNO, FHDNAM, MXCHAR, CHEADR, NCHAR, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	FHDNAM		CHAR*4		Header name			*
C*	MXCHAR		INTEGER		Maximum characters to return	*
C*									*
C* Output parameters:							*
C*	CHEADR		CHAR*NCHAR	File header			*
C*	NCHAR		INTEGER		Header length 			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 8/13	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'../dm/dmcmn.cmn'
C*
	CHARACTER*(*)	fhdnam
	CHARACTER*(*)	cheadr
C*
	CHARACTER	fhdn*5
C------------------------------------------------------------------------
	iret = 0
C
C*	Request the data
C
	CALL ST_NULL ( fhdnam, fhdn, lenf, ier )
	CALL DA_GETFILHDI ( iflno, fhdn, mxchar, cheadr, nc, iret )
	CALL ST_RNUL ( cheadr, cheadr, nchar, ier )
C*
	RETURN
	END
