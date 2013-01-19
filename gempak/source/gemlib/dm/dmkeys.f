	SUBROUTINE DM_KEYS  ( iflno, nrkeys, keyrow, nckeys, keycol,
     +			      iret )
C************************************************************************
C* DM_KEYS								*
C*									*
C* This subroutine returns the row and column keys in a DM file.	*
C*									*
C* DM_KEYS  ( IFLNO, NRKEYS, KEYROW, NCKEYS, KEYCOL, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	NRKEYS		INTEGER		Number of row keys		*
C*	KEYROW (NRKEYS)	CHAR*4		Row keys			*
C*	NCKEYS		INTEGER		Number of column keys		*
C*	KEYCOL (NCKEYS)	CHAR*4		Column keys			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file is not open		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	keyrow (*), keycol (*)
C------------------------------------------------------------------------
C*	Check that the file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Return row keys.
C
	nrkeys = krkeys ( iflno )
	DO  i = 1, nrkeys
	    keyrow ( i ) = kkrow ( i, iflno )
	END DO
C
C*	Return column keys.
C
	nckeys = kckeys ( iflno )
	DO  i = 1, nckeys
	    keycol ( i ) = kkcol ( i, iflno )
	END DO
C*
	RETURN
	END
