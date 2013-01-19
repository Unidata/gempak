	SUBROUTINE DM_RFIL  ( iflno, iret )
C************************************************************************
C* DM_RFIL								*
C*									*
C* This subroutine reads file header information from a DM file.	*
C*									*
C* DM_RFIL  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C------------------------------------------------------------------------
	iread  = kpfile ( iflno )
	num    = kfhdrs ( iflno )
C
C*	Return if there are no file headers.
C
	IF  ( num .eq. 0 )  RETURN
C
C*	Read file names and convert from integers.
C
	CALL DM_RCH4  ( iflno, iread, num, kfhnam (1, iflno), iret )
	IF  ( iret .ne. 0 )  RETURN
	iread = iread + num
C
C*	Read the file header lengths and data types from the file.
C
	CALL DM_RINT  ( iflno, iread, num, kfhlen (1, iflno), iret )
	IF  ( iret .ne. 0 )  RETURN
	iread = iread + num
C
	CALL DM_RINT  ( iflno, iread, num, kfhtyp (1, iflno), iret )
	IF  ( iret .ne. 0 )  RETURN
C*
	RETURN
	END
