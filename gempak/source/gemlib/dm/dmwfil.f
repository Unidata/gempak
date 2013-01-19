	SUBROUTINE DM_WFIL  ( iflno, iret )
C************************************************************************
C* DM_WFIL								*
C*									*
C* This subroutine writes file header information to a DM file.		*
C*									*
C* DM_WFIL  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C------------------------------------------------------------------------
	iret   = 0
	iwrite = kpfile ( iflno )
	num    = kfhdrs ( iflno )
C
C*	Return if there are no file headers.
C
	IF  ( num .eq. 0 )  RETURN
C
C*	Write file header names to file.
C
	CALL DM_WCH4  ( iflno, iwrite, num, kfhnam (1, iflno), iret )
	IF  ( iret .ne. 0 )  RETURN
	iwrite = iwrite + num
C
C*	Write the file header lengths and data types to the file.
C
	CALL DM_WINT  ( iflno, iwrite, num, kfhlen (1, iflno), iret )
	IF  ( iret .ne. 0 )  RETURN
	iwrite = iwrite + num
C
	CALL DM_WINT  ( iflno, iwrite, num, kfhtyp (1, iflno), iret )
	IF  ( iret .ne. 0 )  RETURN
C*
	RETURN
	END
