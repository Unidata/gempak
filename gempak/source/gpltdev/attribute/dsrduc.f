	SUBROUTINE DSRDUC ( filter, rfilt, iret )
C************************************************************************
C* DSRDUC								*
C*									*
C* This subroutine sets the filter factor for the point reduction	*
C* scheme.								*
C*									*
C* DSRDUC ( FILTER, RFILT, IRET )					*
C*									*
C* Input parameters:							*
C*	FILTER		REAL		Filter factor for pnt reduction	*
C*									*
C* Output parameters:							*
C*	RFILT		REAL		Filter factor for pnt reduction	*
C*	IRET		INTEGER		Return code			*
C**									*
C* S. Jacobs/NCEP	 5/99						*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 3
	isend (2) = CSRDUC
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GPUTR  ( filter, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( ircv, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGETR  ( rfilt, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Fill output parameters.
C
	iret   = ircv
C
C*	Fill the ACTIVE common block variables
C
	trfilt = rfilt
C*
	RETURN
	END
