	SUBROUTINE DQDEV  ( device, iunit, iatyp, iret )
C************************************************************************
C* DQDEV								*
C* 									*
C* This subroutine returns the current plot device identifier, unit	*
C* number and access type. If no device is set, a blank is returned. 	*
C* DEVICE has traditionally been a 2-character name, but may now	*
C* contain up to 12 characters.						*
C*									*
C* DQDEV  ( DEVICE, IUNIT, IATYP, IRET )				*
C*									*
C* Output parameters:							*
C* 									*
C* 	DEVICE		CHAR*		Plot device name		*
C* 	IUNIT		INTEGER		Not used			*
C*	IATYP		INTEGER 	Device access type		*
C*					   1 = direct access		*
C*					   2 = sequential access	*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	 7/91	New; needed for UNIX			*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	CHARACTER*(*)	device
C*
	INTEGER		isend (2), ircv (6), idev (3)
C*
	EQUIVALENCE	( ircv (2), idev (1) )
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = CQDEV
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C*
	CALL GGET  ( ircv, 6, ierr )
	IF  ( ierr .eq. NORMAL )  THEN
	    iret   = ircv (1)
	    iunit  = ircv (5)
	    iatyp  = ircv (6)
C
C*	    Convert IDEV into a character.
C
	    CALL ST_ITOS  ( idev, 3, nc, device, ierr )
	  ELSE
	    iret = ierr
	END IF
C*
	RETURN
	END
