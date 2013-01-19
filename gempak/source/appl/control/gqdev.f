	SUBROUTINE GQDEV  ( device, iunit, iatyp, iret )
C************************************************************************
C* GQDEV								*
C* 									*
C* This subroutine returns the current plot device identifier, unit	*
C* number and access type.  If no device is set, a blank is returned. 	*
C*									*
C* GQDEV  ( DEVICE, IUNIT, IATYP, IRET )				*
C*									*
C* Output parameters:							*
C* 	DEVICE		CHAR*		Plot device name		*
C* 	IUNIT		INTEGER		Not used			*
C*	IATYP		INTEGER 	Device access type		*
C*					   1 = direct access		*
C*					   2 = sequential access	*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
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
	isend (2) = FQDEV
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
	    CALL ST_ITOS  ( ircv (2), 3, nc, device, ier )
	  ELSE
	    iret = ierr
	END IF
C*
	RETURN
	END
