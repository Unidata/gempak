	SUBROUTINE DEVENT ( check, iret )
C************************************************************************
C* DEVENT								*
C*									*
C* This subroutine receives messages from GPLT and handles events that	*
C* are raised within the device driver.  This subroutine loops,		*
C* handling events, as long as there are no messages from GPLT.  As	*
C* soon as a message comes in, the message is read into an internal	*
C* buffer and this subroutine exits.					*
C*									*
C* DEVENT ( CHECK, IRET )						*
C*									*
C* Input parameters:							*
C*	CHECK		LOGICAL		Check/do not check for events	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 3/96	Redesigned				*
C* T. Piper/SAIC	10/04	Modified for change to SS_WAIT          *
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
C*
	LOGICAL		check
C
C------------------------------------------------------------------------
C
C*	Is device event-capable and are events to be checked?
C
	IF ( evtflg .and. check ) THEN
C
C*	    Handle events while there are events and no messages from GPLT.
C
	    iret = -1
C
	    DO WHILE ( iret .ne. 0 )
C
C*		Check for a message from GPLT, but do not wait for one.
C
		CALL GGETC ( -1, iret )
C
		IF  ( iret .ne. 0 )  THEN
C
C*		    No message, so pause and handle events from device.
C
		    CALL SS_WAIT ( 0.1, ier )
		    CALL HEVENT ( ier )
		END IF
	    END DO
	ELSE
C
C*	    Events not to be checked, so wait for next message from GPLT.
C
	    CALL GGETC ( 0, iret )
	END IF
C*
	RETURN
	END
