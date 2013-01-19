	SUBROUTINE GIPROC  ( mproc, mbchan, iret )
C************************************************************************
C* GIPROC								*
C*									*
C* This subroutine is called by subprocesses to create a communications	*
C* channel between the subprocess and the calling process.  The 	*
C* subprocess is created using GSPROC.					*
C*									*
C* GIPROC  ( MPROC, MBCHAN, IRET )					*
C*									*
C* Input parameters:							*
C*	MPROC		INTEGER		Subprocess type			*
C*					  0 = gplt			*
C*					  1 = device driver		*
C*									*
C* Output parameters:							*
C*	MBCHAN		INTEGER		Message queue number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 4/82						*
C* M. desJardins/GSFC	 4/85						*
C* M. desJardins/NMC	 1/92	Make VMS & UNIX calls identical		*
C************************************************************************
	INTEGER		ciproc
C------------------------------------------------------------------------
C*	Call the C subroutine.
C
	ier = ciproc  ( mproc, mbchan, iret )
C*
	RETURN
	END
