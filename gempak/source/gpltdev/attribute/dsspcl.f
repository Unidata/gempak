	SUBROUTINE DSSPCL ( szspcl, ispwid, size, jspwid, iret )
C************************************************************************
C* DSSPCL								*
C*									*
C* This subroutine sets the special symbol parameters.  If these	*
C* parameters are not positive, no changes are made.			*
C*									*
C* DSSPCL ( SZSPCL, ISPWID, SIZE, JSPWID, IRET )			*
C*									*
C* Input parameters:							*
C*	SZSPCL		REAL		Special symbol size		*
C*	ISPWID		INTEGER		Special symbol line width	*
C*									*
C* Output parameters:							*
C*	SIZE		REAL		Special symbol size		*
C*	JSPWID		INTEGER		Special symbol line width	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 8/96	Based on DSWTHR				*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 4
	isend (2) = CSSPCL
C
	CALL GPUT ( isend, 2, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( szspcl, 1, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( ispwid, 1, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ier )
	IF  ( ier .ne. NORMAL ) iret = ier
C
	CALL GGETR ( size, 1, ier )
	IF  ( ier .ne. NORMAL ) THEN
	    iret = ier
	ELSE
C
C*	    Save symbol size in the ACTIVE common block.
C
	    tsprsz = size
	END IF
C
	CALL GGET ( jspwid, 1, ier )
	IF  ( ier .ne. NORMAL ) THEN
	    iret = ier
	  ELSE
C
C*	    Save symbol line width in the ACTIVE common block.
C
	    mspwid = jspwid
	END IF
C*
	RETURN
	END
