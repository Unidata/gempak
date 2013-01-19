	SUBROUTINE DSICNG ( szicng, icewid, size, jcewid, iret )
C************************************************************************
C* DSICNG								*
C*									*
C* This subroutine sets the icing symbol parameters.  If these		*
C* parameters are not positive, no changes are made.			*
C*									*
C* DSICNG ( SZICNG, ICEWID, SIZE, JCEWID, IRET )			*
C*									*
C* Input parameters:							*
C*	SZICNG		REAL		Icing symbol size		*
C*	ICEWID		INTEGER		Icing symbol line width		*
C*									*
C* Output parameters:							*
C*	SIZE		REAL		Icing symbol size		*
C*	JCEWID		INTEGER		Icing symbol line width		*
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
	isend (2) = CSICNG
C
	CALL GPUT ( isend, 2, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( szicng, 1, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( icewid, 1, iret )
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
	    tcersz = size
	END IF
C
	CALL GGET ( jcewid, 1, ier )
	IF  ( ier .ne. NORMAL ) THEN
	    iret = ier
	  ELSE
C
C*	    Save symbol line width in the ACTIVE common block.
C
	    mcewid = jcewid
	END IF
C*
	RETURN
	END
