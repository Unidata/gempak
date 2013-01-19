	SUBROUTINE DSTURB ( szturb, ituwid, size, jtuwid, iret )
C************************************************************************
C* DSTURB								*
C*									*
C* This subroutine sets the turbulence symbol parameters.  If these	*
C* parameters are not positive, no changes are made.			*
C*									*
C* DSTURB ( SZTURB, ITUWID, SIZE, JTUWID, IRET )			*
C*									*
C* Input parameters:							*
C*	SZTURB		REAL		Turbulence symbol size		*
C*	ITUWID		INTEGER		Turbulence symbol line width	*
C*									*
C* Output parameters:							*
C*	SIZE		REAL		Turbulence symbol size		*
C*	JTUWID		INTEGER		Turbulence symbol line width	*
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
	isend (2) = CSTURB
C
	CALL GPUT ( isend, 2, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( szturb, 1, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( ituwid, 1, iret )
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
	    ttursz = size
	END IF
C
	CALL GGET ( jtuwid, 1, ier )
	IF  ( ier .ne. NORMAL ) THEN
	    iret = ier
	  ELSE
C
C*	    Save symbol line width in the ACTIVE common block.
C
	    mtuwid = jtuwid
	END IF
C*
	RETURN
	END
