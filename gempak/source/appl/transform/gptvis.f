	SUBROUTINE GPTVIS ( sys, np, x, y, vis, iret )
C************************************************************************
C* GPTVIS								*
C*									*
C* This subroutine returns a logical array indicating whether the 	*
C* input points are within the bounds of the map/graph to be plotted.	*
C*									*
C* GPTVIS ( SYS, NP, X, Y, VIS, IRET )					*
C*									*
C* Input parameters:							*
C*	SYS		CHAR*		Coordinate system		*
C*                                        'S' = screen coordinates      *
C*					  'D' = device coordinates	*
C*					  'N' = normalized coords.	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'M' = map coordinates		*
C*					  'G' = grid coordinates	*
C*	NP		INTEGER		Number of points		*
C*	X (NP)		REAL		X coordinates / latitudes	*
C*	Y (NP)		REAL		Y coordinates / longitudes      *
C*									*
C* Output parameters:							*
C*	VIS (NP)	LOGICAL		Visible flags			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Williams/EAi	 3/94	Removed blank comments from header	*
C* M. Linda/GSC		 3/96	Changed check for GPLT buffer overflow	*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	sys
	REAL		x (*), y (*)
	LOGICAL 	vis (*)
C
	INTEGER 	isend (4)
C------------------------------------------------------------------------
C
C*	Check if GPLT buffer will overflow.
C
	isnd = 2 + ( 2 + ( 2 * np ) )
	ircv = 1 + ( np + 1 )
	IF ( ( isnd + ircv ) .gt. IGBSIZ ) THEN
	    iret = NOBUFF
	    RETURN
	END IF
C
C*	Check validity of the coordinate system.
C
	isys = INDEX ( syslo, sys ) + INDEX ( sysup, sys )
	IF ( isys .eq. 0 ) THEN
	    iret = NOCORD
	    RETURN
	END IF
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = isnd
	isend (2) = FPTVIS
	isend (3) = isys
	isend (4) = np
C
	CALL GPUT ( isend, 4, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( x, np, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( y, np, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ierr )
	IF ( ierr .ne. NORMAL ) THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGETR ( vis, np, ierr )
	IF ( ierr .ne. NORMAL ) iret = ierr
C*
	RETURN
	END
