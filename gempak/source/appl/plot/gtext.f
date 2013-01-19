	SUBROUTINE GTEXT ( sys, x, y, cchar, rotat, ixoff, iyoff, iret )
C************************************************************************
C* GTEXT								*
C*									*
C* This subroutine plots a text string in any coordinate system.  The   *
C* reference point (X,Y) determines the center of the first character.  *
C* The text string may be rotated from horizontal at the reference      *
C* point and offset along the rotated X and Y coordinates.  Positive X  *
C* offsets are toward the right; positive Y offsets are toward the top. *
C* The text is drawn using attributes defined by GSTEXT.		*
C*									*
C* The text string may contain carriage returns and/or line feed        *
C* characters.  A carriage return/line feed terminates one line of      *
C* text and begins a new line.  Each new line is offset normal to the   *
C* rotation from the previous line and oriented along the direction of  *
C* rotation.  All resulting lines of text are placed so that they are   *
C* centered normal to the rotation.                                     *
C*									*
C* GTEXT ( SYS, X, Y, CCHAR, ROTAT, IXOFF, IYOFF, IRET )		*
C*									*
C* Input parameters:							*
C*	SYS		CHAR*		Coordinate system		*
C*                                        'S' = screen coordinates      *
C*					  'D' = device coordinates	*
C*					  'N' = normalized coordinates	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'M' = map coordinates		*
C*					  'G' = grid coordinates	*
C*	X		REAL		X coordinate / latitude		*
C*	Y		REAL		Y coordinate / longitude	*
C*	CCHAR		CHAR*		Text string to plot		*
C*	ROTAT		REAL		Rotation angle in degrees	*
C*	IXOFF		INTEGER		X offset in half characters	*
C*	IYOFF		INTEGER		Y offset in half characters	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	 7/84	GEMPLT Version 3.0			*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* L. Williams/EAi	 3/94	Removed blank comments from header	*
C* S. Jacobs/NCEP	11/96	Added check for 0 length for cchar	*
C* M. Linda/GSC		 1/97	Truncate long strings to avoid overflow	*
C* S. Jacobs/NCEP	 7/97	Updated documentation			*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C* J. Wu/GSC	         8/00	Replaced 'CHLF' with 'CHCR' for Linux	* 
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	cchar, sys
C
	INTEGER		ichar (100), isend (3)
	REAL		rsend (2)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check validity of the coordinate system.
C
	isys = INDEX ( syslo, sys ) + INDEX ( sysup, sys )
	IF  ( isys .eq. 0 ) THEN
	    iret = NOCORD
	    RETURN
	END IF
C
C*	Find the length of the text string in characters and words.
C
	CALL ST_LSTR ( cchar, lenc, ierr )
	IF ( lenc .eq.   0 ) RETURN
	IF ( lenc .gt. 400 ) lenc = 400
	lenw = ( lenc - 1 ) / 4 + 1
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = lenw + 8
	isend (2) = FTEXT
	isend (3) = isys
C
	CALL GPUT ( isend, 3, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	rsend (1) = x
	rsend (2) = y
C
	CALL GPUTR ( rsend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Convert the character string to an integer array and send it.
C
	DO ich = 1, lenc
	   IF ( cchar (ich:ich) .eq. CHLF )  cchar (ich:ich) = CHCR
	END DO

	CALL ST_STOI ( cchar, lenc, nv, ichar, iret )
	CALL GPUT ( ichar, lenw, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Send the rotation angle.
C
	CALL GPUTR ( rotat, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	isend (1) = ixoff
	isend (2) = iyoff
C
	CALL GPUT ( isend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ierr )
	IF ( ierr .ne. NORMAL ) iret = ierr
C*
	RETURN
	END
