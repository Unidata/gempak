	SUBROUTINE GTEXTC ( sys, x, y, cchar, rotat, ixoff, iyoff,
     +			    iret )
C************************************************************************
C* GTEXTC								*
C*									*
C* This subroutine plots a text string in any coordinate system.  Text	*
C* may be plotted in the margins, that is no clipping is done. The text	*
C* string may be rotated from horizontal at the reference point and	*
C* offset along the rotated X and Y coordinates.  Positive X offsets	*
C* are toward the right;  positive Y offsets are toward the top.  The	*
C* text is drawn using attributes defined by GSTEXT.			*
C*									*
C* The text string may contain carriage returns and/or line feed        *
C* characters.  A carriage return/line feed terminates one line of      *
C* text and begins a new line.  Each new line is offset normal to the   *
C* rotation from the previous line and oriented along the direction     *
C* of rotation.  All resulting lines of text are placed so that they    *
C* are centered normal to the rotation.                                 *
C*									*
C* GTEXTC ( SYS, X, Y, CCHAR, ROTAT, IXOFF, IYOFF, IRET )		*
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
C* A. Chang/EAI		 8/94	Modified from GTEXT			*
C* S. Jacobs/NCEP	11/96	Added check for 0 length for cchar	*
C* M. Linda/GSC		 1/97	Truncate long strings to avoid overflow	*
C* E. Safford/GSC	 5/97	Added documentation on multi-line text  *
C* S. Jacobs/NCEP	 7/97	Updated documentation			*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C* S. Jacobs/NCEP	 7/98	Removed ITYPE from calling sequence	*
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
	isend (2) = FTEXTC
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
