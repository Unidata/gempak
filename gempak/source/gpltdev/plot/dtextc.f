	SUBROUTINE DTEXTC ( iwndw, x, y, cchar, lens, rotat,
     +			    ixoff, iyoff, iret )
C************************************************************************
C* DTEXTC								*
C*									*
C* This subroutine plots a text string in any coordinate system.  Text  *
C* may be plotted in the margins, that is no clipping is done. The text *
C* string may be rotated from horizontal at the reference point and     *
C* offset along the rotated X and Y coordinates.  Positive X offsets    *
C* are toward the right;  positive Y offsets are toward the top.  The   *
C* text is drawn using attributes defined by GSTEXT.                    *
C*									*
C* The text string may contain carriage returns and/or line feed        *
C* characters.  A carriage return/line feed will terminate one line of  *
C* text and begin a new line.  Each new line will be offset normal to   *
C* the rotation from the previous line and oriented along the direction *
C* of rotation.  All resulting lines of text will be placed so that     *
C* they are centered normal to the rotation.                            *
C*									*
C* DTEXTC ( IWNDW, X, Y, CCHAR, LENS, ROTAT, IXOFF, IYOFF, IRET )	*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	X		REAL		X coordinate in device units	*
C*	Y		REAL		Y coordinate in device units	*
C*	CCHAR		CHAR*		Text string to plot		*
C*	LENS		INTEGER		Length of string		*
C*	ROTAT		REAL		Rotation angle in degrees	*
C*	IXOFF		INTEGER		X offset in half characters	*
C*	IYOFF		INTEGER		Y offset in half characters	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI		 8/94	Modified from DTEXT			*
C* M. Linda/GSC		 1/97	Changed X and Y to reals, check lens	*
C* E. Safford/GSC	 5/97   Added documentation on multi-line text  *
C* S. Jacobs/NCEP	 7/97	Updated documentation			*
C* S. Jacobs/NCEP	 7/98	Removed ITYPE from calling sequence	*
C* J. Wu/GSC	         8/00	Replaced 'CHLF' with 'CHCR' for Linux	* 
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	cchar
C
	INTEGER		ichar (100), isend (3)
	REAL		rsend (2)
C------------------------------------------------------------------------
C*	Find the length of the text string in characters and words.
C
	lenc = lens
	IF ( lenc .gt. 400 ) lenc = 400
	lenw = ( lenc - 1 ) / 4 + 1
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = lenw + 9
	isend (2) = CTEXTC
	isend (3) = iwndw
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
	CALL GPUT ( lenc, 1, iret )
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
