	SUBROUTINE DTXSY ( iwndw, itype, isym, ijust, ixoff, iyoff, rotn,
     +		x, y, cchar, lens, iret )
C************************************************************************
C* DTXSY								*
C*									*
C* This subroutine plots a special text and symbol to any coordinate    *
C* system.  The special text is centered on the given reference point   *
C* (X,Y).  The text is drawn using attributes defined by GSTEXT, and    *
C* the surrounding box is drawn using attributes defined by GSLINE.     *
C* Depending upon special text type, the box may be filled.             *
C*									*
C* DTXSY ( IWNDW, ITYPE, ISYM, IJUST, IXOFF, IYOFF, ROTN, X, Y,         *
C*				CCHAR, LENS, IRET )			*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*      ITYPE           INTEGER         Special Text type               *
C*                                          1 = low pressure box        *
C*                                          2 = high pressure box       *
C*                                          3 = pressure box            *
C*                                                  bounded, unfilled   *
C*                                          4 = pressure box            *
C*                                                  bounded, filled     *
C*                                          5 = pressure box,           *
C*                                                  unbounded, filled   *
C*                                          6 = freezing level symbol   *
C*                                          7 = turbulence symbol       *
C*					    8 = cloud level		*
C*					    9 = high level turbulence	*
C*					   10 = underline		*
C*					   11 = underline, fill box	*
C*					   12 = midleve icing		*
C*					   13 = overline		*
C*					   14 = overline, fill box	*
C*					   15 = "Big Box" for mid-level	*
C*	ISYM		INTEGER		Turbulence Symbol indicator	*
C*					    One or two digits in range  *
C*					     0-8.  No meaning if text   *
C*					    type is 1-6.		*
C*	IJUST		INTEGER		Justification (-1, 0, 1)	*
C*	IXOFF		INTEGER		X offset			*
C*	IYOFF		INTEGER		Y offset			*
C*	ROTN		REAL		Rotation			*
C*	X		REAL		X coordinate in device units	*
C*	Y		REAL		Y coordinate in device units	*
C*	CCHAR		CHAR*		Text string to plot		*
C*	LENS		INTEGER		Length of string		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Safford/GSC	 4/97   Initial coding -- copied from GTEXTC    *
C* E. Safford/GSC	 5/97   Renamed sym to isym                     *
C* E. Safford/GSC	 7/97   Added ijust, ixoff, iyoff, rotn, 	*
C* S. Jacobs/NCEP	 9/00	Replaced 'CHLF' with 'CHCR' for Linux	*
C* M. Li/SAIC		11/01	Added type 12 for icing			*
C* T. Lee/SAIC		 8/02	Added overline to prolog		*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	cchar
C
	INTEGER		ichar (100), isend (8)
	REAL		rsend (3)
C------------------------------------------------------------------------
C*	Find the length of the text string in characters and words.
C
	lenc = lens
	IF ( lenc .gt. 400 ) lenc = 400
	lenw = ( lenc - 1 ) / 4 + 1
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = lenw + 12
	isend (2) = CTXSY
	isend (3) = iwndw
	isend (4) = itype
	isend (5) = isym
	isend (6) = ijust
	isend (7) = ixoff
	isend (8) = iyoff
C
	CALL GPUT ( isend, 8, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	rsend (1) = rotn
	rsend (2) = x
	rsend (3) = y
C
	CALL GPUTR ( rsend, 3, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( lenc, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Convert the character string to an integer array and send it.
C
	DO  ich = 1, lenc
	    IF ( cchar (ich:ich) .eq. CHLF )  cchar (ich:ich) = CHCR
	END DO
C
	CALL ST_STOI ( cchar, lenc, nv, ichar, iret )
	CALL GPUT ( ichar, lenw, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ierr )
	IF ( ierr .ne. NORMAL ) iret = ierr
C*
	RETURN
	END
