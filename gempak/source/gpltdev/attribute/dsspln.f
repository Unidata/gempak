	SUBROUTINE DSSPLN  ( isltyp, islstr, isldir, slsiz, islwid, 
     +			     jsltyp, jslstr, jsldir, size, jslwid, iret )
C************************************************************************
C* DSSPLN								*
C*									*
C* This subroutine sets special line attributes including the special	*
C* line type number, stroke multiplier, direction indicator, size, and	*
C* width multiplier.							*
C*									*
C* DSSPLN  ( ISLTYP, ISLSTR, ISLDIR, SLSIZ, ISLWID,			*
C*	     JSLTYP, JSLSTR, JSLDIR, SIZE,  JSLWID, IRET )		*
C*									*
C* Input parameters:							*
C*	ISLTYP		INTEGER		Special line type		*
C*	ISLSTR		INTEGER		Special line stroke multiplier	*
C*					   0 = no change		*
C*	ISLDIR		INTEGER		Special line direction indicator*
C*					   1 = up or out		*
C*					   0 = no change		*
C*					  -1 = down or in		*
C*	SLSIZ		REAL		Special line size		*
C*					   0 = no change		*
C*	ISLWID		INTEGER		Special line width multiplier	*
C*					   0 = no change		*
C*									*
C* Output parameters:							*
C*	JSLTYP		INTEGER		Special line type		*
C*	JSLSTR		INTEGER		Special line stroke multiplier	*
C*	JSLDIR		INTEGER		Special line direction indicator*
C*	SIZE		REAL		Special line size		*
C*	JSLWID		INTEGER		Special line width multiplier	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Keiser/GSC	 3/97		Copied from DSLINE		*
C************************************************************************
	INCLUDE 'FUNCCODE.PRM'
	INCLUDE 'ERROR.PRM'
	INCLUDE 'DEVACT.CMN'
C*
	INTEGER isend (5), ircv (3)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 7
	isend (2) = CSSPLN
	isend (3) = isltyp
	isend (4) = islstr
	isend (5) = isldir
C
	CALL GPUT  ( isend, 5, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( slsiz, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( islwid, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	  ELSE
C
C*	    Get the output variables.
C
	    CALL GGET  ( ircv, 3, iret ) 
	    IF  ( iret .ne. NORMAL )  RETURN
C
	    jsltyp = ircv (1)
	    jslstr = ircv (2)
	    jsldir = ircv (3)
C
	    CALL GGETR  ( size, 1, iret )
	    IF  ( iret .ne. NORMAL )  RETURN
C
	    CALL GGET  ( jslwid, 1, iret )
	    IF  ( iret .ne. NORMAL )  RETURN
C
C*	    Set the ACTIVE common block variables.
C
	    msltyp = jsltyp
	    mslstr = jslstr
	    msldir = jsldir
	    tslsiz = size
	    mslwid = jslwid

	END IF
C*
	RETURN
	END
