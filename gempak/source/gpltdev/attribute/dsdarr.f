	SUBROUTINE DSDARR ( szdarw, szdarh, idarwd, idartp, size, 
     +                      sizehd, jdarwd, jdartp, iret )
C************************************************************************
C* DSDARR								*
C* 									*
C* This subroutine sets the directional arrow size, arrow head size,  	*
C* width multipliers and the arrow type.  If these parameters are not 	*
C* positive no changes are made. 					*
C* 									*
C* DSDARR  ( SZDARW, SZDARH, IDARWD, IDARTP, SIZE, SIZEHD, JDARWD, 	*
C* JDARTP, IRET )							*
C*                                                                    	*
C* Input parameters:							*
C* 	SZDARW		REAL		Arrow size multiplier		*
C* 				   	  <=0 = no change		*
C*      SZDARH		REAL		Arrow head size multiplier	*
C*					  <=0 = no change		*
C*      IDARWD		INTEGER		Arrow width multiplier		*
C*					  <=0 = no change		*
C* 	IDARTP		INTEGER		Arrow type			*
C*					  <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Actual size			*
C*	SIZEHD		REAL		Actual head size		*
C* 	JDARWD		INTEGER		Arrow width			*
C* 	JDARTP		INTEGER		Arrow type			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	03/98						*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER 	isend (3), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = CSDARR
	CALL GPUT  ( isend,  2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szdarw, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szdarh, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	isend (1) = idarwd
	isend (2) = idartp
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	CALL GGETR  ( size, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	ELSE
C
C*	    Save the ACTIVE wind arrow size common block variable.
C
	    twdasz = size
	END IF
C*
	CALL GGETR  ( sizehd, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	  ELSE
C
C*	    Save the ACTIVE wind arrow head size common block variable.
C
	    tdahsz = sizehd
	END IF
C
	CALL GGET  ( ircv, 2, ier )
	IF  ( ier .ne. NORMAL ) THEN
	    iret = ier
	ELSE
C
C*	    Save the ACTIVE wind arrow width and type in commmon block 
C*	    and output parameters
C
	    jdarwd = ircv (1)
	    jdartp = ircv (2)
	    mdarwd = jdarwd
	    mdartp = jdartp
	END IF
C*
	RETURN
	END
