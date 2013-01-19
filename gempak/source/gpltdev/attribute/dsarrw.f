	SUBROUTINE DSARRW ( szarrw, szarrh, iarwid, iartyp, size, 
     +                      sizehd, jarwid, jartyp, iret )
C************************************************************************
C* DSARRW								*
C* 									*
C* This subroutine sets the wind arrow size, arrow head size, width 	*
C* multipliers and the arrow type.  If these parameters are not 	*
C* positive no changes are made. 					*
C* 									*
C* DSARRW  ( SZARRW, SZARRH, IARWID, IARTYP, SIZE, SIZEHD, JARWID, 	*
C* JARTYP, IRET )							*
C*                                                                    	*
C* Input parameters:							*
C* 	SZARRW		REAL		Arrow size multiplier		*
C* 				   	  <=0 = no change		*
C*      SZARRH		REAL		Arrow head size multiplier	*
C*					  <=0 = no change		*
C*      IARWID		INTEGER		Arrow width multiplier		*
C*					  <=0 = no change		*
C* 	IARTYP		INTEGER		Arrow type			*
C*					  <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Actual size			*
C*	SIZEHD		REAL		Actual head size		*
C* 	JARWID		INTEGER		Arrow width			*
C* 	JARTYP		INTEGER		Arrow type			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/89	Documentation				*
C* S. Schotz/GSC	 1/90	Added arrow width and type		*
C* S. Schotz/GSC	 8/90	Added arrow head size			*
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
	isend (2) = CSARRW
	CALL GPUT  ( isend,  2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szarrw, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szarrh, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	isend (1) = iarwid
	isend (2) = iartyp
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
	    twasz = size
	END IF
C*
	CALL GGETR  ( sizehd, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	  ELSE
C
C*	    Save the ACTIVE wind arrow head size common block variable.
C
	    twahsz = sizehd
	END IF
C
	CALL GGET  ( ircv, 2, ier )
	IF  ( ier .ne. NORMAL ) THEN
	    iret = ier
	ELSE
C
C*	    Save the AVTIVE wind arrow width and type in commmon block 
C*	    and output parameters
C
	    jarwid = ircv (1)
	    jartyp = ircv (2)
	    marwid = jarwid
	    martyp = jartyp
	END IF
C*
	RETURN
	END
