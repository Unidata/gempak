	SUBROUTINE DSHASH ( szhsh, ihwid, ilwid, size, 
     +                      jhwid, jlwid, iret )
C************************************************************************
C* DSHASH								*
C* 									*
C* This subroutine sets the hash mark size, line width, and line 	*
C* spacing.  If these parameters are not positive no changes are made.	*
C* 									*
C* DSHASH  ( SZHSH, IHWID, ILWID, SIZE, JHWID, JLWID, IRET )		*
C*                                                                    	*
C* Input parameters:							*
C* 	SZHSH		REAL		Hash mark size multiplier	*
C* 				   	  <=0 = no change		*
C*      IHWID		INTEGER		Hash mark line width multiplier	*
C*					  <=0 = no change		*
C* 	ILWID		INTEGER		Hash mark line spacing		*
C*					  <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Actual size			*
C* 	JHWID		INTEGER		Hash mark line width		*
C* 	JLWID		INTEGER		Hash mark line spacing		*
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
	isend (1) = 5
	isend (2) = CSHASH
	CALL GPUT  ( isend,  2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szhsh, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	isend (1) = ihwid
	isend (2) = ilwid
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
C*	    Save the ACTIVE hash mark size common block variable.
C
	    thshsz = size
	END IF
C
	CALL GGET  ( ircv, 2, ier )
	IF  ( ier .ne. NORMAL ) THEN
	    iret = ier
	ELSE
C
C*	    Save the ACTIVE hash mark line width and spacing 
C*	    in commmon block and output parameters.
C
	    jhwid  = ircv (1)
	    jlwid  = ircv (2)
	    mhwid  = jhwid
	    mlwidh = jlwid
	END IF
C*
	RETURN
	END
