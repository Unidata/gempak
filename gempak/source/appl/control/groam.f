	SUBROUTINE GROAM  ( ityp, sys, x, y, iret )
C************************************************************************
C* GROAM								*
C* 									*
C* This subroutine moves the current roam window to the specified       *
C* position in any coordinate system except 'S'.  The base point of the *
C* roam can be the upper left of the screen or the center of the screen.*
C*									*
C* GROAM  ( ITYP, SYS, X, Y, IRET )					*
C*									*
C* Input parameters:							*
C* 	ITYP		INTEGER		The base point of roam 		*
C*                                        0 = upper left screen corner  *
C*                                        1 = center of the screen 	*
C*                                        2 = delta_x, delta_y		*
C*      SYS             CHAR*           Coordinate system               *
C*                                        'S' = screen coordinates      *
C*                                        'D' = device coordinates      *
C*                                        'N' = normalized coordinates  *
C*                                        'V' = view coordinates        *
C*                                        'P' = plot coordinates        *
C*                                        'M' = map coordinates         *
C*                                        'G' = grid coordinates        *
C* 	X		REAL		Upper left x coordinate 	*
C* 	Y		REAL		Upper left y coordinate 	*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI	 	6/97						*
C* A. Hardy/GSC         6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER*(*)   sys
C*
	INTEGER		isend (4)
	REAL		rsend (2)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*      Check validity of the coordinate system.
C
        isys = INDEX ( syslo, sys ) + INDEX ( sysup, sys )
        IF  ( isys .eq. 0 ) THEN
            iret = NOCORD
            RETURN
        END IF
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = FROAM
	isend (3) = ityp
	isend (4) = isys
C
	CALL GPUT  ( isend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	rsend (1) = x
	rsend (2) = y
C
	CALL GPUTR  ( rsend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
        CALL GGET  ( iret, 1, ierr )
        IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
