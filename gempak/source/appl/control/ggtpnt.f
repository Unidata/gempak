	SUBROUTINE GGTPNT  ( sys, ityp, x, y, iret )
C************************************************************************
C* GGTPNT								*
C*									*
C* This subroutine returns the requested number of points from the 	*
C* cursor position when the mouse button is pressed.			*
C*									*
C* GGTPNT  ( SYS, ITYP, X, Y, IRET )					*
C*									*
C* Input parameters:							*
C*	SYS		CHAR*           Coordinate system		*
C*                                        'S' = screen coordinates      *
C*					  'D' = device coordinates	*
C*					  'N' = normalized coordinates	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'M' = map coordinates		*
C*					  'G' = grid coordinates	*
C*	ITYP		INTEGER		Type of cursor			*
C*					   1 = point, NP = 1		*
C*					   2 = line,  NP = 2		*
C*					   3 = box,   NP = 2		*
C*									*
C* Output parameters:							*
C*	X (NP)		REAL		X coordinates / latitudes	*
C*	Y (NP)		REAL		Y coordinates / longitudes	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Chou/EAI		 6/93						*
C* S. Jacobs/EAI	 6/93	Clean up				*
C* S. Jacobs/EAI	 9/93	Added ITYP				*
C* L. Williams/EAi	 3/94	Removed blank comments from		*
C* S. Schotz/NCEP	 7/97	Update documentation for S coord header	*
C* A. Hardy/GSC		 5/98	Corrected prolog                	*
C* S. Jacobs/NCEP	 6/98	Removed NP from calling sequence	*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	sys
	REAL		x ( * ), y ( * )    
C*
	INTEGER		isend ( 4 )
C------------------------------------------------------------------------
C*	Check the validity of the coordinate system.
C
	isys = INDEX ( syslo, sys ) + INDEX ( sysup, sys )
	IF  ( isys .eq. 0 )  THEN
	    iret = NOCORD
	    RETURN
	END IF
C
C*      Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 4
	isend (2) = FGTPNT
	isend (3) = isys
	isend (4) = ityp
C
	CALL GPUT  ( isend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*      Get output parameters.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
C*	Get cursor point(s).
C
	IF  ( ityp .eq. 1 )  THEN
	    np = 1
	  ELSE
	    np = 2
	END IF
C
	CALL GGETR  ( x, np, ierr )
	IF  ( ierr .eq. NORMAL )  CALL GGETR  ( y, np, ierr )
C
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
