	SUBROUTINE GLOGO ( sys, x, y, size, iclmod, ilogo, iret )
C************************************************************************
C* GLOGO								*
C*									*
C* This subroutine draws a specified emblem in any coordinate system    *
C* such as the NOAA seagull emblem.					*
C*									*
C* GLOGO ( SYS, X, Y, SIZE, ICLMOD, ILOGO, IRET )				*
C*									*
C* Input parameters:							*
C*	SYS		CHAR*		Coordinate system		*
C*					  'S' = screen coordinates	*
C*					  'D' = device coordinates	*
C*					  'N' = normalized coordinates	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'M' = map coordinates		*
C*					  'G' = grid coordinates	*
C*	X		REAL		X coordinate / latitude	        *
C*	Y		REAL		Y coordinate / longitude	*
C*	SIZE		REAL		Emblem size			*
C*	ICLMOD		INTEGER		Emblem color mode		*
C*					  '1' = monochrome		*
C*					  '2' = color			*
C*	ILOGO		INTEGER		Emblem ID			*
C*					  '1' = NOAA			*
C*					  '2' = NWS			*
C*					  '3' = NOAA w/o text		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 7/97	Original				*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C* A. Hardy/GSC          5/00   Added emblem color mode			*
C* J. Wu/GSC		 3/01   Added emblem ID 			*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sys
C
	INTEGER		isend ( 3 )
	REAL		rsend ( 3 )
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
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend ( 1 ) = 8
	isend ( 2 ) = FLOGO
	isend ( 3 ) = isys
C
	CALL GPUT ( isend, 3, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	rsend ( 1 ) = x
	rsend ( 2 ) = y
	rsend ( 3 ) = size
C
	CALL GPUTR ( rsend, 3, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( iclmod, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( ilogo, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ierr )
	IF ( ierr .ne. NORMAL ) iret = ierr
C*
	RETURN
	END
