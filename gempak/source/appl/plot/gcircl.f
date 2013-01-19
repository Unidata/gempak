	SUBROUTINE GCIRCL  ( sys, xpt, ypt, xcrm, ycrm, np, iret )
C************************************************************************
C* GCIRCL								*
C*									*
C* This subroutine draws a circle centered at a point which may be      *
C* defined in any coordinate system.  The radius of the circle is 	*
C* defined by the points xcrm and ycrm on the circumference of the      *
C* circle.  NP is the number of points to be used in drawing the        *
C* circle.  If NP is zero, 10 points are used.				*
C*									*
C* GCIRCL  ( SYS, XPT, YPT, XCRM, YCRM, NP, IRET )			*
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
C*	XPT		REAL		X coordinate / latitude		*
C*	YPT		REAL		Y coordinate / longitude	*
C*	XCRM		REAL		X radius coordinate / latitude	* 
C*	YCRM		REAL		Y radius coordinate / latitude	*
C* 	NP		INTEGER		Number of points on circle	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/86						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* M. Linda/GSC		 9/97	Corrected right border of prologue	*
C* T. Piper/GSC		 5/98	Corrected prolog		        *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C* A. Hardy/GSC         11/98   Changed calling sequence                *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sys
C*
	INTEGER		isend (3)
	REAL		rsend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to mailbox.
C
	isend (1) = 8
	isend (2) = FCIRCL
C
C*	Check the validity of the coordinate system.
C
	isys = INDEX ( syslo, sys ) + INDEX ( sysup, sys )
	IF  ( isys .eq. 0 )  THEN
	    iret = NOCORD
	    RETURN
	END IF
C
	isend (3) = isys
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	rsend (1) = xpt
	rsend (2) = ypt
	rsend (3) = xcrm
	rsend (4) = ycrm
	CALL GPUTR  ( rsend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( np, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	If the writes were sucessful, get output parameter.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
