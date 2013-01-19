	SUBROUTINE GARC  ( sys, xpt, ypt, xcrm, ycrm, np, ang1,
     +                     ang2, endpts, iret )
C************************************************************************
C* GARC									*
C*									*
C* This subroutine draws an arc centered at a point which may be        *
C* defined in any coordinate system.  The radius of the arc is 		*
C* defined by the points xcrm and ycrm on the circumference of the      *
C* arc.   NP is the number of points to be used in drawing the arc.     *
C* If NP is zero, 10 points are used.					*
C*									*
C* GARC  ( SYS, XPT, YPT, XCRM, YCRM, NP, ANG1, ANG2, ENDPTS, IRET )    *
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
C* 	NP		INTEGER		Number of points on an arc	*
C*      ANG1		REAL            Starting angle			*
C*      ANG2		REAL            Ending angle			*
C*									*
C* Output parameters:							*
C*      ENDPTS (4)	REAL            Begining/Ending arc points 	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC          6/00		Modified from GCIRCL		*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sys
C*
	INTEGER		isend (3)
	REAL		rsend (4), ends (4), endpts (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to mailbox.
C
	isend (1) = 10
	isend (2) = FARC
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
	CALL GPUTR  ( ang1, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GPUTR  ( ang2, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	If the writes were sucessful, get output parameter.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
	CALL GGETR  ( ends, 4, ier )
        IF ( ier .eq. NORMAL ) THEN
            endpts (1) = ends (1)
            endpts (2) = ends (2)
            endpts (3) = ends (3)
            endpts (4) = ends (4)
        END IF
C*
	RETURN
	END
