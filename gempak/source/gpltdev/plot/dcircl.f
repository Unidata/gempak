	SUBROUTINE DCIRCL  ( iwndw, x, y, xc, yc, np, iret )
C************************************************************************
C* DCIRCL								*
C*									*
C* This subroutine will draw a circle centered on x and y through the   *
C* points xc, yc on the circle's circumference.          	        *
C*									*
C* DCIRCL  ( IWNDW, X, Y, XC, YC, NP, IRET )				*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER	 	Clipping window 		*
C*	X		REAL		X coordinates 			*
C*	Y		REAL		Y coordinates 			*
C*	XC		REAL		X radius circumference coords.  *
C*	YC		REAL		Y radius circumference coords. 	*
C* 	NP		INTEGER	 	Number of points 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 Return code				*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/86						*
C* I. Graffman/RDS	9/88	Documentation and returns		*
C* A. Hardy/GSC		11/98   Changed calling sequence; isend & rsend *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (3)
	REAL            rsend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to mailbox.
C
	isend (1) = 8
	isend (2) = CCIRCL
	isend (3) = iwndw
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	rsend (1) = x
	rsend (2) = y
	rsend (3) = xc
	rsend (4) = yc
	CALL GPUTR  ( rsend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( np, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	If the writes were sucessful, get output parameter.
C    	        
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
