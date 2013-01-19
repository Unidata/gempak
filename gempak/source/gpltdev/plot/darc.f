	SUBROUTINE DARC  ( iwndw, x, y, xc, yc, np, ang1, ang2, 
     +                     endpts, iret )
C************************************************************************
C* DARC 								*
C*									*
C* This subroutine will draw an arc centered on x and y with the        *
C* circumference of xc, yc between the angles, ang1 and ang2. 		*
C*									*
C* DARC  ( IWNDW, X, Y, XC, YC, NP, ANG1, ANG2, ENDPTS, IRET )		*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER	 	Clipping window 		*
C*	X		REAL		X coordinates 			*
C*	Y		REAL		Y coordinates 			*
C*	XC		REAL		X radius circumference coords.  *
C*	YC		REAL		Y radius circumference coords. 	*
C* 	NP		INTEGER	 	Number of points 		*
C*	ANG1		REAL		Starting angle			*
C*	ANG2		REAL		Ending angle			*
C*									*
C* Output parameters:							*
C*	ENDPTS (4)	REAL		Beginning/Ending arc points	*
C*	IRET		INTEGER	 Return code				*
C**									*
C* Log:									*
C* A. Hardy/GSC		 5/00		Modified from DCIRCL		*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (3)
	REAL            rsend (4), ends (4), endpts (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to mailbox.
C
	isend (1) = 10 
	isend (2) = CARC
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
	CALL GPUTR  ( ang1, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( ang2, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	If the writes were sucessful, get output parameter.
C    	        
	CALL GGET  ( iret, 1, ier ) 
        iret = ier
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGETR  ( ends, 4, iret )
	IF  ( iret .eq. NORMAL ) THEN  
            endpts (1) = ends (1)
            endpts (2) = ends (2)
            endpts (3) = ends (3)
            endpts (4) = ends (4)
        END IF
C*
	RETURN
	END
