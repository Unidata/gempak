	SUBROUTINE GDAXLB  ( iaxis, ixtyp, ax, xn, yn, chrlab, iret )
C************************************************************************
C* GDAXLB								*
C* 									*
C* This subroutine labels graph axes.					*
C*     									*
C* GDAXLB  ( IAXIS, IXTYP, AX, XN, YN, CHRLAB, IRET )			*
C* 									*
C* Input parameters:							*
C* 	IAXIS		INTEGER		Axis label type			*
C*					  1 = x-axis, labels below	*
C*					  2 = y-axis, labels left	*
C*					  3 = x-axis, labels above	*
C*					  4 = y-axis, labels right	*
C*	IXTYP		INTEGER		X-axis ( 5 = polar )		*
C*	AX		REAL		Locations on other axis		*
C*	XN		REAL		X value in norm coords		*
C*	YN		REAL		Y value in norm coords		*
C*	CHRLAB		CHAR*		Character label			*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Chatters/RDS	10/82						*
C* M. desJardins/GSFC	 9/88	Put in subroutine properly		*
C* T. Lee/GSC		 9/97	Added ax in calling sequence		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*	
	CHARACTER*(*)	chrlab
C*
	INTEGER		ixoff (4), iyoff (4), ixlen (4), iylen (4)
	DATA		ixoff  /  1, -1,  1, +3 /,
     +                  iyoff  / -3,  0, +2,  0 /,
     +			ixlen  / -1, -2, -1,  0 /,
     +                  iylen  /  0,  0,  0,  0 /
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Get length of label.
C
	CALL ST_LSTR  ( chrlab, lenc, ier )
C
C*	Get the offsets to use in writing label.
C
	ixo = lenc * ixlen (iaxis) + ixoff (iaxis)
	iyo = lenc * iylen (iaxis) + iyoff (iaxis)
C
C*	Modify offsets for polar coordinates.
C
	IF  ( ( ixtyp .eq. 5 ) .and. ( iaxis .eq. 1 ) )  THEN
	    iy0 = -2
	END IF
	IF  ( ( ixtyp .eq. 5 ) .and. ( iaxis .eq. 2 ) )  THEN
	    IF  ( ( ax .gt. .01 ) .and. ( ax .lt. 90. ) )  THEN
	        ixo = 0
	        iyo = 2
	      ELSE IF  ( (ax .gt. 90.) .and. (ax .le. 180.) )  THEN
	        ixo = -6
	        iyo = -1
	      ELSE IF ( (ax .gt. 180.) .and. (ax .lt. 270.) )  THEN
	        ixo = 2
	        iyo = -2
	      ELSE IF ( (ax .ge. 270.) .and. (ax .le. 359.) )  THEN
	        ixo = 3
	        iyo = 0
	      ELSE IF (ax .lt. 1) THEN
	        ixo = 2
	        iyo = 1
	    END IF
	END IF
C
	IF  ( (ixtyp .eq. 5) .and. (iaxis .eq. 4) )  THEN
	    IF  ( (ax .gt. 0.) .and. (ax .lt. 90.) )  THEN
	        ixo = 1
	        iyo = -1
	      ELSE IF  ( (ax .gt. 90.) .and. (ax .le. 180.) )  THEN
	        ixo = -4
	        iyo = 2
	      ELSE IF  ( (ax .gt. 180.) .and. (ax .le. 270.) )  THEN
	        ixo = -6
	        iyo = -1
	      ELSE IF  ( (ax .gt. 270.) .and. (ax .lt. 361.) )  THEN
	        ixo = -5
	        iyo = -2
	    END IF
	END IF
C
C*	Write label.
C*
	CALL GTEXT  ( 'V', xn, yn, chrlab ( :lenc), 0.0, ixo, iyo, ier )
C*
	RETURN
	END
