	SUBROUTINE GTICK ( iaxis, axpos, mtfq, np, axary, iret )
C************************************************************************
C* GTICK								*
C*									*
C* This subroutine draws tick marks along an axis of a graph.		*
C*									*
C* GTICK ( IAXIS, AXPOS, MTFQ, NP, AXARY, IRET )			*
C*									*
C* Input parameters:							*
C*									*
C*	IAXIS		INTEGER		Axis				*
C*					  1 = x axis labeled below	*
C*					  2 = y axis labeled left	*
C*					  3 = x axis labeled above	*
C*					  4 = y axis labeled right	*
C*	AXPOS		REAL		Intersection with other axis	*
C*	MTFQ		INTEGER		Tick mark frequency		*
C*	NP		INTEGER		Number of values in AXARY	*
C*	AXARY (NP)	REAL		Locations on axis		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/89						*
C* M. desJardins/GSFC	 7/89	Change buffer to 530; check vis		*
C* K. Brill/GSC		 5/90	Added frequency and test for skewT	*
C* M. Linda/GSC		12/96	Removed /TICMBF/, passing reals on down	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'GTBUFF.CMN'
C*
	REAL		axary (*)
C
	INTEGER		mark  ( 6, 4 )
	DATA		mark  / 7, 8, 3, 4, 12, 10,
     +				6, 5, 2, 1, 11,  9,
     +				8, 7, 4, 3, 12, 10,
     +				5, 6, 1, 2, 11,  9 /
C
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check for zero points.
C
	IF ( np .le. 0 ) RETURN
C
C*	Check that device has been set.
C
	IF  ( ddev .eq. ' ' ) THEN
	    iret = NDVICE
	    RETURN
	END IF
C
C*	Set coordinate system to 2 for clipping in view coordinates.
C
	iwndw = 2
C
C*	Check for valid axis type.
C
	IF  ( ( iaxis .lt. 1 ) .or. ( iaxis .gt. 4 ) ) THEN
	    iret = NINVAL
	    RETURN
	END IF
C
C*	Get tick type to use.
C
	jtktyp = ktktyp
	iticmk = mark ( jtktyp, iaxis )
C
C*	Reset color if necessary.
C
	IF ( lcolr .ne. mcolr ) CALL DSCOLR ( lcolr, jcm, ier )
C
C*	Loop when points will not fit in one buffer.
C
	npass = ( ( np - 1 ) / LLMXPT ) + 1
	ib = 1
	DO  m = 1, npass
	    ie = ib + LLMXPT - 1
	    IF ( ie .gt. np ) ie = np
	    num = ie - ib + 1
C
C*	    Transform to device coordinates.
C
	    IF  ( ( iaxis .eq. 1 ) .or. ( iaxis .eq. 3 ) ) THEN
		DO  i = 1, num
		    hx ( i ) = axary ( ib + i - 1 )
		    hy ( i ) = axpos
		END DO
	    ELSE
		DO  i = 1, num
		    hx ( i ) = axpos
		    hy ( i ) = axary ( ib + i - 1 )
		END DO
	    END IF
	    CALL GPTVIS ( 'M', num, hx, hy, gvis, ier )
	    CALL GTRANS ( 'M', 'D', num, hx, hy, hx, hy, ier )
C
C*	    Reset positions for a skewed axis.
C
	    IF  ( ( jxtyp .eq. 4 ) .and.
     +		  ( iaxis .eq. 2 .or. iaxis .eq. 4 ) ) THEN
		CALL GQGRAF ( mxt, myt, ysz, qxl, qyb, qxr, qyt, ier )
		CALL GTRANS ( 'M', 'D', 1, axpos, qyb, skpos, yy, ier )
		DO  i = 1, num
		    hx ( i ) = skpos
		END DO
	    END IF
C
C*	    Extract only the visible tick marks.
C
	    ip = 0
	    DO  i = 1, num, mtfq
		IF  ( gvis ( i ) ) THEN
		    ip = ip + 1
		    hx ( ip ) = hx ( i )
		    hy ( ip ) = hy ( i )
		END IF
	    END DO
C
C*	    Send tick marks to device driver.
C
	    CALL DTICMK ( iwndw, iticmk, rticsz, ip, hx, hy, iret )
C
	    ib = ie + 1
	END DO
C*
	RETURN
	END
