	SUBROUTINE GDMLBL ( grdlbl, kx, ky, ix1, iy1, ix2, iy2,
     +			    ixinc, iyinc, iret )
C************************************************************************
C* GDMLBL								*
C*									*
C* This subroutine will plot axis labels on the map.			*
C*									*
C* GDMLBL  ( GRDLBL, KX, KY, IX1, IY1, IX2, IY2, IXINC, IYINC, IRET )	*
C*									*
C* Input parameters:							*
C*	GRDLBL		CHAR*		Input for GRDLBL		*
C*	KX		INTEGER		Number of points in the x dir	*
C*	KY		INTEGER		Number of points in the y dir	*
C*	IX1		INTEGER		First point in the x dir	*
C*	IY1		INTEGER		First point in the y dir	*
C*	IX2		INTEGER		Last point in the x dir		*
C*	IY2		INTEGER		Last point in the y dir		*
C*	IXINC		INTEGER		Increment in the x dir		*
C*	IYINC		INTEGER		Increment in the y dir		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 9/91						*
C* M. desJardins/NMC	10/91	Use only color in grdlbl		*
C* J. Whistler/SSAI	 7/92	Changed to use 'D' coordinates		*
C* D.W.Plummer/NCEP	 1/97	Remove call to GTRANS, use 'G' coords	*
C* R. Tian/SAIC		10/02	Add call to DG_IGRG			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	grdlbl
C*
	CHARACTER	chlbl*10
	LOGICAL		found
C------------------------------------------------------------------------
	iret = 0
C*
C*
	CALL GQCOLR  ( icltmp, ierr )
	CALL ST_ILST  ( grdlbl, ';', -1, 1, icolor, n, ier )
C*
	IF  ( icolor .gt. 0 )  THEN
	    CALL GSCOLR ( icolor, ierr )
C
C*	    Plot labels for X-axis.
C
	    iymid = ( iy2 + iy1 ) / 2
	    found = .false.
	    ii = 1
	    DO WHILE ( ( ii .le. iyinc )  .and. (.not. found) )
		DO i = iy1, iy2, iyinc
		    IF ( iymid .eq. i ) found = .true.
		END DO
		iymid = iymid - 1
		ii = ii + 1
	    END DO
	    iymid = iymid + 1
	    DO  i = ix1, ix2, ixinc
		fx = i
		fy = ( iymid + iymid + iyinc ) / 2.
		fxx = fx
		fyy = fy
		CALL DG_IGRG ( 1, fxx, fyy, fxx, fyy, ier )
		ix = NINT(fxx)
	        CALL ST_INCH ( ix, chlbl, ier )
	        CALL ST_LSTR ( chlbl, leng, ier )
	        ixoff = - leng
	        CALL GTEXT ( 'G', fx, fy, chlbl, 0., ixoff, 0, ier )
	    END DO
C
C*	    Plot labels for Y-axis.
C
	    ixmid = ( ix1 + ix2 ) / 2
	    found = .false.
	    ii = 1
	    DO WHILE ( ( ii .le. ixinc )  .and. (.not. found) )
		DO i = ix1, ix2, ixinc
		    IF ( ixmid .eq. i ) found = .true.
		END DO
		ixmid = ixmid - 1
		ii = ii + 1
	    END DO
	    ixmid = ixmid + 1
	    DO  j = iy1, iy2, iyinc
		fx = ( ixmid + ixmid + ixinc ) / 2.
		fy = j
		fxx = fx
		fyy = fy
		CALL DG_IGRG ( 1, fxx, fyy, fxx, fyy, ier )
		iy = NINT(fyy)
	        CALL ST_INCH ( iy, chlbl, ier )
	        CALL ST_LSTR ( chlbl, leng, ier )
	        ixoff = - leng
	        CALL GTEXT ( 'G', fx, fy, chlbl, 0., ixoff, 0, ier )
	    END DO
	END IF
C*
	CALL GSCOLR ( icltmp, ierr )
C*
	RETURN
	END
