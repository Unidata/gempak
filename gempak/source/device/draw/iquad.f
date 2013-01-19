	SUBROUTINE IQUAD ( ix, iy, rad, ibeg, ifin, ixoff, iyoff, iret )
C************************************************************************
C* IQUAD								*
C*									*
C* This subroutine will fill a section of a circle between the 		*
C* beginning and ending angles.  Angles are measured in a counter-	*
C* clockwise direction from the x axis.  The offsets create unfilled	*
C* zones having a width of twice the offset centered on the axis 	*
C* perpendicular to the direction for which the offset is defined.	*	
C*									*
C* IQUAD ( IX, IY, RAD, IBEG, IFIN, IXOFF, IYOFF, IRET )		*
C*									*
C* Input parameters:							*
C*	IX		INTEGER		X coord of the circle center	*
C*	IY		INTEGER		Y coord of the circle center	*
C*	RAD		REAL		Radius of the circle		*
C*	IBEG		INTEGER		The beginning angle for the arc	*
C*	IFIN		INTEGER		The ending angle for the arc	*
C*	IXOFF		INTEGER		Offset along x axis		*
C*	IYOFF		INTEGER		Offset along y axis		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 9/91		Created for use with ISKY	*
C* K. Brill/NMC		10/91	Added rounding for points		*
C* K. Brill/NMC		11/91	Generate pts as in ICIRC; fix for gaps  *
C* K. Brill/NMC		02/92	Cleaned up and added fill		*
C* K. Brill/NMC		02/92	Changed meaning of offsets and coded it *
C* K. Brill/NMC		09/92	Do the fill correctly by filling sector	*
C* K. Brill/NMC		11/92	Use integer arguments in BETWEN		*
C* S. Jacobs/NCEP	 9/97	Added call to DSFILL to set solid fill	*
C* S. Jacobs/NCEP	 3/98	Changed value of solid fill in DSFILL	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		ixp(64), iyp(64)
	LOGICAL		betwen
	BETWEN ( ixx, ix1, ix2 ) =
     +			 ( ( ixx .ge. ix1 .and. ixx .le. ix2 ) .or.
     +			   ( ixx .le. ix1 .and. ixx .ge. ix2 ) )
C------------------------------------------------------------------------
	iret = 0
C*
	IF ( ibeg .eq. 0 ) THEN
	    rc = rad
	    rs = 0.0
	ELSE IF ( ibeg .eq. 90 ) THEN
	    rc = 0.0
	    rs = rad
	ELSE IF ( ibeg .eq. 180 ) THEN
	    rc = -rad
	    rs = 0.0
	ELSE IF ( ibeg .eq. 270 ) THEN
	    rc = 0.0
	    rs = -rad
	ELSE
	    rs = ( rad * SIN ( ibeg * DTR ) )
	    rc = ( rad * COS ( ibeg * DTR ) )
	END IF
C*
	npp = TWOPI * rad * ( FLOAT ( ifin - ibeg ) / 360. )
	npp = npp + 1
	pang = ( FLOAT (ifin - ibeg) * DTR ) / FLOAT (npp-1)
	cosp = cos ( pang )
	sinp = sin ( pang )
C*
	IF ( filflg ) THEN
	    jbeg = ( ibeg + ifin ) / 2
	    IF ( jbeg .ge. 0 .and. jbeg .le. 180 ) THEN
	    	jyoff = iyoff
	    	iyf = iy + NINT (rad) * ispany
	    ELSE
		jyoff = - iyoff
	    	iyf = iy - NINT (rad) * ispany
	    END IF
	    IF ( jbeg .gt. 270 .or.
     +	         jbeg .ge. 0 .and. jbeg .le. 90 ) THEN
		jxoff = ixoff
	    	ixf = ix + NINT (rad) * ispanx
	    ELSE IF ( jbeg .ge. 90 .and. jbeg .le. 270 ) THEN
	    	jxoff = - ixoff
	    	ixf = ix - NINT (rad) * ispanx
	    END IF
	    ixp (1) = ix + jxoff * ispanx
	    iyp (1) = iy + jyoff * ispany
	    ixo = ixp (1) 
	    iyo = iyp (1)
	    IF ( jxoff .eq. 0 ) THEN
		ixo = ix - NINT (rad) * ispanx
		ixf = ix + NINT (rad) * ispanx
	    END IF
	    IF ( jyoff .eq. 0 ) THEN
		iyo = iy - NINT (rad) * ispany
		iyf = iy + NINT (rad) * ispany
	    END IF
	    index = 1
	    imod = npp/16
	    IF ( imod .eq. 0 ) imod = 1
	    DO iang = 1, npp
		irs = NINT ( rs )
	        irc = NINT ( rc * aspect )
		ixpc = ix + ispanx * irc
		iypc = iy + ispany * irs
		IF ( BETWEN ( ixpc, ixo, ixf ) .and.
     +		     BETWEN ( iypc, iyo, iyf ) ) THEN
		    ixpcl = ixpc
		    iypcl = iypc
		    IF ( index .eq. 1 .or. iang .eq. npp .or.
     +			 MOD ( iang, imod ) .eq. 0 ) THEN
		    	index = index + 1
		    	ixp ( index ) = ixpc
		    	iyp ( index ) = iypc
		    END IF
		END IF
		xc = cosp * rc - sinp * rs
		yc = sinp * rc + cosp * rs
		rs = yc
		rc = xc
	    END DO
	    IF ( ixpcl .ne. ixpc .and. iypcl .ne. iypc ) THEN
		index = index + 1
		ixp (index) = ixpcl
		iyp (index) = iypcl
	    END IF
	    msvft = mfltyp
	    svfsz = tfilsz
	    CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	    CALL IFILL ( index, ixp, iyp, ier )
	    CALL DSFILL ( svfsz, msvft, fsize, itype, ier )
C*
	  ELSE
	    DO  iang = 1, npp
C
C*	    	Fill with horizontal lines.
C
	    	irs = NINT ( rs )
	    	irc = NINT ( rc * aspect )
	     	IF ( irc .lt. 0 ) THEN
		    ixp (1) = ix - ixoff * ispanx
		ELSE
		    ixp (1) = ix + ixoff * ispanx
		END IF
	    	iyp (1) = iy + ispany * irs
	    	ixp (2) = ix + ispanx * irc
	    	iyp (2) = iyp (1)
		IF ( IABS ( iyp (2) - iy ) .ge. iyoff .and.
     +		     IABS ( ixp (2) - ix ) .gt. IABS ( ixp (1) - ix ) )
     +				CALL ILINE ( 2, ixp, iyp, ier )
	    	xc  = cosp * rc - sinp * rs
	    	yc  = sinp * rc + cosp * rs
	    	rs  = yc
	    	rc  = xc
C*
	    END DO
	END IF
C
	RETURN
	END
