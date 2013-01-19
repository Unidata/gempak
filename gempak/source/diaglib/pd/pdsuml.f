	SUBROUTINE PD_SUML ( z1, z2, th1, th2, thp1, thp2, cape, cins )
C************************************************************************
C* PD_SUML  ( Z1, Z2, th1, th2, thp1, thp2, cape, cins )		*
C*									*
C* This routine determines positive (CAPE) and negative (CINS) area by	*
C* a trapezoidal approximation.						*
C*									*
C* The area is computed based on 4 cases:				*
C*	1) The entire layer is positive					*
C*	2) The bottom of the layer is positive, top is negative		*
C*	3) The top of the layer is positive, bottom is negative		*
C*	4) The entire layer is negative					*
C*									*
C* For cases 2 & 3 above, the equilibrium level will be determined,	*
C* and the fractional area will be computed.				*
C*									*
C* Input parameters:							*
C* 	Z1		REAL		Z bottom			*
C*	Z2		REAL		Z top				*
C* 	TH1		REAL		Theta (environment) bottom	*
C*	TH2		REAL		Theta (environment) top		*
C* 	THP1		REAL		Theta (parcel) bottom		*
C*	THP2		REAL		Theta (parcel) top		*
C*									*
C* Input and Output parameters:						*
C* 	CAPE		REAL		CAPE value in layer		*
C* 	CINS		REAL		CINS value in layer		*
C*									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'

	capetmp = 0.
	cinstmp = 0.

	dttop = thp2 - th2
	dtbot = thp1 - th1

	dz = z2 - z1

	IF ( ( dtbot .ge. 0 ) .and. ( dttop .ge. 0 ) ) THEN
C
C*          CASE I . . . bottom and top positive - add in contribution.
C
	    capetmp = ( dttop + dtbot ) * dz / ( th2 + th1 )
	ELSE IF ( dtbot .gt. 0 ) THEN
C
C*          CASE II . . . bottom positive, top negative - hit an
C*          equilibrium level (not necessarily HTEQ, yet); take
C*          partial layer.
C
	    ratio = dtbot / ( dtbot - dttop)
	    zeq = z1 + ( dz * ratio )
	    teq = th1 + ( ( th2 - th1 ) * ratio )
	    capetmp = dtbot * ( zeq - z1 ) / ( th1 + teq )
	    cinstmp = dttop * ( z2 - zeq ) / ( th2 + teq )
	ELSE IF ( dttop .gt. 0 ) THEN
C
C*          CASE III . . . bottom negative, top positive - hit a level
C*          of free convection, ZFC (not necessarily the first);
C*          take partial layer.
C
	    IF (dttop .lt. 0) write(*,*) 'oops'
	    ratio = dttop / ( dttop - dtbot )
            zfc  = z2 - ( dz * ratio )
            tfc  = th2 - ( ( th2 - th1 ) * ratio )
            capetmp = dttop * ( z2 - zfc ) / ( tfc + th2 )
	    cinstmp = dtbot * ( zfc - z1 ) / ( tfc + th1 )
	ELSE
C
C*	    CASE IV . . . . bottom and top negative - CINS
C
	    cinstmp = ( dttop + dtbot ) * dz / ( th2 + th1 )
	END IF
C
	IF ( capetmp .gt. 0 ) THEN
	    cape = cape + (capetmp * GRAVTY )
	END IF
C
	IF ( cinstmp .lt. 0 ) THEN
	    cins = cins + (cinstmp * GRAVTY )
	END IF

	RETURN
	END
