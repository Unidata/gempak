	SUBROUTINE SN_INTC  ( data, nparms, nlev, tmpc, p, h, iret )
C************************************************************************
C* SN_INTC								*
C*									*
C* This subroutine finds the pressure and height of temperature, TMPC	*
C* from climatology sounding data. It assumes that the temperature	*
C* decreases monotonically with	height, and the temperature at the	*
C* lowest level is always warmer than TMPC.  If TMPC is colder than	*
C* the sounding, an extrapolation of the top level data is used		*
C*									*
C* SN_INTC  ( DATA, NPARMS, NLEV, TMPC, P, H, IRET )			*
C*									*
C* Input parameters:							*
C*	DATA		REAL		Climatology sounding data	*
C*	  (NPARMS, NLEV)						*
C*	NPARMS		INTEGER		Number of parameters		*
C*	NLEV		INTEGER		Number of levels		*
C*	TMPC		REAL		Temperature in Celsius		*
C*									*
C* Output parameters:							*
C*	P		REAL		Pressure			*
C*	H		REAL		Height				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*                                       +6 = Warm sounding		*
C**									*
C* Log:									*
C* T. Lee/GSC		 5/00	Initial coding				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( JPRE = 1, JTMP = 2, JHGT = 3 )
	REAL		data ( nparms, * )
C*
	LOGICAL		done
C------------------------------------------------------------------------
	iret = 0
C
C*	Return if TMPC is warmer than the sounding.
C
	IF  (  tmpc .gt. data ( JTMP, 1 ) )  THEN
	    iret = +6
	    p    = RMISSD
	    h    = 0.
	    RETURN
	END IF
C
	k = 1
	ntop = nlev
	done = .false.
	tb = data ( JTMP, k )
	zb = data ( JHGT, k )
	pb = data ( JPRE, k )
C
C*	Loop through the level and find the intersection.
C
	DO WHILE  ( .not. done )
	    tu = data ( JTMP, k + 1 )
	    zu = data ( JHGT, k + 1 )
	    pu = data ( JPRE, k + 1 )
C
	    IF  (  tu .le. tmpc )  THEN
C
C*	        An intersection is hit.
C
		rmult = ( tmpc - tb ) / ( tu - tb )
		plog = ALOG ( pu ) - ALOG ( pb )
		p  = pb * EXP ( plog * rmult )
		h  = zb + ( zu - zb ) * rmult
		RETURN
	      ELSE
		tb = tu
		zb = zu
		pb = pu
		k = k + 1
		IF  ( k .ge. ntop ) done = .true.
	    END IF 
	END DO
C
C*	Extrapolate the temperature at top two levels if needed.
C
	zb = data ( JHGT, ntop - 1 )
	tb = data ( JTMP, ntop - 1 )
	pb = data ( JPRE, ntop - 1 )
	rmult = ( tmpc - tu ) /  ( tu - tb )
	plog = ALOG ( pu ) - ALOG ( pb )
	p  = pu * EXP ( plog * rmult )
	h  = zu + ( zu - zb ) * rmult
C*
	RETURN
	END
