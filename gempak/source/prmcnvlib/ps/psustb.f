	SUBROUTINE PS_USTB  ( datain, nparm, plev, outdat, iret )
C************************************************************************
C* PS_USTB								*
C*									*
C* This subroutine finds the most unstable level of a sounding from	*
C* surface up to PLEV. The most unstable level is defined as the level	*
C* which has the warmest pseudo	wet-bulb potential temperature computed	*
C* by lifting the air parcel to saturation then returning it moist	*
C* adiabatically to 1000 mb.  If plev = -1, the entire sounding	is	*
C* searched.								*
C*									*
C* PS_USTB  ( DATAIN, NPARM, PLEV, OUTDAT, IRET )			*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Input sounding data		*
C*	NPARM		INTEGER         Number of parameters		*
C*	PLEV		REAL		Pressure level			*
C*									*
C* Output parameters:							*
C*	OUTDAT (*)	REAL		Data at the most unstable level	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/GSC		 1/00	Created					*
C* T. Lee/GSC		 4/01	Assigned plev to a temporary variable	*
C* T. Piper/SAIC	 4/02	Fixed UMR; initialized datout		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	PARAMETER	( NPMS = 10 )
C*
	REAL            datain (*), outdat (*)
C*
	REAL		datlev (NPMS), datout (NPMS)
	LOGICAL		done 
C* 
C------------------------------------------------------------------------
	iret = 0
	eps  = RMISSD
	ppp  = plev
C
C*	Find the top and surface level.
C
	CALL PC_FTOP ( datain, nparm, nlev, datlev, ier )
	ptop = datlev ( 1 ) 
	CALL PC_FLVL ( 0., 1, datain, psfc, level1, level2, lvtyp, ier )
C
	IF  ( ppp .gt. psfc )  THEN
	    DO  i = 1, NPMS
		outdat ( i ) = RMISSD
	    END DO
	    RETURN
	  ELSE IF ( ( ppp .eq. -1. ) .or. ( ppp .le. ptop ) ) THEN
	    ppp = ptop
	END IF
C
C*	Loop through the sounding data.
C
	done  = .false.
	lev   = 1
	DO i = 1, NPMS
	    datout (i) = RMISSD
	END DO

	DO WHILE  ( .not. done )
	    CALL PC_GLEV    ( lev, datain, nparm, datlev, ier )
	    CALL PC_COMP    ( 5, datlev, datout, ier )
	    pres  = datout  ( 1 )
	    tmpc  = datout  ( 2 )
	    dwpc  = datout  ( 3 )
	    thwc  = PR_THWC ( pres, tmpc, dwpc ) 
C
	    IF  ( ( thwc .gt. eps ) .and. ( pres .ge. ppp ) )  THEN
		eps = thwc
		DO  i = 1, NPMS
		    outdat ( i ) = datout ( i )
		END DO
	    END IF
	    lev = lev + 1
	    IF  ( pres .le. ppp )  done = .true.
	END DO
C*
	RETURN
	END
