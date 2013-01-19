	SUBROUTINE PC_DPTH  ( datain, nparm, clev, ivcord, dfdpth,
     +			      idfcrd, nocc, depth, idcord, stndl,
     +			      stndb, stndt, iret )
C************************************************************************
C* PC_DPTH								*
C*									*
C* This subroutine gets the depth of a layer for the layer and		*
C* station parameters in the PC package.  If the base level is the	*
C* surface (0.), the depth will be from the surface up.  Otherwise,	*
C* the depth will be centered at CLEV.					*
C*									*
C* PC_DPTH ( DATAIN, NPARM, CLEV, IVCORD, DFDPTH, IDFCRD, NOCC,		*
C*	     DEPTH, IDCORD, STNDL, STNDB, STNDT, IRET )			*
C*									*
C* Input parameters:							*
C*	DATAIN		REAL		Input station data		*
C*	NPARM		INTEGER		Number of dataset parameters	*
C*	CLEV		REAL		Base level			*
C*	IVCORD		INTEGER		Base vertical coordinate	*
C*	DFDPTH		REAL		Default depth			*
C*	IDFCRD		INTEGER		Default depth vertical coord	*
C*	NOCC		INTEGER		Number of occurrence		*
C*									*
C* Output parameters:							*
C*	DEPTH		REAL		Depth				*
C*	IDCORD		INTEGER		Depth vertical coordinate	*
C*	STNDL (7)	REAL		Station data at base		*
C*	STNDB (7)	REAL		Station parameters at bottom	*
C*	STNDT (7)	REAL		Station parameters at top	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/90						*
C* J. Nielsen/SUNYA	 8/90	Fixed bug				*
C*				Used PC_LYRD if requested depth = 0	*
C* T. Lee/GSC		 8/97	Used PC_LYRD if requested depth = RMISSD*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		stndb (*), stndt (*), stndl (*)
C*
	CHARACTER	cvalue*20
C*
	INCLUDE         'ERMISS.FNC'
C-------------------------------------------------------------------------
	iret   = 0
	depth  = dfdpth
	idcord = idfcrd
C
C*	Get user input values.  The depth should follow an ! while the
C*	vertical coordinate should follow %.
C
	noccn = nocc
	IF ( noccn .le. 0 ) noccn = 1
	CALL PC_GCND  ( '!', noccn, rvalue, cvalue, ier )
	IF ( ( ier .eq. 0 ) .and. ( rvalue .ge. 0. ) )  THEN
	    depth  = rvalue
	    CALL PC_GCND  ( '%', noccn, rvalue, cvalue, ier )
	    IF  ( ( ier .eq. 0 ) .and. ( rvalue .ge. 1. ) .and.
     +		  ( rvalue .le. 3. ) )  THEN
		idcord = rvalue
	    END IF
	END IF
C
C*	Check that vertical coordinate is 1, 2, or 3.
C
	IF  ( ( idcord .lt. 1 ) .or. ( idcord .gt. 3 ) )  THEN
	    iret = -31
	    RETURN
	END IF
C
C*	Get all the station values at the base level.
C
	CALL PC_CMDT  ( 5, 6, 7, clev, ivcord, datain, stndl, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -31
	    RETURN
	END IF
C
C*      If depth is RMISSD, get data at the first levels above and below
C*	the base level, and calculate depth between levels.
C
        IF  ( ERMISS ( depth )  )  THEN
            CALL PC_LYRD  ( datain, nparm, clev, ivcord,
     +                      stndt, stndb, iret )
            IF  ( iret .ne. 0 )  RETURN
            IF  ( idcord .eq. 1 )  THEN
                depth = stndb (1) - stndt (1)
            ELSE IF  ( idcord .eq. 2 )  THEN
                depth = PR_THTA ( stndt (2), stndt (1) )
     +                  - PR_THTA ( stndb (2), stndb (1) )
            ELSE
                depth = stndt (6) - stndb (6)
            END IF
            IF  ( depth .le. 0. )  THEN
                iret = -31
                RETURN
            END IF
            RETURN
	  ELSE IF ( depth .lt. 0. )  THEN
	    iret = -31
	    RETURN
        END IF
C
C*	Get bottom of layer in the depth vertical coordinate.
C
	IF  ( clev .ne. 0. )  THEN
	    IF  ( idcord .eq. 1 )  THEN
		clevb = stndl (1) + depth / 2.
	      ELSE IF  ( idcord .eq. 2 )  THEN
		clevth = PR_THTA  ( stndl (2), stndl (1) )
		clevb  = clevth - depth / 2.
	      ELSE
		clevb = stndl (6) - depth / 2.
	    END IF
	  ELSE
	    IF  ( idcord .eq. 1 )  THEN
		clevb = stndl (1)
	      ELSE IF  ( idcord .eq. 2 )  THEN
		clevb = PR_THTA  ( stndl (2), stndl (1) )
	      ELSE
		clevb = stndl (6)
	    END IF
	END IF
C
C*	Get data at the bottom level.
C
	CALL PC_CMDT  ( 5, 6, 7, clevb, idcord, datain, stndb, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -31
	    RETURN
	END IF
C
C*	Get top of layer in the depth vertical coordinate.
C
	IF  ( idcord .eq. 1 )  THEN
	    clevt = clevb - depth
	  ELSE IF  ( idcord .eq. 2 )  THEN
	    clevt = clevb + depth
	  ELSE
	    clevt = clevb + depth
	END IF
C
C*	Get data at the top level.
C
	CALL PC_CMDT  ( 5, 6, 7, clevt, idcord, datain, stndt, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -31
	    RETURN
	END IF
C*
	RETURN
	END
