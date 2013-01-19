	SUBROUTINE MR_SIGT  ( datsgt, nsgt, datast, nast, 
     +			      nlev, ipt, stndat, idtype, iret )
C************************************************************************
C* MR_SIGT								*
C*									*
C* This subroutine adds significant temperature data to the station	*
C* data array.  Pointers to the data ordered according to decreasing	*
C* pressure are returned.  						*
C*									*
C* MR_SIGT  ( DATSGT, NSGT, DATAST, NAST, NLEV, IPT, STNDAT, IDTYPE,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	DATSGT (3,NSGT)	REAL		Sig temp data below 100 mb	*
C*	NSGT		INTEGER		Number of levels below 100 mb	*
C*	DATAST (3,NAST)	REAL		Sig temp data above 100 mb	*
C*	NAST		INTEGER		Number of levels above 100 mb	*
C*									*
C* Input and output parameters:						*
C*	NLEV		INTEGER		Number of levels		*
C*	IPT    ( NLEV )	INTEGER		Pointers to ordered data	*
C*	STNDAT (6,NLEV)	REAL		Station data			*
C*	IDTYPE (NLEV)	INTEGER		Data type flags			*
C*					  1 = mandatory			*
C*					  2 = sig temperature		*
C*					  3 = sig wind			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/86						*
C* M. desJardins/GSFC	 9/87	Rewritten for GEMPAK4			*
C* M. desJardins/GSFC	12/87	Fixed bug when there is no sfc data	*
C* M. desJardins/GSFC	 1/89	Added IDTYPE				*
C* K. Brill/NMC		01/92	Set ilev=1 for sig lvls below 1st mand	*
C* D. Kidwell/NCEP	 3/01	Fixed pointer in store for same levels  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		datsgt (3,*), datast (3,*), stndat (6,*)
	INTEGER		ipt (*), idtype (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Set the following values which are used throughout the code:
C*		ILEV	Current level
C*		PBOT	Pressure at current level
C*		PTOP	Pressure at next level
C
	IF  ( .not. ERMISS ( stndat ( 1, 1 ) ) )  THEN
	    ilev = 1
	    pbot = stndat ( 1, 1 )
	  ELSE IF  ( nlev .gt. 1 )  THEN
	    ilev = 2
	    pbot = stndat ( 1, 2 )
	    IF ( pbot .lt. datsgt ( 1, 2 ) ) THEN
		ilev = 1
	        pbot = 1050.
	    END IF
	  ELSE
	    ilev = LLMXLV
	    pbot = 1050.0
	END IF
C
C*	Find next pressure.
C
	IF  ( ilev .lt. nlev )  THEN
	    ptop = stndat ( 1, ipt ( ilev + 1 ) ) 
	  ELSE
	    ptop = 0.0
	END IF
C
C*	Check each significant level below 100 mb.
C
	DO  i = 1, nsgt
C
C*	    Check that both pressure and temperature are present.
C
	    IF  ( ( .not. ERMISS ( datsgt ( 1, i ) ) ) .and.
     +		  ( .not. ERMISS ( datsgt ( 2, i ) ) ) .and.
     +		  ( datsgt ( 1, i ) .ne. 0.0 ) )  THEN
C
C*		Get the pressure value.
C
		pres = ABS ( datsgt ( 1, i ) )
C
C*		Get correct insertion point.
C
		DO WHILE  ( pres .le. ptop )
C
C*		    Go to next level and check that data is not past top.
C
		    ilev = ilev + 1
		    pbot = ptop
		    IF  ( ilev .ge. nlev )  THEN
			ptop = 0.0
			ilev = LLMXLV
		      ELSE
			ptop = stndat ( 1, ipt ( ilev + 1 ) )
		    END IF
		END DO
C
C*		Ignore data if pressure is increasing.
C
		IF  ( pres .gt. pbot )  THEN
C*		    ! Ignore
C
C*		    If levels are identical, replace missing data.
C
		  ELSE IF  ( pres .eq. pbot )  THEN
		    IF  ( ilev .le. nlev ) THEN
		      IF  ( ERMISS ( stndat ( 2, ipt (ilev) ) ) )  THEN
			  stndat ( 2, ipt (ilev) ) = datsgt ( 2, i )
			  stndat ( 3, ipt (ilev) ) = datsgt ( 3, i )
		      END IF
		    END IF
C
C*		    Otherwise, insert level.
C
		  ELSE
C
C*		    Add data at next level.
C
		    nlev = nlev + 1
		    DO  ii = 1, 3
			stndat (  ii , nlev ) = datsgt ( ii, i )
			stndat ( ii+3, nlev ) = RMISSD
		    END DO
		    idtype ( nlev ) = 2
C
C*		    Correct pointers to following data.
C
		    DO  ii = nlev, ilev + 2, -1
			ipt ( ii ) = ipt ( ii - 1 )
		    END DO
C
C*		    Insert pointer to data.
C
		    pbot = pres
		    ilev = ilev + 1
		    IF  ( ilev .le. nlev )  THEN
			ipt ( ilev ) = nlev
		      ELSE
			ipt ( nlev ) = nlev
		    END IF
		END IF
	    END IF
	END DO
C
C*	Check each significant level above 100 mb.
C
	DO  i = 1, nast
C
C*	    Check that both pressure and temperature are present.
C
	    IF  ( ( .not. ERMISS ( datast ( 1, i ) ) ) .and.
     +		  ( .not. ERMISS ( datast ( 2, i ) ) ) .and.
     +		  ( datast ( 1, i ) .ne. 0.0 ) )  THEN
C
C*		Get the pressure value.
C
		pres = ABS ( datast ( 1, i ) )
C
C*		Get correct insertion point.
C
		DO WHILE  ( pres .le. ptop )
C
C*		    Go to next level; check that data is not past top.
C
		    ilev = ilev + 1
		    pbot = ptop
		    IF  ( ilev .ge. nlev )  THEN
			ptop = 0.0
			ilev = LLMXLV
		      ELSE
			ptop = stndat ( 1, ipt ( ilev + 1 ) )
		    END IF
		END DO
C
C*		Ignore data if pressure is increasing.
C
		IF  ( pres .gt. pbot )  THEN
C*		    ! Ignore
C
C*		    If levels are identical, replace missing data.
C
		  ELSE IF  ( pres .eq. pbot )  THEN
		    IF  ( ilev .le. nlev ) THEN
		      IF  ( ERMISS ( stndat ( 2, ipt (ilev) ) ) )  THEN
			  stndat ( 2, ipt (ilev) ) = datast ( 2, i )
			  stndat ( 3, ipt (ilev) ) = datast ( 3, i )
		      END IF
		    END IF
C
C*		    Otherwise, insert level.
C
		  ELSE
C
C*		    Add data at next level.
C
		    nlev = nlev + 1
		    DO  ii = 1, 3
			stndat (  ii , nlev ) = datast ( ii, i )
			stndat ( ii+3, nlev ) = RMISSD
		    END DO
		    idtype ( nlev ) = 2
C
C*		    Correct pointers to following data.
C
		    DO  ii = nlev, ilev + 2, -1
			ipt ( ii ) = ipt ( ii - 1 )
		    END DO
C
C*		    Insert pointer to data.
C
		    pbot = pres
		    ilev = ilev + 1
		    IF  ( ilev .le. nlev )  THEN
			ipt ( ilev ) = nlev
		      ELSE
			ipt ( nlev ) = nlev
		    END IF
		END IF
	    END IF
	END DO
C*
	RETURN
	END
