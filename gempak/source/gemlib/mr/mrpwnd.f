	SUBROUTINE MR_PWND  ( datsgw, nsgw, datasw, nasw, 
     +			      nlev, ipt, stndat, idtype, iret )
C************************************************************************
C* MR_PWND								*
C*									*
C* This subroutine adds significant wind on p levels to the station	*
C* data array.  Pointers to the data ordered according to decreasing	*
C* pressure are returned.  						*
C*									*
C* MR_PWND  ( DATSGW, NSGW, DATASW, NASW, NLEV, IPT, STNDAT, IDTYPE,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	DATSGW (3,NSGW)	REAL		Sig wind data below 100 mb	*
C*	NSGW		INTEGER		Number of levels below 100 mb	*
C*	DATASW (3,NASW)	REAL		Sig wind data above 100 mb	*
C*	NASW		INTEGER		Number of levels above 100 mb	*
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
C* M. desJardins/GSFC	 9/87	Rewritten for GEMPAK4			*
C* M. desJardins/GSFC	 9/87	Added absolute value for pressure	*
C* M. desJardins/GSFC	 1/89	Added IDTYPE				*
C* M. desJardins/GSFC	 4/89	Added check for missing spd		*
C* S. Jacobs/NCEP	 7/98	Fixed check for same levels above 100mb	*
C* D. Kidwell/NCEP	 2/01	Fixed pointer in store for same levels  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		datsgw (3,*), datasw (3,*), stndat (6,*)
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
	  ELSE
	    ilev = LLMXLV
	    pbot = 0.0
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
	DO  i = 1, nsgw
C
C*	    Check that both pressure and wind are present.
C
	    IF  ( ( .not. ERMISS ( datsgw ( 1, i ) ) ) .and.
     +		  ( .not. ERMISS ( datsgw ( 2, i ) ) ) .and.
     +		  ( .not. ERMISS ( datsgw ( 3, i ) ) ) .and.
     +		  ( datsgw ( 1, i ) .ne. 0.0 ) )  THEN
C
C*		Get the pressure value.
C
		pres = ABS ( datsgw ( 1, i ) )
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
		      IF  ( ERMISS ( stndat ( 4, ipt (ilev) ) ) .or.
     +			    ERMISS ( stndat ( 5, ipt (ilev) ) ) )  THEN
			  stndat ( 4, ipt (ilev) ) = datsgw ( 2, i )
			  stndat ( 5, ipt (ilev) ) = datsgw ( 3, i )
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
		    stndat ( 1, nlev ) = ABS ( datsgw ( 1, i ) )
		    stndat ( 2, nlev ) = RMISSD
		    stndat ( 3, nlev ) = RMISSD
		    stndat ( 4, nlev ) = datsgw ( 2, i )
		    stndat ( 5, nlev ) = datsgw ( 3, i )
		    stndat ( 6, nlev ) = RMISSD
		    idtype (  nlev   ) = 3
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
	DO  i = 1, nasw
C
C*	    Check that both pressure and wind are present.
C
	    IF  ( ( .not. ERMISS ( datasw ( 1, i ) ) ) .and.
     +		  ( .not. ERMISS ( datasw ( 2, i ) ) ) .and.
     +		  ( .not. ERMISS ( datasw ( 3, i ) ) ) .and.
     +		  ( datasw ( 1, i ) .ne. 0.0 ) )  THEN
C
C*		Get the pressure value.
C
		pres = ABS ( datasw ( 1, i ) )
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
		      IF  ( ERMISS ( stndat ( 4, ipt (ilev) ) ) .or.
     +			    ERMISS ( stndat ( 5, ipt (ilev) ) ) )  THEN
			  stndat ( 4, ipt (ilev) ) = datasw ( 2, i )
			  stndat ( 5, ipt (ilev) ) = datasw ( 3, i )
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
		    stndat ( 1, nlev ) = ABS ( datasw ( 1, i ) )
		    stndat ( 2, nlev ) = RMISSD
		    stndat ( 3, nlev ) = RMISSD
		    stndat ( 4, nlev ) = datasw ( 2, i )
		    stndat ( 5, nlev ) = datasw ( 3, i )
		    stndat ( 6, nlev ) = RMISSD
		    idtype (  nlev   ) = 3
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
