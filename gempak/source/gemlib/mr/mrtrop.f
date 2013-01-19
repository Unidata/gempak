	SUBROUTINE MR_TROP  ( dattrp, ntrp, datatr, natr, nlev, ipt,
     +			      stndat, idtype, iret )
C************************************************************************
C* MR_TROP								*
C*									*
C* This subroutine adds tropopause data to the station data array.      *
C* Pointers to the data ordered according to decreasing	pressure are    *
C* returned.  								*
C*									*
C* MR_TROP  ( DATTRP, NTRP, DATATR, NATR, NLEV, IPT, STNDAT, IDTYPE,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	DATTRP (5,NTRP)	REAL		Tropopause data below 100 mb	*
C*	NTRP		INTEGER		Number of levels below 100 mb	*
C*	DATATR (5,NATR)	REAL		Tropopause data above 100 mb	*
C*	NATR		INTEGER		Number of levels above 100 mb	*
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
C* D. Kidwell/NCEP	 3/01	                                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		dattrp (5,*), datatr (5,*), stndat (6,*)
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
	    IF ( pbot .lt. dattrp ( 1, 2 ) ) THEN
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
C*	Check each tropopause level below 100 mb.
C
	DO  i = 1, ntrp
C
C*	    Check that both pressure and temperature are present.
C
	    IF  ( ( .not. ERMISS ( dattrp ( 1, i ) ) ) .and.
     +		  ( .not. ERMISS ( dattrp ( 2, i ) ) ) .and.
     +		  ( dattrp ( 1, i ) .ne. 0.0 ) )  THEN
C
C*		Get the pressure value.
C
		pres = ABS ( dattrp ( 1, i ) )
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
			  stndat ( 2, ipt (ilev) ) = dattrp ( 2, i )
			  stndat ( 3, ipt (ilev) ) = dattrp ( 3, i )
		      END IF
		      IF  ( ERMISS ( stndat ( 4, ipt (ilev) ) ) )  THEN
			  stndat ( 4, ipt (ilev) ) = dattrp ( 4, i )
			  stndat ( 5, ipt (ilev) ) = dattrp ( 5, i )
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
		    DO  ii = 1, 5
			stndat (  ii , nlev ) = dattrp ( ii, i )
		    END DO
		    stndat ( 6, nlev ) = RMISSD
		    idtype ( nlev )    = 2
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
	DO  i = 1, natr
C
C*	    Check that both pressure and temperature are present.
C
	    IF  ( ( .not. ERMISS ( datatr ( 1, i ) ) ) .and.
     +		  ( .not. ERMISS ( datatr ( 2, i ) ) ) .and.
     +		  ( datatr ( 1, i ) .ne. 0.0 ) )  THEN
C
C*		Get the pressure value.
C
		pres = ABS ( datatr ( 1, i ) )
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
			  stndat ( 2, ipt (ilev) ) = datatr ( 2, i )
			  stndat ( 3, ipt (ilev) ) = datatr ( 3, i )
		      END IF
		      IF  ( ERMISS ( stndat ( 4, ipt (ilev) ) ) )  THEN
			  stndat ( 4, ipt (ilev) ) = datatr ( 4, i )
			  stndat ( 5, ipt (ilev) ) = datatr ( 5, i )
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
		    DO  ii = 1, 5
			stndat (  ii , nlev ) = datatr ( ii, i )
		    END DO
		    stndat ( 6, nlev ) = RMISSD
		    idtype ( nlev )    = 2
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
