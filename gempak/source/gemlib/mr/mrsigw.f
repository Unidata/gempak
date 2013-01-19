	SUBROUTINE MR_SIGW  ( datsgw, nsgw, datasw, nasw, nlev, ipt,
     +			      stndat, idtype, sclhgt, iret )
C************************************************************************
C* MR_SIGW								*
C*									*
C* This subroutine adds significant wind data to the station data	*
C* array.  Pointers to the data ordered according to increasing 	*
C* height are returned.							*
C*									*
C* MR_SIGW  ( DATSGW, NSGW, DATASW, NASW, NLEV, IPT, STNDAT, IDTYPE,	*
C*            SCLHGT, IRET )						*
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
C*	SCLHGT ( NLEV )	REAL		Scale heights			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/86						*
C* M. desJardins/GSFC	 9/87	Rewritten for GEMPAK4			*
C* M. desJardins/GSFC	12/88	Fixed sig wind at mand lev		*
C* M. desJardins/GSFC	 1/89	Added IDTYPE				*
C* M. desJardins/GSFC	 4/89	Added check for missing spd or dir	*
C* T. Piper/SAIC	 2/02   Initialized znxt & zold; not always set	*
C* S. Jacobs/NCEP	10/10	Fixed updating sig winds at existing	*
C*				levels					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	REAL		datsgw (3,*), datasw (3,*), stndat (6,*)
	REAL		sclhgt (*)
	INTEGER		ipt (*), idtype (*)
	LOGICAL		more, skip, above
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Skip the surface data.
C
	IF ( (nsgw .ge. 1) .and. ( ( datsgw (1,1) .eq. 0. ) .or.
     +			( datsgw (1,1) .eq. stndat (1,1) ) ) )  THEN
	    istart = 2
	  ELSE
	    istart = 1
	END IF
C
C*	Merge only the heights between levels already in dataset.
C
	pressf = stndat ( 1, ipt (1) )
	hghtsf = stndat ( 6, ipt (1) )
	IF ( ( nlev .ge. 2 ) .and. ( .not. ERMISS ( pressf ) ) .and.
     +	     ( .not. ERMISS ( hghtsf ) ) )  THEN
	    more = .true.
	    zold = stndat ( 6, ipt (1) )
	    znxt = stndat ( 6, ipt (2) )
	    ilev = 2
	  ELSE IF  ( nlev .ge. 3 )  THEN
	    more = .true.
	    zold = stndat ( 6, ipt (2) )
	    znxt = stndat ( 6, ipt (3) )
	    ilev = 3
	  ELSE
	    znxt = RMISSD
	    zold = RMISSD
	END IF
	IF (( ERMISS ( zold ) ) .or. ( ERMISS ( znxt ) )) more = .false.
C
C*	Loop throught data below 100 mb. and then above. 
C*	I points to next input data.
C
	IF  ( istart .le. nsgw ) THEN
	    above = .false.
	    i     = istart
	    iend  = nsgw
	  ELSE
	    above = .true.
	    i     = 1
	    iend  = nasw
	END IF
C*
	DO WHILE  ( more .and. ( i .le. iend ) )
	    IF ( .not. above ) THEN
		hght = datsgw (1,i)
		drct = datsgw (2,i)
		sped = datsgw (3,i)
	      ELSE
		hght = datasw (1,i)
		drct = datasw (2,i)
		sped = datasw (3,i)
	    END IF
	    skip = .false.
C
C*	    Skip this data if data is missing.
C
	    IF  ( ( ERMISS ( hght ) ) .or. ( ERMISS ( sped ) ) .or.
     +		  ( ERMISS ( drct ) ) )  THEN
		skip = .true.
C
C*		If this is an existing level, add winds if they were
C*		missing.
C
	      ELSE IF ( ABS ( zold - hght ) .lt. 1. ) THEN
		skip = .true.
		IF  ( ERMISS ( stndat ( 4, ipt (ilev - 1) ) ) .or.
     +		      ERMISS ( stndat ( 5, ipt (ilev - 1) ) ) )  THEN
		    stndat ( 4, ipt ( ilev - 1 ) ) = drct
		    stndat ( 5, ipt ( ilev - 1 ) ) = sped
		END IF
C
C*		Skip data is it is not above last data.
C
	      ELSE IF ( hght .le. zold ) THEN
		skip = .true.
C
C*		Check that bounding layers are correct.
C
	      ELSE IF ( hght .ge. znxt ) THEN
		DO WHILE  ( more .and. ( hght .gt. znxt ) )
		    zold = znxt
		    ilev = ilev + 1
		    IF ( ilev .gt. nlev ) THEN
			more = .false.
		      ELSE
			znxt = stndat ( 6, ipt (ilev) )
			IF  ( ERMISS ( znxt ) ) more = .false.
		    END IF
		END DO
	    END IF
C
C*	    Add new data to list.
C
	    IF  ( more .and. ( .not. skip ) )  THEN
C
C*		Add the wind data for this level, if it does not exist.
C
		IF  (  ABS ( znxt - hght ) .lt. 1. )  THEN
		    IF  ( ERMISS ( stndat ( 4, ipt (ilev) ) ) .or.
     +			  ERMISS ( stndat ( 5, ipt (ilev) ) ) )  THEN
			stndat ( 4, ipt ( ilev) ) = drct
			stndat ( 5, ipt ( ilev) ) = sped
		    END IF
C
C*		    Otherwise, add new level.
C
		  ELSE
		    nlev = nlev + 1
		    stndat ( 6, nlev ) = hght
		    stndat ( 4, nlev ) = drct
		    stndat ( 5, nlev ) = sped
		    DO  k = 1, 3
			stndat ( k, nlev ) = RMISSD
		    END DO
		    idtype ( nlev ) = 3
C
C*		    Add new station to ipt array.
C
		    DO  j = nlev, ilev + 1, -1
			ipt ( j )    = ipt ( j-1 )
C			sclhgt ( j ) = sclhgt ( j-1 )
		    END DO
		    ipt ( ilev ) = nlev
		    ilev = ilev + 1
		    zold = hght
		END IF
	    END IF
C
C*	    Set end of loop.  Change to data above 100 mb if below
C*	    is being processed and this is the last level.
C
	    IF ( ( .not. above ) .and. (i .eq. nsgw) ) THEN
		above = .true.
		i     = 1
		iend  = nasw
	      ELSE
		i     = i + 1
	    END IF
	END DO
C*
	RETURN
	END
