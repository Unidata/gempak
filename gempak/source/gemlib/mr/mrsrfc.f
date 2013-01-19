	SUBROUTINE MR_SRFC  ( datman, nman, datsgt, nsgt, datsgw, nsgw,
     +			      zbwind, selv, nlev, ipt, stndat, idtype, 
     +			      iret )
C************************************************************************
C* MR_SRFC								*
C*									*
C* This subroutine retrieves the surface data from the mandatory and	*
C* significant level data and moves it to the first level of the	*
C* station data.  The surface data is retrieved as follows:		*
C*     1.  The first level of the mandatory data is used.		*
C*     2.  If the first mandatory level found is below the surface,	*
C*         the surface data is eliminated.				*
C*     3.  If the first significant level temperature data is not	*
C*         missing and the pressure is below or at the current		*
C*         surface pressure, this data is used for the surface		*
C*         pressure, temperature and dewpoint.				*
C*     4.  If the first significant level wind data is present at	*
C*         the surface, the surface wind data is replaced.		*
C*     5.  The surface height is set to the elevation.			*
C*									*
C* MR_SRFC  ( DATMAN, NMAN, DATSGT, NSGT, DATSGW, NSGW, ZBWIND, 	*
C*            SELV, NLEV, IPT, STNDAT, IDTYPE, IRET )			*
C*									*
C* Input parameters:							*
C*	DATMAN (6,NMAN)	REAL		Mandatory data below 100 mb	*
C*	NMAN		INTEGER		Number of mandatory levels	*
C*	DATSGT (3,NSGT)	REAL		Sig temp data below 100 mb	*
C*	NSGT		INTEGER		Number of sig temp levels	*
C*	DATSGW (3,NSGW)	REAL		Sig wind data below 100 mb	*
C*	NSGW		INTEGER		Number of sig wind levels	*
C*	ZBWIND		LOGICAL		Flag set for sig wind on z	*
C*	SELV		REAL		Surface elevation		*
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
C* M. desJardins/GSFC	 9/87	Added winds on p surfaces.		*
C* M. desJardins/GSFC	 1/89	Added IDTYPE				*
C* T. Lee/SAIC		10/02	Checked 1st lvl pressure for sig wind	*
C* T. Lee/SAIC		10/02	Checked 1st lvl pressure for sig temp	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	REAL		datman (6,*), datsgt (3,*), datsgw (3,*)
	REAL		stndat (6,*)
	INTEGER		ipt (*), idtype (*)
	LOGICAL		zbwind, pwsfc
C
	INCLUDE		'ERMISS.FNC'
	pwsfc (xx,yy) = ( ABS ( xx - yy ) .lt. RDIFFD )
C------------------------------------------------------------------------
	iret    = 0
	nlev    = 1
	ipt (1) = 1
	idtype (1) = 1
C
C*	Check for surface information in the mandatory data.
C
	IF  ( nman .lt. 1 ) THEN
	    DO  i = 1, 5
		stndat (i,nlev) = RMISSD
	    END DO
	  ELSE
	    DO  i = 1, 5
		stndat (i,nlev) = datman (i,nlev)
	    END DO
	END IF
C
C*	Use the surface elevation for the surface height.
C
	stndat ( 6, 1 ) = selv
C
C*	Find the first reporting mandatory level above the surface.
C*	If the pressure reported at the surface is above the first
C*	mandatory report, it is assumed to be erroneous and the surface
C*	data is changed to the missing value.
C
	pman  = RMISSD
	iman  = 2
	DO WHILE ( ( ERMISS ( pman ) ) .and. ( iman .le. nman ) )
	    IF  ( ( .not. ERMISS ( datman ( 1, iman ) ) ) .and.
     +		  ( .not. ERMISS ( datman ( 2, iman ) ) ) .and.
     +		  ( .not. ERMISS ( datman ( 6, iman ) ) ) )  THEN
		pman = datman ( 1, iman )
	    END IF
	    iman = iman + 1
	END DO
C
C*	If surface pressure is higher than 1060 mb, set to missing.
C
	spres = stndat ( 1, 1 )
	IF  ( spres .gt. 1060. )  spres = RMISSD
C
C*	If surface pressure is missing or is less than first reporting
C*	mandatory level, set all surface data to missing.
C
	IF  ( ERMISS ( spres )  .or. ( ( spres .lt. pman ) .and.
     +				( .not. ERMISS ( pman ) ) ) )  THEN
	    stndat ( 1, 1 ) = RMISSD
	    stndat ( 2, 1 ) = RMISSD
	    stndat ( 3, 1 ) = RMISSD
	    stndat ( 4, 1 ) = RMISSD
	    stndat ( 5, 1 ) = RMISSD
	END IF
C
C*	If the surface significant temperature data is not missing,
C*	use it to replace the surface pressure, temperature and 
C*	dewpoint.  The check for psgl to be less than or equal to 
C*	pman eliminates using wildly erroneous data (perhaps).
C
	IF  ( ( nsgt .ge. 1 ) .and. ( .not. ERMISS ( datsgt (1,1) ) )
     +		.and.  ( .not. ERMISS ( datsgt (2,1) ) ) )  THEN
	    pman = stndat ( 1, 1 )
	    psgl = datsgt ( 1, 1 )
	    IF  ( ERMISS ( pman ) .or. ( pwsfc ( pman, psgl ) ) )  THEN
		stndat ( 1, 1 ) = datsgt ( 1, 1 )
		stndat ( 2, 1 ) = datsgt ( 2, 1 )
		stndat ( 3, 1 ) = datsgt ( 3, 1 )
	    END IF
	END IF
C
C*	If the first significant level wind data is surface information,
C*	use it to replace the surface data if the pressure is at surface.
C
	IF  ( zbwind )  THEN
	    IF  ( ( nsgw .ge. 1 ) .and. ( datsgw ( 1, 1 ) .eq. 0. )
     +		    .and. ( .not. ERMISS ( datsgw ( 2, 1 ) ) ) )  THEN
		stndat ( 4, 1 ) = datsgw ( 2, 1 )
		stndat ( 5, 1 ) = datsgw ( 3, 1 )
	    END IF
	  ELSE
	    IF  ( ( nsgw .ge. 1 ) .and. (.not. ERMISS ( datsgw (1,1) ) )
     +		    .and.  ( .not. ERMISS ( datsgw (2,1) ) ) )  THEN
		pman = stndat ( 1, 1 )
		psgl = ABS ( datsgw ( 1, 1 ) )
		IF  ( ERMISS ( pman ) .or. ( pwsfc ( pman, psgl ) ) )  
     +		    THEN
		    stndat ( 1, 1 ) = ABS ( datsgw ( 1, 1 ) )
		    stndat ( 4, 1 ) = datsgw ( 2, 1 )
		    stndat ( 5, 1 ) = datsgw ( 3, 1 )
		END IF
	    END IF
	END IF
C*
	RETURN
	END
