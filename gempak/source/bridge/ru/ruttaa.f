	SUBROUTINE RU_TTAA  ( report, lenr, itopwn, wnknot, iwndht, 
     +			      ipoint, istnm, data, nlev, tdata, nlevt, 
     +			      wxdata, nlevw, drpwnd, iret )
C************************************************************************
C* RU_TTAA								*
C*									*
C* This subroutine decodes TTAA reports.  These reports contain		*
C* surface, mandatory, tropopause and max wind data below 100 mb.       *
C* Surface/mandatory data are ordered PRES TEMP DWPT DRCT SPED HGHT.    *
C* Tropopause data are ordered PRES TEMP DWPT DRCT SPED.  Max wind data *
C* are ordered PRES DRCT SPED.                                          *
C*									*
C* RU_TTAA  ( REPORT, LENR, ITOPWN, WNKNOT, IWNDHT, IPOINT, ISTNM, 	*
C*            DATA, NLEV, TDATA, NLEVT, WXDATA, NLEVW, DRPWND, IRET )	* 
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		TTAA report			*
C*	LENR		INTEGER		Length of station report	*
C*	ITOPWN		INTEGER		Pressure for last wind report	*
C*	WNKNOT		LOGICAL		Flag for wind in knots		*
C*	IWNDHT		INTEGER		Dropsonde wind cutoff value	*
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report		*
C*	ISTNM		INTEGER		Station number			*
C*									*
C* Output parameters:							*
C*	DATA (6,NLEV)	REAL		Mandatory level data		*
C*	NLEV		INTEGER		Number of mandatory levels	*
C*	TDATA (5,NLEVT) REAL		Tropopause level data           *
C*	NLEVT		INTEGER		Number of tropopauses           *
C*	WXDATA(3,NLEVW)	REAL		Maximum wind level data         *
C*	NLEVW		INTEGER		Number of maximum wind levels   *
C*	DRPWND		LOGICAL		Flag to replace dropsonde wind	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = no data found		*
C*					 -4 = bad dropsonde id          *
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* D. Kidwell/NCEP	 2/01	Added tropopause and max wind data      *
C* D. Kidwell/NCEP	 3/01	Added RU_DRP1 call and argument istnm   *
C* D. Kidwell/NCEP	 2/05	Added argument drop to RU_MLVL call     *
C* m.gamazaychikov/SAIC	07/05	Added parms to CS and code for drpwnd	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
	REAL		data (*), tdata (*), wxdata (*)
	LOGICAL		wnknot
C*
	LOGICAL		above, drop, drpwnd
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret   = 0
	above  = .false.
	drpwnd = .false.
C
C*	Get the surface data.
C
	CALL RU_MSFC  ( report, lenr, wnknot, ipoint, data, ier1 )
C
C*	Check for a dropsonde.
C
	drop = .false.
        IF ( ( istnm .ge. 0 ) .and. ( istnm .le. 99 ) ) drop = .true.
C
C*	Read the mandatory level data.  Start the data after the
C*	surface data.
C
	CALL RU_MLVL  ( report, lenr, wnknot, itopwn, above, drop, 
     + 			ipoint, data ( 7 ), nlev, ier2 )
C
C*	Check for neither mandatory nor surface data.
C
	IF  ( ( ier1 .ne. 0 ) .and. ( nlev .eq. 0 ) )  THEN 
	    iret = -2
	  ELSE
	    nlev = nlev + 1
	END IF
C
C*	Read the tropopause data.
C
 	CALL RU_MTRP ( report, lenr, wnknot, above, ipoint, tdata,
     +		       nlevt, ier )
C
C*	Read the maximum wind data.
C
 	CALL RU_MMXW ( report, lenr, wnknot, above, ipoint, wxdata,
     +		       nlevw, ier )
C
C*      Check the station number.  If it is an hour value, this is a
C*      dropsonde report and number must be gotten from the 61616
C*      section.
C
        IF ( drop ) THEN
            CALL RU_DRP1 ( report, lenr, ipoint, istnm, lstwnd, ier )
	    IF ( iret .eq. 0 )  iret = ier
            IF ( iret .eq. 0 ) THEN
C
C*             Check if the last reported wind is less than or
C*             equal to the cutoff value
C
	       IF ( lstwnd .ne. IMISSD ) THEN
	          IF ( lstwnd .le. iwndht) THEN
	             IF ( ERMISS ( data ( 4 ) ) .and. .not. 
     +                    ERMISS ( data ( 1 ) ) ) THEN
		        drpwnd = .true.
	             END IF
	          END IF
	       END IF
	    END IF
        END IF
C*
	RETURN
	END
