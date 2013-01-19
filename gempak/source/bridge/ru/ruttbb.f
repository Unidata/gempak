	SUBROUTINE RU_TTBB ( report, lenr, wnknot, iwndht, ipoint, 
     +			     istnm, tdata, ntemp, wdata,  nwind,
     +			     drpwnd, iret )
C************************************************************************
C* RU_TTBB								*
C*									*
C* This subroutine decodes TTBB reports.  These reports contain		*
C* significant temperature data and may also contain significant wind	*
C* data on pressure surfaces.  The output temperature data are		*
C* ordered PRES TEMP DWPT .  The output wind data are ordered		*
C* PRES DRCT SPED .							*
C*									*
C* RU_TTBB  ( REPORT, LENR, WNKNOT, IWNDHT, IPOINT, ISTNM, TDATA,	* 
C*            NTEMP, WDATA, NWIND, DRPWND, IRET				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		TTBB report			*
C*	LENR		INTEGER		Length of report		*
C*	WNKNOT		LOGICAL		Flag for wind in knots		*
C*      IWNDHT          INTEGER         Dropsonde wind cutoff value     *
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report		*
C*	ISTNM		INTEGER		Station number                  *
C*									*
C* Output parameters:							*
C*	TDATA (3,NTEMP)	REAL		Significant temperature data	*
C*	NTEMP		INTEGER		Number of temperature levels	*
C*	WDATA (3,NWIND)	REAL		Significant wind data		*
C*	NWIND		INTEGER		Number of wind levels		*
C*      DRPWND          LOGICAL         Flag to replace dropsonde wind  *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = bad dropsonde id          *
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	 9/87	Rewritten for GEMPAK4			*
C* D. Kidwell/NCEP	 3/01	Added RU_DRP1 call and argument istnm   *
C* m.gamazaychikov/SAIC 07/05   Added parms to CS and code for drpwnd   *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C
	REAL		tdata ( 3, * ), wdata ( 3, * )
	CHARACTER*(*)	report
	LOGICAL		wnknot
C*
	LOGICAL		above, wind, drpwnd
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret  = 0
	above = .false.
	drpwnd = .false.
	level = -1
	nwind = 0
	ntemp = 0
C	
C*	Check for significant wind data which is preceded by "21212".
C
	iwind  =  INDEX ( report ( ipoint: lenr ), '21212' )
	IF  ( iwind .eq. 0 )  THEN
	    wind  = .false.
	    lens  = lenr
	  ELSE
	    iwind = ipoint + iwind - 1
	    wind  = .true.
	    lens  = iwind - 1
	    iwind = iwind + 5
	END IF
C
C*	Process the temperature data.
C
	CALL RU_STMP ( report, lens, above, ipoint, tdata, ntemp, ier )
C
C*	Process the wind data.
C
	IF  ( wind )  THEN
	    ipoint = iwind
	    CALL RU_PWND ( report, lenr, wnknot, above, ipoint, wdata, 
     +			   nwind,  ier )
	END IF
C
C*      Check the station number.  If it is an hour value, this is a
C*      dropsonde report and number must be gotten from the 61616
C*      section.
C
        IF ( ( istnm .ge. 0 ) .and. ( istnm .le. 99 ) ) THEN
            CALL RU_DRP1 ( report, lenr, ipoint, istnm, lstwnd, iret )
C
C*          Check if the last reported wind is less than or
C*          equal to the cutoff value
C
	    IF ( ( iret .eq. 0 ) .and. ( nwind .gt. 0 ) ) THEN
	       IF ( lstwnd .ne. IMISSD ) THEN
	          IF ( lstwnd .le. iwndht) THEN
	             IF ( ERMISS ( wdata ( 2, 1 ) ) ) THEN
		        drpwnd = .true.
	             END IF
	          END IF
	       END IF
	    END IF
        END IF
C*
	RETURN
	END
