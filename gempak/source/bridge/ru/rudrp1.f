	SUBROUTINE RU_DRP1  ( report, lenr, ipoint, istnm, lstwnd, iret)
C************************************************************************
C* RU_DRP1								*
C*									*
C* This subroutine decodes the dropsonde id from the 61616 section and	*
C* the height of last reported wind from the 62626 section.		*
C* The dropsonde id is 7 digits, defined as follows:                    *
C*	 JJHHNNN where JJ = observation number                          *
C*		       HH = report hour					*
C*		      NNN = aircraft identification number (3 digits if *
C*			    Air Force, 00N if NOAA or NASA)             *
C*									*
C* RU_DRP1  ( REPORT, LENR, IPOINT, ISTNM, LSTWND, IRET )		*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Report				*
C*	LENR		INTEGER		Length of report		*
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report           *
C*	ISTNM           INTEGER		Dropsonde id (station number)	*
C*									*
C* Output parameters:							*
C*	LSTWND		INTEGER		Height of last reported wind	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*					 -4 = bad/missing dropsonde id  *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/01						*
C* D. Kidwell/NCEP	 7/01	Allow NASA aircraft and missing ob no.	*
C* D. Kidwell/NCEP	 8/01	Allow one-digit ob no.; set ob 0 missing*
C* m.gamazaychikov/SAIC	07/05	Added lstwnd to the output parameters	*
C* S. Jacobs/NCEP	 8/10	Added new patterns for aircraft IDs	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
C*
	CHARACTER 	field*10
	LOGICAL		restor, good
C------------------------------------------------------------------------
	iret = 0
        lstwnd = IMISSD
	good = .true.
C
C*	Reset ipoint if it is already at report end.
C
	IF ( ipoint .gt. lenr ) THEN
	    restor = .true.
	    ipsav  = ipoint
	    ipoint = 1
	  ELSE
	    restor = .false.
	END IF
C
C*	Find the 61616 group which precedes the dropsonde id.
C
	idrop = INDEX ( report ( ipoint:lenr ), '61616' )
	IF ( idrop .eq. 0 ) THEN
	    good = .false.
	  ELSE
C
C*	    Get the aircraft identifier group.
C
	    ipoint = ipoint + idrop + 4
	    CALL RU_GFLD ( report, lenr, ipoint, field, lenf, ier )
	    IF ( ( ier .eq. 0 ) .and. ( lenf .eq. 5 ) ) THEN
C
C*	    Determine if this is an Air Force, NOAA or NASA aircraft.
C
	        IF ( ( field ( 1:4 ) .eq. 'NOAA' ) .or.
     +		     ( field ( 1:4 ) .eq. 'NASA' ) )  THEN
		    ibeg  = 5
		    iend  = 5
	          ELSE IF ( field ( 1:2 ) .eq. 'AF' ) THEN
		    ibeg  = 3
		    iend  = 5
	          ELSE IF ( field ( 1:2 ) .eq. 'NA' ) THEN
		    ibeg  = 3
		    iend  = 5
	          ELSE IF ( field ( 1:1 ) .eq. 'N' .and.
     +			    field ( 5:5 ) .eq. 'F' ) THEN
		    ibeg  = 2
		    iend  = 4
	          ELSE
		    good  = .false.
	        END IF
	      ELSE
		good = .false.
	    END IF
	END IF
C
C*	Get the aircraft identification number.  On input, istnm
C*	contains the report hour.
C
	IF ( good ) THEN
	    CALL ST_INTG ( field ( ibeg:iend ), ident, ier )
	    IF ( ier .eq. 0 ) THEN
		istnm = istnm * 1000 + ident
	      ELSE
		good = .false.
	    END IF
	END IF
C
	IF ( good ) THEN
C
C*	    Look for OB, which precedes the observation number.
C
	    iob    = INDEX ( report ( ipoint:lenr ), 'OB' )
	    IF ( iob .gt. 0 ) THEN
C
C*	        Get the observation number.
C
	        ipoint = ipoint + iob + 1
	        CALL RU_GFLD ( report, lenr, ipoint, field, lenf, ier )
	        IF ( ( ier .eq. 0 ) .and. 
     +		     ( ( lenf .eq. 2 ) .or. ( lenf .eq. 1 ) ) ) THEN
	            CALL ST_INTG ( field ( :lenf ), iobsvn, ier )
	            IF ( ( ier .eq. 0 ) .and. ( iobsvn .gt. 0 ) ) THEN
			istnm = iobsvn * 100000 + istnm
		      ELSE
			good = .false.
		    END IF
		  ELSE
		    good = .false.
	        END IF
	      ELSE
		good = .false.
	    END IF
C
C*	    If observation number is bad or missing, use 99.
C
	    IF ( .not. good ) THEN
		good  = .true.
		istnm = 9900000 + istnm
	    END IF
	END IF
C
	IF ( good ) THEN
C
C*          Look for section 62626.
C
            iob    = INDEX ( report ( ipoint:lenr ), '62626' )
            IF ( iob .gt. 0 ) THEN
C
C*              Section 62626 found, look for LST WND.
C
                ipoint = ipoint + iob + 4
                iob = INDEX ( report ( ipoint:lenr ), 'LST WND' )
                IF ( iob .gt. 0 ) THEN
C
C*                 LST WND found, decode and get the height value.
C
                   ipoint = ipoint + iob + 6
                   CALL RU_GFLD ( report, lenr, ipoint, field, lenf, 
     +                            ier )
                   IF ( ier .eq. 0 ) THEN
                      CALL ST_INTG ( field ( :lenf ), lstwnd, ier )
                   END IF
                END IF
            END IF
	END IF
	IF ( restor ) ipoint = ipsav
	IF ( .not. good ) THEN
	    iret = -4
	    istnm = IMISSD
	END IF
C*
	RETURN
	END
