	SUBROUTINE RU_MLVL  ( report, lenr, wnknot, itopwn, above, drop,
     +			      ipoint, data, nlev,   iret )
C************************************************************************
C* RU_MLVL								*
C*									*
C* This subroutine decodes data for all mandatory levels from TTAA 	*
C* and TTCC reports.  The output data are ordered  PRES TEMP DWPT	*
C* DRCT SPED HGHT .							*
C*									*
C* RU_MLVL  ( REPORT, LENR, WNKNOT, ITOPWN, ABOVE, DROP, IPOINT, DATA,	*
C*            NLEV, IRET ) 						*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Station report			*
C*	LENR		INTEGER		Length of report		*
C*	WNKNOT		LOGICAL		Flag for speed in knots		*
C*	ITOPWN		INTEGER		Highest level reporting winds	*
C*	ABOVE		LOGICAL		Above 100 mb flag		*
C*	DROP		LOGICAL		Dropsonde flag                  *
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report		*
C*									*
C* Output parameters:							*
C*	DATA (6,NLEV)	REAL		Mandatory station data		*
C*	NLEV		INTEGER		Number of levels returned	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/86						*
C* M. desJardins/GSFC	 8/87	GEMPAK 4				*
C* B. Doty/RDS		10/87						*
C* M. desJardins/GSFC	12/87						*
C* K. Brill/NMC		01/92	Added new 925 mb mandatory level;	*
C*				Changed check for unexpected lvl to look*
C*				at current field before moving on;	*
C*				Do not reject underground man levels;	*
C*				Use RU_GFLD to scan ahead for valid lvl *
C* S. Jacobs/EAI	 6/92	Fixed checking to avoid an infinite	*
C*				loop. (J. Nielsen)			*
C* D. Kidwell/NCEP	11/98	Fixed for itopwn = 0 above 100 mb and   *
C*				redundant incrementation of level       *
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *
C*                              DATA statement                          *
C* D. Kidwell/NCEP	 2/01	Added check for tropopause or max wind  *
C* D. Kidwell/NCEP	 2/05	CSC for drop, added to RU_MAND call     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'rucmn.cmn'
C*
	CHARACTER*(*)	report
	LOGICAL		wnknot, above, drop
C*
	REAL		rdata ( 6 )
	REAL		data ( 6, * )
	CHARACTER	pp*2, field*10
	LOGICAL		good, scan
C*
	LOGICAL		ENDRPT, ENDTRP, ENDMXW
C*
	CHARACTER*2	pval ( 11, 2 )
	REAL		pres ( 11, 2 )
	INCLUDE		'ERMISS.FNC'
	DATA		pval  / '00', '92', '85', '70', '50', '40',
     +				'30', '25', '20', '15', '10', 
     +				'70', '50', '30', '20', '10',
     +				'07', '05', '03', '02', '01', 'xx' /
	DATA		pres / 1000., 925., 850., 700., 500., 400., 
     +				300., 250., 200., 150., 100.,
     +				 70.,  50.,  30.,  20.,  10.,
     +				  7.,   5.,   3.,   2.,   1., 0. /
C*
C
C*	Function to check for end of message.
C
	ENDRPT ( field, lenf, ier ) =  
     +				( ( lenf .gt. MAXFLN )  .or.
     +				  ( ier  .ne. 0 )  .or. 
     +				  ( field (1:2) .eq. '66' )   .or.
     +				  ( field (1:2) .eq. '77' )   .or.
     +				  ( field (1:2) .eq. '88' )   .or.
     +				  ( field (1:5) .eq. '51515' ) )
	ENDTRP ( field, lenf, ier ) = ( field (1:2) .eq. '88' )
	ENDMXW ( field, lenf, ier ) = ( ( field (1:2) .eq. '66' ) .or.
     +					( field (1:2) .eq. '77' ) )
C------------------------------------------------------------------------
C*	Initialize variables.
C
	iret = 0
	nlev = 0
	IF  ( above )  THEN
	    iabv = 2
	  ELSE
	    iabv = 1
	END IF
	iskpct = 0 
C
C*	Compute the top pressure reporting wind data.
C*	If ITOPWN is missing, set TOPWND to a large value so that
C*	all the winds will be assumed to be missing.
C
	IF  ( itopwn .eq. IMISSD )  THEN 
	    topwnd = 2000.0
	  ELSE IF  ( itopwn .gt. 0 )  THEN
	    IF ( above ) THEN
		topwnd = FLOAT ( itopwn ) * 10.
		IF  ( itopwn .eq. 1 )  topwnd = 0.
	      ELSE
		topwnd = FLOAT ( itopwn ) * 100.
	    END IF
	  ELSE IF  ( itopwn .eq. 0 )  THEN
	    IF ( .not. above ) THEN
	        topwnd = 1000.
	      ELSE
		topwnd = 0.
	    END IF
	  ELSE
	    topwnd = 2000.
	END IF
C
C*	First expected value is level 1.
C
	level = 1
C
C*	Loop through the data until an invalid level is reached or
C*	until the last level is processed.
C
	DO WHILE  ( level .le. 11 )
C
C*	    Set flag indicating report is ok.
C
	    good = .false.
C
C*	    Get first field which contains the pressure.
C
	    ipsave = ipoint
	    CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, ier )
C
C*	    Check for the end of message.
C
	    IF  ( ENDRPT ( field, lenf, ier ) )  THEN
		IF ( .not. ( ENDTRP ( field, lenf, ier ) .or.
     +			     ENDMXW ( field, lenf, ier ) ) ) THEN
		    ipoint = lenr + 1
		  ELSE
		    ipoint = ipsave
		END IF
		RETURN
	      ELSE
		pp = field ( 1 : 2 )
	    END IF
C
C*	    Do special check for absence of 925 level.
C
	    IF ( pp .ne. pval ( level, iabv ) .and.
     +		 pp .eq. '85'                 .and.
     +           level .eq. 2 ) level = level + 1
C
C*	    Check if this is the expected level.
C
	    IF  ( pp .eq. pval ( level, iabv ) )  THEN
C
C*		This was the expected level.  Go ahead and parse it.
C
		CALL RU_MAND  ( field, report, lenr, wnknot, topwnd, 
     +				above, drop, ipoint, rdata, ier )
		IF  ( ier .eq. 0 )  good = .true. 
C
C*		If there is data, add this level to data.
C
		IF ( ( .not. ERMISS ( rdata (1) ) ) .and. 
     +		     ( .not. ERMISS ( rdata (6) ) ) ) THEN
		    nlev = nlev + 1
		    DO ii = 1, 6
			data ( ii, nlev ) = rdata ( ii )
		    END DO
		    good = .true.
		  ELSE IF  ( ( .not. ERMISS ( rdata (1) ) ) .and.
     +			     ( .not. good ) )  THEN
		    level = level + 1
		END IF
C
C*		If we are at the end of report, stop now.
C
		IF  ( ier .eq. -5 )  RETURN
	    END IF
C
C*	    Advance to the next level.
C
	    IF  ( good )  THEN
		level = level + 1
C
C*		If the last level was bad in some way then find next
C*		level.
C
	      ELSE 
C
C*		Scan for the next valid level.
C
		scan = .true.
		DO WHILE  ( scan )
		    ilev = level
		    DO WHILE  ( ( ilev .le. 10 ) .and. ( scan ) ) 
			IF  ( pp .eq. pval ( ilev, iabv ) )  THEN
C
C*			    A valid level header has been found.
C*			    We will accept this as the next valid
C*			    level if the level after this one is
C*			    also valid.
C
			    p = pres ( ilev, iabv )
			    IF  ( p .ge. topwnd )  THEN
				iword = 3
			      ELSE
				iword = 2
			    END IF
			    ipsv = ipoint
			    DO isr = 1, iword
				CALL RU_GFLD ( report, lenr, ipsv,
     +					       field, lenf, ier )
			    END DO
			    IF  ( ( ier .eq. 0 ) .and. 
     +				  ( field ( 1:2 ) .eq. 
     +					pval ( ilev + 1, iabv ) ) )  THEN
				level  = ilev
				scan   = .false.
				ipoint = ipsave
			    END IF
			END IF
			ilev = ilev + 1
		    END DO
C
C*		    Check the number of fields skipped.
C
		    IF ( scan ) THEN
		      iskpct = iskpct + 1
		      IF  ( iskpct .gt. MAXSKP )  THEN
			RETURN
		      END IF
C
C*		      Save pointer and then get next field.
C
		      ipsave = ipoint
		      CALL RU_GFLD  ( report, lenr, ipoint, field, lenf,
     +				      ier )
		      IF  ( ENDRPT ( field, lenf, ier ) )  THEN
			ipoint = lenr + 1
			RETURN
		      END IF
C
C*		      Get pressure to check for valid value.
C
		      pp   = field ( 1 : 2 )
		    END IF 
		END DO
	    END IF    
	END DO
C*
	RETURN
	END
