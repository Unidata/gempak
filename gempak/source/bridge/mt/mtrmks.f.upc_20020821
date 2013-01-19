	SUBROUTINE MT_RMKS ( strrmk, numrmk, iret )
C************************************************************************
C* MT_RMKS			 				        *
C*								        *
C* This subroutine decodes the remarks fields of a METAR report.	*
C* The fields will be stored in common.					*
C*								        *
C* MT_RMKS ( STRRMK, NUMRMK, IRET )					*
C*								        *
C* Input parameters:						        *
C*	STRRMK(*)	CHAR*		Array of remarks	        *
C*	NUMRMK		INTEGER		Number of entries in strrmk     *
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRP06I)  REAL  		6 hour precip (inches)          *
C*	RIVALS(IRP03I)	REAL		3 hour precip (inches)          *
C*      RIVALS(IRSNOW)  REAL		Snow depth on ground (inches)   *
C*	RIVALS(IRP24I)	REAL		24 hour precip (inches)         *
C*	RIVALS(IRP01I)	REAL		1 hour precip (inches)          *
C*      RIVALS(IRMSUN)  REAL		Duration of sunshine (minutes)  *
C*      RIVALS(IRWEQS)  REAL		Water equiv. of ground snow (in)*
C*      RIVALS(IRAUTO)  REAL		Automatic station flag          *
C*      RIVALS(IRTMPC)  REAL		Temperature (C)                 *
C*      RIVALS(IRDWPC)  REAL		Dew point (C)                   *
C*      RIVALS(IRTDXC)  REAL		24 hour maximum temperature (C) *
C*      RIVALS(IRTDNC)  REAL		24 hour minimum temperature (C) *
C*      RIVALS(IRCTMX)	REAL		City maximum temperature (F)    *
C*      RIVALS(IRCTMN)	REAL		City minimum temperature (F)    *
C*      RIVALS(IRCTTP)	REAL		City hourly temperature (F)     *
C*	IRET		INTEGER		Return code		        *
C*					   0 = normal return 	        *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	11/95	Original author		                *
C* D. Kidwell/NCEP	 9/96   Added city temp, changed precip check   *
C*                              from missing to zero, changed logic     *
C*                              for 9 character temperature groups      *
C* D. Kidwell/NCEP	11/96   Added 'snow increasing rapidly'         *
C* K. Tyle/GSC	 	 1/97	Change precip check from zero to 	*
C*				missing; reorganized header and comments*
C* K. Tyle/GSC		 2/97	Changed error processing; refine hourly	*
C*				precip search				*
C* D. Kidwell/NCEP	 5/97   Removed ERMISS reference to integer arg *
C* D. Kidwell/NCEP	 6/97   ST_LSTR -> INDEX and ST_CRNM -> ST_INTG *
C* D. Kidwell/NCEP	 6/97   Added check for remark of length .ge. 40*
C* D. Kidwell/NCEP       4/98   New interface; cleaned up & reorganized;*
C*				added city max and min temp             *
C* D. Kidwell/NCEP       5/98   Corrected prologue units for city temps *
C* S. Chiswell/Unidata	 8/02	Added Canadian cloud types		*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	strrmk ( * )
C*
	CHARACTER 	remark*40
	LOGICAL		ok, itrace, cldtyp
C*
        INCLUDE         'ERMISS.FNC'
C-----------------------------------------------------------------------
 	iret = 0
	irmk = 1
	ib   = 1
C
	tempc = RMISSD
	dewpc = RMISSD
	cldtyp = .false.
C
	DO WHILE ( irmk .le. numrmk )
	    remark = strrmk ( irmk )
	    ier = -1
	    lenrmk = INDEX ( remark, ' ' ) - 1
	    IF ( lenrmk .lt. 0 ) lenrmk = 40
C
C*	    Check for Canadian METAR cloud type reporting
C
	    IF ( ( civals ( icstid )( 1:1 ) .eq. 'C' ) .and.
     +	       ( .not. cldtyp ) .and.
     +	       ( lenrmk .ge. 3 .and. lenrmk .le. 12 ) ) THEN
		CALL MT_CNCLTP ( remark, lenrmk, cldtyp, ier )
		IF ( ier .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'MT', ier,
     +			remark ( :lenrmk ), ierr )
		END IF
	    END IF
C
	    IF ( lenrmk .eq. 5 ) THEN
		IF ( remark ( 1:1 ) .eq. '6' ) THEN
C
C*		    Decode 3 and 6 hour precipitation amount.
C
		    ok = .true.
		    IF ( MOD ( irtarr ( 4 ), 6 ) .eq. 0 .and.
     +			 ERMISS ( rivals ( irp06i ) ) ) THEN
			jrpci = irp06i
		      ELSE IF ( ERMISS ( rivals ( irp03i ) ) .and.
     +				ERMISS ( rivals ( irp06i ) ) ) THEN
			jrpci = irp03i
		      ELSE
			ok = .false.
		    END IF
		    IF ( ok ) THEN
			itrace = .true.
			CALL MT_PRRM ( remark, itrace, prec36, ier )
			IF ( ier .eq. 0 ) THEN
			    rivals ( jrpci ) = prec36
			  ELSE
			    ier = 19
			    errflg = .true.
			    CALL DC_WLOG ( 2, 'MT', ier, 
     +					   remark ( :5 ), ierr )
			END IF
		    END IF
C
		  ELSE IF ( remark ( 1:2 ) .eq. '4/' ) THEN
C
C*		    Decode snow depth on ground.
C
		    IF ( ERMISS ( rivals ( irsnow ) ) ) THEN
			CALL ST_INTG ( remark ( 3:5 ), isnowd, ier )
			IF ( ier .eq. 0 ) THEN
			    rivals ( irsnow ) = FLOAT ( isnowd )
			  ELSE
			    ier = 20
			    errflg = .true.
			    CALL DC_WLOG ( 2, 'MT', ier, 
     +				           remark ( :5 ), ierr )
			END IF
		    END IF
C
		  ELSE IF ( remark ( 1:2 ) .eq. '8/' ) THEN
C
C*		    Decode low, middle and high cloud types.
C
		    IF ( .not. cldtyp ) THEN
		   	cldtyp = .true.
			CALL MT_CLTP ( remark, ier )
			IF ( ier .ne. 0 ) THEN
			    CALL DC_WLOG ( 2, 'MT', ier, 
     +					   remark ( :5 ), ierr )
			END IF
		    END IF
C
		  ELSE IF ( ( remark ( 1:2 ) .eq. '10' ) .or.
     +			    ( remark ( 1:2 ) .eq. '11' ) .or.
     +	  	 	    ( remark ( 1:2 ) .eq. '20' ) .or.
     +			    ( remark ( 1:2 ) .eq. '21' ) ) THEN
C
C*		    Decode 6-hourly maximum or minimum temperature.
C
		    CALL MT_MMT6 ( remark, ier )
		    IF ( ier .ne. 0 ) THEN
			errflg = .true.
			CALL DC_WLOG ( 2, 'MT', ier, remark (:5), ierr )
		    END IF
C
		  ELSE IF ( remark ( 1:1 ) .eq. '5' ) THEN
C
C*		    Decode 3-hourly pressure tendency.
C
		    IF ( ERMISS ( rivals ( irp03d ) ) ) THEN
			CALL MT_PTEN ( remark, ier )
			IF ( ier .ne. 0 ) THEN
			    errflg = .true.
			    CALL DC_WLOG ( 2, 'MT', ier, 
     +					   remark ( :5 ), ierr )
			END IF
		    END IF
C
		  ELSE IF ( remark ( 1:1 ) .eq. '7' ) THEN
C
C*		    Decode 24 hour precipitation amount.
C
		    IF ( ERMISS ( rivals ( irp24i ) ) ) THEN
			itrace = .false.
     			CALL MT_PRRM ( remark, itrace, prec24, ier )
			IF ( ier .eq. 0 ) THEN
			    rivals ( irp24i ) = prec24 
			  ELSE
			    ier = 23
			    errflg = .true.
			    CALL DC_WLOG ( 2, 'MT', ier, 
     +					   remark ( :5 ), ierr )
			END IF
		    END IF
C
                  ELSE IF ( remark ( 1:1 ) .eq. 'P'  .and. 
     +			    remark ( 1:2 ) .ne. 'PK' .and.
     +			    remark ( 1:2 ) .ne. 'PW') THEN
C
C*                  Decode hourly precipitation amount.
C
		    IF ( ERMISS ( rivals ( irp01i ) ) ) THEN
			itrace = .true.
			CALL MT_PRRM ( remark, itrace, prec01, ier )
			IF ( ier .eq. 0 ) THEN
			    rivals ( irp01i ) = prec01 
			  ELSE
			    ier = 24
			    errflg = .true.
			    CALL DC_WLOG ( 2, 'MT', ier, 
     +					   remark ( :5 ), ierr )
			END IF
		    END IF
C
		  ELSE IF ( remark ( 1:2 ) .eq. '98' ) THEN
C
C*		    Decode duration of sunshine.
C
		    IF ( ERMISS ( rivals ( irmsun ) ) ) THEN
			CALL ST_INTG ( remark ( 3:5 ), isunsh, ier )
			IF ( ier .eq. 0 ) THEN
			    rivals ( irmsun ) = FLOAT ( isunsh )
			  ELSE
			    ier = 25
			    errflg = .true.
			    CALL DC_WLOG ( 2, 'MT', ier, 
     +					   remark ( :5 ), ierr )
			END IF
		    END IF
C
		  ELSE IF ( remark .eq. 'WSHFT' ) THEN
C
C*		    Decode wind shift.
C
		    irmk = irmk + 1
		    IF ( irmk .le. numrmk) THEN
			remark = strrmk ( irmk )
			lenr = INDEX ( remark, ' ' ) - 1
			IF ( (lenr .eq. 2) .or. (lenr .eq. 4) ) THEN
			    CALL MT_WSHF ( remark, lenr, ier ) 
			    IF ( ier .eq. 0 ) THEN
C
C*				Skip over 'FROPA' if present.
C
				IF ( ( irmk .lt. numrmk ) .and.
     +				     ( strrmk ( irmk+1 ) (1:5) .eq.
     +				     'FROPA' ) ) irmk = irmk + 1
			      ELSE	
				ier    = 26
				errflg = .true.
				CALL DC_WLOG ( 2, 'MT', ier,
     +				               remark ( :lenr ), ierr )
			    END IF
			  ELSE
			    irmk = irmk - 1
			END IF
		    END IF
C
		END IF
C
	      ELSE IF ( lenrmk .eq. 6 ) THEN
C
		IF ( remark ( 1:2 ) .eq. '8/' ) THEN
C
C*		    Decode cloud types - alternate configuration with
C*		    / at end.
C
		    IF ( .not. cldtyp ) THEN
			cldtyp = .true.
			CALL MT_CLTP ( remark, ier )
			IF ( ier .ne. 0 ) THEN
			    errflg = .true.
			    CALL DC_WLOG ( 2, 'MT', ier, 
     +					   remark ( :6 ), ierr )
			END IF
		    END IF
C
		  ELSE IF ( remark ( 1:3 ) .eq. 'SLP' ) THEN
C
C*		    Decode sea level pressure.
C
		    IF ( ERMISS ( rivals ( irpmsl ) ) ) THEN 
		    	CALL MT_SLP ( remark ( 4:6 ), ier )
			IF ( ier .ne. 0 ) THEN
			    errflg = .true.
			    CALL DC_WLOG ( 2, 'MT', ier, 
     +					   remark ( :6 ), ierr )
			END IF
		    END IF
C
		  ELSE IF ( remark ( 1:3 ) .eq. '933' ) THEN
C
C*		    Decode water equivalent of snow on ground.
C
		    IF ( ERMISS ( rivals ( irweqs ) ) ) THEN
		        CALL ST_INTG ( remark ( 4:6 ), ih2oeq, ier )
		        IF ( ier .eq. 0 ) THEN
			    rivals ( irweqs ) = FLOAT ( ih2oeq ) * .1
			  ELSE	
			    ier = 28
			    errflg = .true.
			    CALL DC_WLOG ( 2, 'MT', ier, 
     +					   remark ( :6 ), ierr )
			END IF
		    END IF
C 
		  ELSE IF ( remark ( 1:6 ) .eq. 'SNINCR' ) THEN
C
C*		    Decode snow increasing rapidly.
C
		    irmk = irmk + 1
		    IF ( irmk .le. numrmk ) THEN
		    	remark = strrmk ( irmk )
			lenr = INDEX ( remark, ' ' )  - 1
			IF ( lenr .lt. 0 ) lenr = 40
			islsh = INDEX ( remark ( 1:lenr ), '/' )
			IF ( ( lenr .ge. 3) .and. ( lenr .le. 6 )
     +			     .and. ( islsh .ne. 0 ) ) THEN
			    IF ( ERMISS ( rivals ( irsnew ) ) ) THEN 
				CALL MT_SNOW ( remark, lenr, islsh,
     +					       ier )
				IF ( ier .ne. 0 ) THEN
				    errflg = .true.
				    CALL DC_WLOG ( 2, 'MT', ier, 
     +					       remark ( :lenr ), ierr )
				END IF
			    END IF
			  ELSE
			    irmk = irmk - 1
		   	END IF
		    END IF
		END IF
C
	      ELSE IF ( lenrmk .eq. 7 ) THEN
C
		IF ( remark ( 1:3 ) .eq. '933' ) THEN
C
C*		    Decode water equivalent of snow on ground - 
C*		    alternative configuration with / at end.
C
		    IF ( ERMISS ( rivals ( irweqs ) ) ) THEN
		        CALL ST_INTG ( remark ( 4:6 ), ih2oeq, ier )
		        IF ( ier .eq. 0 ) THEN
			    rivals ( irweqs ) = FLOAT ( ih2oeq ) * .1
			  ELSE	
			    ier    = 28
			    errflg = .true.
			    CALL DC_WLOG ( 2, 'MT', ier, 
     +					   remark ( :6 ), ierr )
			END IF
		    END IF
		END IF
C
	      ELSE IF ( lenrmk .eq. 3 ) THEN
C
		iao = 0
		IF ( remark .eq. 'AO1' ) THEN
C
C*		    Decode automated station type AO1.
C
		    iao = 3
C
		  ELSE IF (remark .eq. 'AO2') THEN
C
C*		    Decode automated station type AO2.
C
		    iao = 4
C
		  ELSE IF ( remark .eq. 'SLP' ) THEN
C
C*		    Have sea level pressure with embedded blank -
C*		    look at next field.
C
		    irmk = irmk + 1
		    IF ( irmk .le.numrmk ) THEN
			remark = strrmk ( irmk )
			lenr = INDEX ( remark, ' ' ) - 1
			IF ( lenr .eq. 3 ) THEN
		            IF ( ERMISS ( rivals ( irpmsl ) ) ) THEN
				CALL MT_SLP ( remark, ier ) 
				IF ( ier .ne. 0 ) THEN
				    errflg = .true.
				    CALL DC_WLOG ( 2, 'MT', ier,
     +						remark ( :lenr ), ierr )
				END IF
			    END IF
			  ELSE
			    irmk = irmk - 1
			END IF
		    END IF
		END IF
C
C*		Check to see if auto station type found.
C
		IF ( iao .ne. 0 ) THEN
		    IF ( ERMISS ( rivals ( irauto) ) ) THEN
			rivals ( irauto ) = FLOAT ( iao )
		      ELSE
			rivals ( irauto ) = FLOAT ( iao - 2 )
		    END IF
		    ier = 0
		END IF
C
	      ELSE IF ( lenrmk .eq. 9 ) THEN
C
		IF ( ( remark ( 1:1 ) .eq. 'T' ) .or.
     +		     ( remark ( 1:1 ) .eq. '4' ) ) THEN
		    IF ( ( ( remark ( 2:2 ) .eq. '0' ) .or.
     +			   ( remark ( 2:2 ) .eq. '1' ) ) .and.
     +			 ( ( remark ( 6:6 ) .eq. '0' ) .or.
     +			   ( remark ( 6:6 ) .eq. '1' ) ) ) THEN
			jer = -1
			ker = -1
			IF ( remark ( 1:1 ) .eq. 'T' ) THEN
C
C*			    Decode hourly temperature and dew point.
C
			    IF ( ERMISS ( tempc ) ) THEN 
      			        CALL MT_TPRM ( remark (2:5), tempc, jer)
			        IF ( jer .eq. 0 ) THEN
				    rivals ( irtmpc ) = tempc
			          ELSE
				    jer = 30
				END IF
			    END IF
C
			    IF ( ERMISS ( dewpc ) ) THEN
      			        CALL MT_TPRM ( remark (6:9), dewpc, ker)
			        IF ( ker .eq. 0 ) THEN
				    rivals ( irdwpc ) = dewpc
			          ELSE
				    ker = 30
				END IF
			    END IF
C
			  ELSE
C
C*			    Decode 24-hour max and min temperatures.
C
			    IF ( ERMISS ( rivals ( irtdxc ) ) ) THEN
      			        CALL MT_TPRM ( remark (2:5), tmm, jer )
			        IF ( jer .eq. 0 ) THEN
				    rivals ( irtdxc ) = tmm
			          ELSE
				    jer = 31
				END IF
			    END IF
C
			    IF ( ERMISS ( rivals ( irtdnc ) ) ) THEN
      			        CALL MT_TPRM ( remark (6:9), tmm, ker )
			        IF ( ker .eq. 0 ) THEN
				    rivals ( irtdnc ) = tmm
			          ELSE
				    ker = 31
				END IF
			    END IF
			END IF
C
			ier = MAX ( jer, ker )
			IF ( ier .gt. 0 ) THEN
			    errflg = .true.
			    CALL DC_WLOG ( 2, 'MT', ier, remark ( :9 ), 
     +				           ierr )
			END IF
		    END IF
		END IF
C
	      ELSE IF ( lenrmk .eq. 2 ) THEN
C
		IF ( remark .eq. 'PK' ) THEN
C
C*		    Decode peak wind.
C
		    irmk = irmk + 1
		    IF ( irmk .lt. numrmk ) THEN
			IF ( strrmk ( irmk ) ( 1:3 ) .eq. 'WND' ) THEN
			    irmk = irmk + 1
			    remark = strrmk ( irmk ) 
			    CALL MT_PKWD ( remark, ier )
			    IF ( ier .ne. 0 ) THEN
				lenr = INDEX ( remark, ' ' ) - 1
				IF ( lenr .lt. 0 ) lenr = 40
				CALL DC_WLOG ( 2, 'MT', ier, 
     +					       remark ( :lenr ), ierr )
			    END IF
			  ELSE
			    irmk = irmk - 1
			END IF
		    END IF
		END IF
C
	      ELSE IF ( lenrmk .eq. 4 ) THEN
C
		IF ( remark .eq. 'CITY' ) THEN
C
C*		    Decode city temperature - max, min or hourly.
C
		    irmk = irmk + 1
		    IF ( irmk .le. numrmk ) THEN
			IF ( strrmk ( irmk ) ( 1:3 ) .eq. 'MAX' ) THEN
			    jrct = irctmx
			  ELSE IF ( strrmk (irmk)(1:3) .eq. 'MIN' ) THEN
			    jrct = irctmn
			  ELSE
			    jrct = ircttp
			END IF
			IF ( jrct .ne. ircttp ) irmk = irmk + 1
			IF ( irmk .le. numrmk ) THEN
			    remark = strrmk ( irmk )
			    lenr = INDEX ( remark, ' ' ) - 1
			    IF ( lenr .eq. 2 .or. lenr .eq. 3 ) THEN
			        IF ( ERMISS ( rivals ( jrct ) ) ) THEN
      				    CALL ST_INTG ( remark ( :lenr ),
     +						   icityt, ier )
				    IF ( ier .eq. 0 ) THEN
				        rivals ( jrct ) = FLOAT (icityt)
				      ELSE
				        ier = 34
				        errflg = .true.
				        CALL DC_WLOG ( 2, 'MT', ier,
     +					     remark ( :lenr ), ierr )
				    END IF
				END IF
			      ELSE
				irmk = irmk - 1
			    END IF
			END IF
		    END IF
		END IF
	    END IF
C
	    IF ( (ier .ne. 0) .and. ( (ib + lenrmk) .le. 200 ) ) THEN  
		rmkund ( ib:ib + lenrmk ) = remark ( 1:lenrmk ) // ' '
		ib = ib + lenrmk + 1
	    END IF 
C
	    irmk = irmk + 1
	END DO
C*
	RETURN
	END
