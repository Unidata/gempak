	SUBROUTINE MT_DECD ( report, lenr, irpnt, iret )
C************************************************************************
C* MT_DECD                                                             	*
C*                                                                      *
C* This subroutine will decode a single METAR or SPECI report.		*
C*									*
C* Upon entering the routine, the pointer IRPNT points to the first	*
C* character following the time field.					*
C*									*
C* The report is checked for remarks; if they exist, the remarks are	*
C* split from the main body of the report.				*
C*									*
C* Each field in the main body of the report is translated by a 	*
C* separate subroutine.  Following the attempted translation, MT_POST	*
C* or MT_POS2 checks if the field was translated and sets the pointer	*
C* in the report appropriately.						*
C*									*
C* Miscoded fields are denoted by positive values of IRTD.		*
C* 								        *
C* MT_DECD ( REPORT, LENR, IRPNT, IRET )			        *
C*								        *
C* Input parameters:						        *
C*	REPORT		CHAR*		METAR or SPECI report           *
C*	LENR		INTEGER		Report length		        *
C*	IRPNT		INTEGER		Pointer after time field	*
C* Output parameters:						        *
C*	IRET		INTEGER		Return code                     *
C*					  0 = normal return             *
C*					 -1 = zero length - no report   *
C*					 -2 = No fields in main body	*
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	 3/95				                *
C* K. Tyle/GSC		 1/97	Reorganized header and comments; 	*
C*				changed LRET to IRET; change calls to	*
C*				DC_WLOG; eliminate call to MT_OUT	*
C* K. Tyle/GSC		 1/97	Eliminate / change parm. 		*
C*				initializations; call DC_WLOG		*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* K. Tyle/GSC		 2/97	Check for "RMKS"			*
C* D. Kidwell/NCEP	 6/97   Replaced ST_LSTR with INDEX             *
C* D. Kidwell/NCEP	 6/97   Added check for string of length .ge. 40*
C* D. Kidwell/NCEP	 4/98   Used parameters for max no. of fields;  *
C*				changed error processing; new interface;*
C*				do not decode Mexican remarks           *
C* D. Kidwell/NCEP	 3/00   Restored decode of Mex remarks for slp  *
C* Yen&Caruso Magee/NCEP 2/04   Checked for decoding flag of 2 (slashes)*
C* H. Zeng/SAIC		08/07	replaced ST_CLST with ST_CLSL		*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	report
C*
	INTEGER 	idecod ( 100 )
	CHARACTER*40  	strarr ( 100 ), strrmk ( 90 )
	LOGICAL		dirvis, donvis, donrvr, donwea, donsky, twofld
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for zero length.
C
	IF ( irpnt .gt. lenr ) THEN
	    iret = -1
	    CALL DC_WLOG ( 2, 'MT', iret, ' ', ier )
	    RETURN
	END IF
C
 	nexp = 100
C
C*	Break down the report into an array.
C
	CALL ST_CLSL ( report ( irpnt:lenr ), ' ', ' ', nexp, strarr,
     +	       	       num, mret)
	donlin = . false.
	iptr = 1
	errflg = .false.
C
C*	Initialize step-through processing values.
C
	idcrvr = 0
	idcwea = 0
	idcsky = 0
	idcvis = 0
	donrvr = .false.
	donwea = .false.
	donsky = .false.
	donvis = .false.
	DO i = 1, nexp    
	    idecod ( i ) = 0
	END DO
	imhere = 1
	iquit = 1
C
C*	Look for remarks prefaced by 'RMK' or 'RMKS'.
C
	numrmk = 0
	CALL ST_FIND ( 'RMK', strarr, num, ipos, nret )		
	IF ( ipos .eq. 0 ) CALL ST_FIND ( 'RMKS', strarr, num,
     +					   ipos, nret )	
	IF ( ipos .gt. 0 ) THEN 
C
C*	    Split out remarks from body of report.
C
	    numrmk = num - ipos
	    DO i = 1, numrmk
		strrmk ( i ) = strarr ( ipos + i )
	    END DO
	    num = ipos - 1
	END IF
C
C*	Treat 'NIL' as end of body of report, as well as 
C*	Trend Forecast indicators 'NOSIG', 'BECMG' and 'TEMPO'.
C
	CALL ST_FIND ( 'NIL', strarr, num, ipos, nret )
	IF ( ipos .ge. 1 ) num = ipos - 1
	CALL ST_FIND ( 'NOSIG', strarr, num, ipos, nret )
	IF ( ipos .ge. 1 ) num = ipos - 1
	CALL ST_FIND ( 'BECMG', strarr, num, ipos, nret )
	IF ( ipos .ge. 1 ) num = ipos - 1
	CALL ST_FIND ( 'TEMPO', strarr, num, ipos, nret )
	IF ( ipos .ge. 1 ) num = ipos - 1
C
C*	Check for fields which can be dropped.
C		
	IF ( num .gt. 0 )
     +      CALL MT_DROP ( strarr, num, strarr, num, nret )
	IF ( iptr .gt. num ) THEN
	    donlin = .true.
	    iret = -2
	    CALL DC_WLOG ( 4, 'MT', iret, ' ', ier )
	END IF
C
C*	Begin sequential decoding of groups.
C
	DO WHILE ( .not. donlin )
C
C*          Check for 'AUTO' or 'COR' type group.
C
	    IF ( imhere .eq. 1 ) THEN
	        CALL MT_AUTO ( strarr ( iptr ), idecod ( iptr ), irtd )
		IF ( irtd .gt. 0 ) 
     +			CALL MT_ELOG ( strarr ( iptr ), irtd, ier ) 
		CALL MT_POST ( 2, idecod ( iptr ), nret )
	    END IF
C
C*	    Wind direction and speed group.
C
	    IF ( imhere .eq. 2 ) THEN
	        CALL MT_WIND ( strarr ( iptr ), idecod ( iptr ), irtd )
		IF ( irtd .gt. 0 )
     +			CALL MT_ELOG ( strarr ( iptr ), irtd, ier ) 
		CALL MT_POST ( 3, idecod ( iptr ), nret )
	    END IF
C
C*	    Variable wind direction group.
C
	    IF ( imhere .eq. 3 ) THEN
		CALL MT_VWND ( strarr ( iptr ), idecod ( iptr ), irtd)
		IF ( irtd .gt. 0 )
     +			CALL MT_ELOG ( strarr ( iptr ), irtd, ier ) 
		CALL MT_POST ( 4, idecod ( iptr ), nret )
	    END IF
C
C*	    Visibility group(s) (may require 2 fields if fractional) -
C*	    or up to 2 groups (if directional visibility).
C
 	    IF ( imhere .eq. 4 ) THEN
		idcvis = idcvis + 1
		IF ( idcvis .gt. MXVISB ) donvis = .true.
		DO WHILE ( .not. donvis )
		    IF ( iptr .eq. num ) strarr ( iptr + 1 ) = ' '
		    CALL MT_VISB ( strarr ( iptr ), strarr ( iptr + 1 ),
     +			 idcvis, idecod ( iptr ), twofld, dirvis, irtd )
		    IF ( idecod (iptr) .eq. 2 ) donvis = .true.
		    IF ( irtd .gt. 0 )
     +			CALL MT_ELOG ( strarr ( iptr ), irtd, ier ) 
		    CALL MT_POS2 ( 4, idecod ( iptr ),
     + 			 MXVISB, idcvis, donvis, nret )
	    	    IF ( .not. donvis .and. ( idecod (iptr -1) .ne. 0 )
     +				.and. twofld ) THEN
		        idecod ( iptr ) = idecod ( iptr -1 )
		        CALL MT_NEXT ( nret )
		    END IF
               	    IF ( ( .not. dirvis ) .and.
     +		         ( idecod ( iptr -1 ) .ne. 0 ) ) donvis = .true.
		END DO
		IF ( .not. donlin ) imhere = 5
            END IF
C   
C*	    Runway visual range group(s) - up to 4.
C
 	    IF ( imhere .eq. 5 ) THEN
		idcrvr = idcrvr + 1
		IF ( idcrvr .gt. MXRWVR ) donrvr = .true.
		DO WHILE ( .not. donrvr )
	            CALL MT_RVR4 ( strarr ( iptr ),
     +				idcrvr, idecod ( iptr ), irtd )
		    IF ( irtd .gt. 0 )
     +			CALL MT_ELOG ( strarr ( iptr ), irtd, ier ) 
		    CALL MT_POS2 ( 5, idecod ( iptr ),
     +				MXRWVR, idcrvr, donrvr, nret )
		END DO
		IF ( .not. donlin ) imhere = 6
            END IF
C
C*          Weather phenomena group(s) - up to 3.
C
	    IF ( imhere .eq. 6 ) THEN
		idcwea = idcwea + 1
		IF ( idcwea .gt. MXWTHR ) donwea = .true.
		DO WHILE ( .not. donwea )
	            CALL MT_WEA3 ( strarr ( iptr ),
     +				idcwea, idecod ( iptr ), irtd )

		    IF ( irtd .gt. 0 )
     +			CALL MT_ELOG ( strarr ( iptr ), irtd, ier ) 
		    IF ( idecod ( iptr ) .eq. 2 ) donwea = .true.
		    CALL MT_POS2 ( 6, idecod ( iptr ),
     +				MXWTHR, idcwea, donwea, nret )
		END DO
		IF ( .not. donlin ) imhere = 7
	    END IF
C
C*          Sky condition group(s) - up to 6.
C
            IF ( imhere .eq. 7 ) THEN
		idcsky = idcsky + 1
C
C*     		First check for sky groups which need to be split up.
C
		IF ( idcsky .eq. 1 ) THEN
		    CALL MT_SSPL ( iptr, strarr, num, strarr, num, jret)
        	    IF ( jret .eq. 1 ) THEN
	    		CALL MT_SSPL ( iptr, strarr, 
     +					num, strarr, num, jret )
		    END IF
		END IF
C
		IF ( idcsky .gt. MXSKYC ) donsky = .true.
		DO WHILE ( .not. donsky )
	            CALL MT_SKY6 ( strarr ( iptr ),
     +				strarr ( iptr + 1 ), idcsky,
     +				idecod ( iptr ), twofld, irtd )
		    IF ( irtd .gt. 0 )
     +			CALL MT_ELOG ( strarr ( iptr ), irtd, ier ) 
		    IF ( idecod ( iptr ) .eq. 2 ) donsky = .true.
		    CALL MT_POS2 ( 7, idecod ( iptr ),
     +				MXSKYC, idcsky, donsky, nret )
		    IF ( .not. donsky .and. ( idecod (iptr -1) . ne. 0)
     +				  .and. twofld ) THEN
			idecod ( iptr ) = idecod ( iptr - 1 )
			CALL MT_NEXT ( nret )
		    END IF
		END DO
		IF ( .not. donlin ) imhere = 8 
	    END IF
C
C*	    Temperature and dewpoint group.
C
	    IF ( imhere .eq. 8 ) THEN
	        CALL MT_TDPT ( strarr ( iptr ), idecod ( iptr ), irtd )
		IF ( irtd .gt. 0 )
     +			CALL MT_ELOG ( strarr ( iptr ), irtd, ier ) 
		CALL MT_POST ( 9, idecod ( iptr ), nret)
	    END IF
C
C*	    Altimeter setting group--last group before RMKs begin.
C
	    IF ( imhere .eq. 9 ) THEN
	        CALL MT_ALTM ( strarr ( iptr ), idecod ( iptr ), irtd )
		IF ( irtd .gt. 0 )
     +			CALL MT_ELOG ( strarr ( iptr ), irtd, ier ) 
		CALL MT_POST ( 10, idecod ( iptr ), nret)
	    END IF
C
	    IF ( ( .not. donlin ) .and. ( idecod ( iptr) .eq. 0 ) ) THEN
C
C*		Go back for more fields.
C
		imhere = iquit
		iptr   = iptr + 1
		IF ( iptr .gt. num ) donlin = .true.
C
C*		The following lines insure correct handling of an
C*		undecodable in the middle of repeated like groups.
C
		IF ( ( imhere .le. 4 ) .and. ( idcvis .le. MXVISB ) )
     +		       donvis = .false.
		IF ( ( imhere .le. 5 ) .and. ( idcrvr .le. MXRWVR ) )
     +		       donrvr = .false.
		IF ( ( imhere .le. 6 ) .and. ( idcwea .le. MXWTHR ) ) 
     +		       donwea = .false.
		IF ( ( imhere .le. 7 ) .and. ( idcsky .le. MXSKYC ) ) 
     +		       donsky = .false.
            END IF
	END DO
C
C*	Decode remarks if they exist.
C	
	IF ( numrmk .gt. 0 ) THEN
	    IF ( contry .ne. 'MX  ' ) THEN
		CALL MT_RMKS ( strrmk, numrmk, ier )
	      ELSE
		CALL MT_RMMX ( strrmk, numrmk, ier )
	    END IF
	END IF
C
	IF ( ( num .gt. 0 ) .or. ( numrmk .gt. 0 ) ) THEN
	    IF ( errflg ) THEN
C
C*	        Write out entire report if any coding error was found.
C
		logmsg = report ( 1:lenr )
		CALL DC_WLOG ( 2, 'MT', 15, logmsg, ier )
	      ELSE
C
C*		Write out entire report if undecodable field found.
C
                ip = 1
                DO WHILE ( ip .le. num )
                    IF ( idecod ( ip ) .eq. 0 ) THEN
			logmsg = report ( 1:lenr )
			CALL DC_WLOG ( 4, 'MT', 16, logmsg, ier )
                        ip = num + 1
		      ELSE
                        ip = ip + 1
                    END IF
                END DO
	    END IF
	END IF
C*
	RETURN
	END
