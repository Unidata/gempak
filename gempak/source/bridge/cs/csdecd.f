	SUBROUTINE CS_DECD ( report, lenr, iotarr, icorr, itest, tissue,
     +			     gemfil, stntbl, iadstn, maxtim, org, iret )
C************************************************************************
C* CS_DECD 								*
C*									*
C* This subroutine processes a single convective sigmet and convective  *
C* outlook bulletin.							*
C*                                                                      *
C* CS_DECD ( REPORT, LENR, IOTARR, ICORR, ITEST, TISSUE, GEMFIL, STNTBL,*
C*	     IADSTN, MAXTIM, ORG, IRET )                                *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Convective sigmet bulletin      *
C*	LENR		INTEGER		Length of bulletin              *
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*	ICORR		INTEGER		Correction flag                 *
C*	ITEST		INTEGER		Test flag                       *
C*	TISSUE		CHAR*  		Bull. issue time, GEMPAK format *
C*	GEMFIL		CHAR*		Output file name template       *
C*	STNTBL		CHAR*		Station table                   *
C*	IADSTN		INTEGER		Number of additional stations   *
C*	MAXTIM 		INTEGER		Number of hours prior to CURTIM *
C*	ORG		CHAR*    	Originating station             *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	 8/02	Created					*
C* A. Hardy/NCEP	 8/02	Added check for region			*
C* A. Hardy/NCEP	 1/04   Modified check on nil type reports	*
C* A. Hardy/NCEP	 5/04	Added checks if either type report exist*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	report, tissue, gemfil, stntbl, org
	INTEGER		iotarr (*)
C*
	CHARACTER	parms (MMPARM)*4, filnam*132,
     +                  bbb*3, strbuf*250, outar*(DCMXBF), astart*20, 
     +			reg*2, conar*(DCMXBF)
	LOGICAL		done
C------------------------------------------------------------------------
	iret  = 0
C
C*	Ensure that report is upper case.
C
	CALL ST_LCUC ( report, report, ier )
C
        iend   = 0
        ifrom  = 0
        ilvl   = 0
	ibeg   = 1
        iflsrc = 2
	done   = .false.
	strbuf = report ( :160 )
C
	DO WHILE ( .not. done )
	    CALL CS_HDR ( strbuf, iotarr, astart, reg, bbb, iptr, ierr )
            IF ( reg .eq. 'P' ) ierr = -1
C
C*          Check if it is a corrected report.
C
	    IF ( ierr .eq. 0 ) THEN
	        IF ( icorr .eq. 0 ) THEN
                    IF ( ( bbb  .eq. 'COR' ) .or.
     +                   ( bbb ( :2 ) .eq. 'CC' ) ) THEN
                        icor = 1
		      ELSE
		        icor = 0
                    END IF
	          ELSE
		    icor = icorr
	        END IF
	      ELSE
		done = .true.
		CALL DC_WLOG ( 2, 'DCCSIG', ierr, strbuf ( :72 ), ier )
	    END IF

            IF ( ierr .eq. 0 ) THEN
C
C*              Separate Convective section from outlook section.
C
	        icon = INDEX ( report( iptr:lenr ),'CONVECTIVE SIGMET')
	        iout = INDEX ( report( iptr:lenr ),'OUTLOOK VALID' )
C
C*		Check if Outlook is missing or badly formed.
C
                IF ( iout .eq. 0 ) THEN
	            iwst = INDEX ( report( iptr:lenr ),'WST ISSUANCE')
		END IF
C
                IF ( icon .gt. 0 ) THEN
                    IF ( ( iwst .gt. 0 ) .and. ( iout .eq. 0  ) ) THEN
                        conar =  report ( iptr+icon-1:iptr+iwst-2)
                      ELSE IF ((iwst .eq. 0) .and. ( iout .eq. 0))THEN
                        conar =  report ( iptr+icon-1:lenr)
                      ELSE 
                        conar =  report ( iptr+icon-1:iptr+iout-2)
                    END IF
                    CALL ST_LSTR ( conar, lenc, ier)
                END IF 
C
C*              Have Outlook segment.
C
                IF ( iout .gt. 0 ) THEN
                    outar =  report ( iptr+iout-1:)
                    CALL ST_LSTR ( outar, leno, ier)
                END IF 
C
C
C*	        Make a file name from the template and the 
C*	        time.  Open the file as part of the open 
C*	        file list.
C
 	        CALL FL_MNAM ( tissue, gemfil, filnam, ier )
 	        CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn, maxtim, 
     +			   lunf, nparm, parms, ierr )
            END IF 
C
	    DO WHILE ( ( ierr .eq. 0 ) .and. ( .not. done ) ) 
	 	IF ( ( icon .gt. 0 ) .or. ( iout .gt. 0 ) ) THEN
C
C*                  Break apart the Convective Sigmet section.        
C
                    IF ( icon .gt. 0 ) THEN
                        inon   = 0
		        inon = INDEX ( conar( :lenr ), '...NONE' )
                        iptr = 1
                        CALL CS_CONV ( lunf, conar, lenc, lenr, iotarr, 
     +                             iptr, astart, icor, inon, reg, ier )
                    END IF 
C
C*                  Break apart the Convective Outlook section.        
C
                    IF ( iout .gt. 0 ) THEN
                        inon   = 0
		        inon = INDEX ( outar( :lenr ), 'NOT EXPD' )
                        iptr = 1
                        CALL CS_OTLK ( lunf, outar, leno, reg, 
     +				   iotarr, icor, inon, ier ) 
                    END IF
                    done = .true.        
		  ELSE
		    done = .true.
                END IF 
	    END DO
	    IF ( ierr .lt. 0 ) done = .true.
	END DO
C
 	CALL DC_FCLS ( ier )
C*
	RETURN
	END
