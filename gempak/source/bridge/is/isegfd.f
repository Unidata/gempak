	SUBROUTINE IS_EGFD ( report, lenr, ifeggy, ipend, iret )
C************************************************************************
C* IS_EGFD 								*
C*									*
C* This subroutine finds and checks '.' to see if it is a possible	*
C* phenomenon delimeter in an EGGY, RJAA, NTAA, or MUHA report.  For	*
C* NTAA, numeric delimeters such as "2)" are searched first.		*
C*                                                                      *
C* IS_EGFD ( REPORT, LENR, IFEGGY, IPEND, IRET )		 	*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		International sigmet report     *
C*	LENR		INTEGER		Length of bulletin              *
C*	IFEGGY		INTEGER		Country ID:  1 if EGGY; 2 if	*
C*					RJAA; 3 if NTAA; 4 if MUHA	*
C*									*
C* Output parameters:							*
C*	IPEND		INTEGER		Pointer to delimeter in report	*
C*					  0 = delimeter not found	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 8/01						*
C* F. J. Yen/NCEP	 6/02	Rewritten for new NTAA inconsistencies	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
C*
	LOGICAL		done
C------------------------------------------------------------------------
	iret  = 0
C
	done = .false.
	ipend = 0
	IF ( ifeggy .eq. 3 ) THEN
C
C*	    If NTAA, then search for numbered delimeter (ie, "2)")
C
	    ip = 1
	    jp = MIN ( ip + 250, lenr )
	    DO WHILE ( .not. done )
	        ikr = INDEX ( report ( ip:jp ), ')' )
	        IF ( ikr .ne. 0 ) THEN
	            ika = ip + ikr - 1
		    IF ( report (ika-1:ika-1) .eq. '1' ) THEN
C
C*			Disregard this ')' since '1)' is at the 
C*			beginning of a phenonmenon.  Search for next ')'
C
		        ipr = INDEX ( report ( ika+1:jp ), ')')
		        ipa = ipr + ika 
		      ELSE
		        ipr = ikr
		        ipa = ika
		    END IF
		    IF ( ipr .ne. 0 ) THEN
		        CALL ST_ALNM ( report(ipa-1:ipa-1),ityp, ier )
		        IF ( ityp .eq. 1 ) THEN
			    ipend = ipa
			    done = .true.
		          ELSE
			    ip = ipa
		        END IF
		      ELSE
		        done = .true.
		    END IF
		    ip = ip + 1
		    IF ( ip .ge. lenr ) THEN
		        ipend = 0
		        done = .true.
		    END IF
	          ELSE
		    ipend = 0
		    done = .true.
		END IF
	    END DO
	END IF
	IF ( ipend .ne. 0 ) RETURN
C
C*	If not NTAA or if numbered delimeter not found for NTAA,
C*	then search for '.' delimeter
C
	done = .false.
	ip = 1
	DO WHILE ( .not. done )
	    ipnd = INDEX ( report ( ip:lenr ), '.' )
	    ipend = ip + ipnd - 1
	    IF ( ipnd .ne. 0 ) THEN
	        ip = ip + ipnd - 1
		IF ( ip .lt. lenr ) THEN
C
C*		    Test for '.' being a possible decimal point
C*		    instead of a delimeter.			
C
	            CALL ST_ALNM ( report(ip+1:ip+1), ityp, ier )
		    IF ( ityp .ne. 1 ) THEN
			IF ( ifeggy .ne. 3 ) THEN
			    done = .true.
			  ELSE
C
C*			    The following tests will weed out
C*			    most false NTAA delimeters:
C*			    If the next '.' is a decimal point
C*			    (part of an area definition) then the
C*			    '.' is treated as a delimeter.  If the
C*			    next '.' is not a decimal point and is
C*			    closerthan 45 characters, then it will
C*			    be considered a false delimeter.
C    
			    ipnx = INDEX ( report (ip+1:lenr), '.' )
			    ipna = ipnx + ip + 1			
    	            	    CALL ST_ALNM ( report(ipna:ipna),
     +					       ityp, ier )
		    	    IF ( ityp .eq. 1 ) THEN
			        done = .true.
			      ELSE
				IF ( ipnx .eq. 0 ) ipnx = lenr - ip
				IF ( ipnx .ge. 45 ) done = .true.
		            END IF
			END IF
		    END IF
		  ELSE
		    done = .true.
		END IF
	      ELSE
		done = .true.
		ipend = 0
	    END IF
C
	    ip = ip + 1
	    IF ( .not. done .and. ip .ge. lenr ) THEN
		ipend = 0
		done = .true.
	    END IF
	END DO
	IF ( ipend .ge.lenr ) ipend = lenr
C*
	RETURN
	END
