	SUBROUTINE IS_FLVL ( report, lenr, iflflg, iflvl, iptr, iret )
C************************************************************************
C* IS_FLVL 								*
C*									*
C* This subroutine decodes a flight level from an international sigmet  *
C* report.                                                              *
C*                                                                      *
C* IS_FLVL ( REPORT, LENR, IFLFLG, IFLVL, IPTR, IRET )                  *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
C*									*
C* Output parameters:							*
C*	IFLFLG 		INTEGER 	Sigmet flight level flag        *
C*	IFLVL(*) 	INTEGER 	Sigmet flight level(s)          *
C*	IPTR		INTEGER		Pointer to location after level *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = flight level not found    *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	11/99	Allowed for some format variations      *
C* F. J. Yen/NCEP	 1/00	Increase length min of 80		*
C* F. J. Yen/NCEP	 3/00	Increase len to string length for RJAA	*
C* F. J. Yen/NCEP 	 7/00	Allowed for product generation variation*
C* F. J. Yen/NCEP 	10/01	Added keyword 'TOP' when no 'FL'	*
C* F. J. Yen/NCEP 	 9/03	Added keyword 'BLW' when no 'FL' and	*
C*				got rid of false level following 'FCST'	*
C* F. J. Yen/NCEP	11/03	Added keyword 'BETWEEN','BLO',&'TOPPED'.*
C*				Allowed for 'SFC' & FZLVL. Fixed when no*
C*				FL prefix for 'BTN'. Fixed size of flag.*
C* F. J. Yen/NCEP	12/03	Checked for false FL keywords 'SFC' and	*
C*				'FLO'.  Added keyword 'FLIGHT LEVEL'.	*
C* F. J. Yen/NCEP	 2/04	Allowed for plural 'FLIGHT LEVELS' even *
C*				when no 'BTN' type keyword.  Account for*
C*				more than 1 FLxxx-xxx giving false FL	*
C*			 	prefix for second FL.			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
	INTEGER		iflvl (*)
C*
	CHARACTER	flag*33
	LOGICAL 	done, more, numfld, flpref, abbrv
C------------------------------------------------------------------------
	iret        = 0
	iflflg      = IMISSD
	iflvl ( 1 ) = IMISSD
	iflvl ( 2 ) = IMISSD
	iptr        = 0
C
C*	Look for the key field FL.
C
	len = lenr
	iloc = INDEX ( report ( :len ), ' FLIGHT LEVEL')
	IF ( iloc .eq. 0 ) THEN
	    abbrv = .true.
	    iloc = INDEX ( report ( :len ), ' FL' )
	    IF ( iloc .ne. 0 ) THEN
		iflo = INDEX ( report (iloc:iloc+3), ' FLO' )
		IF ( iflo .ne. 0 ) THEN
		    iloc = INDEX ( report (iloc+iflo+3:len), ' FL' )
		    IF ( iloc .ne. 0 ) THEN
			iflo2 = INDEX ( report (iloc:iloc+3), ' FLO' )
			IF ( iflo2 .ne. 0 ) THEN
			    iloc = 0
			  ELSE
			    iloc = iloc + iflo + 2
			END IF
		    END IF
		END IF
	    END IF
	  ELSE
	    abbrv = .false.
C
C*	    If flight level is a plural, set iflflg to 2 in case
C*	    a 'BETWEEN' type keyword is missing.
C
	    IF ( report ( iloc + 13: iloc + 13 ) .eq. 'S' ) iflflg = 2
	END IF
	ilocsf = INDEX (  report ( :len ), ' SFC' )
	IF ( ilocsf .ne. 0 ) THEN
	    lnth = MIN ( ilocsf+9, len )
C
C*	    Handle "SFC/WNDS", "SFC WNDS", etc.
C
	    ilocwn = INDEX (  report (ilocsf+5:lnth), 'WNDS' )
	    IF ( ilocwn .ne. 0 ) THEN
		ilocsf = 0
	      ELSE
	        lnth = MIN ( ilocsf+10, len )
		ilocwn = INDEX (  report (ilocsf+4:lnth), ' GUSTS' )
		IF ( ilocwn .ne. 0 ) ilocsf = 0
	    END IF 
	END IF
	ilocfz = INDEX (  report ( :len ), ' FZLVL' ) 
	ilocfs = MAX ( ilocsf, ilocfz )
	IF ( iloc .gt. 0 .and. ilocfs .gt. 0 ) THEN
	    IF ( ilocfs .lt. iloc ) THEN
		iloc = ilocfs
	    END IF
	END IF
	IF ( iloc .le. 0 ) THEN
C
C*	    Look for key fields TOPS, TOP, BLW, BLO, or FCST.
C
	    flpref = .false.
	    iloc = INDEX ( report ( :len), ' TOPS ' )
	    IF ( iloc .le. 0 ) THEN
		iloc = INDEX ( report ( :len), ' TOP ' )
     		IF ( iloc .le. 0 ) THEN
		    iloc = INDEX ( report ( :len), ' BLW ' )
	    	    IF ( iloc .le. 0 ) THEN
		        iloc = INDEX ( report ( :len), ' BLO ' )
	    	        IF ( iloc .le. 0 ) THEN
		    	  iloc = INDEX ( report( :len), ' TOPPED ' )
			  IF ( iloc .le. 0 ) THEN
			    iloc = INDEX ( report( :len), ' BETWEEN ' )
			    IF ( iloc .le. 0 ) THEN
			      iloc = INDEX ( report( :len), ' BTN ' )  
			      IF ( iloc .le. 0 ) THEN
				iloc = INDEX ( report ( :len), ' FZLVL' )
				iloc1 = INDEX ( report ( :len), ' SFC' )
				iloc = iloc + iloc1
				IF ( iloc .le. 0 ) THEN
		          	  iloc = INDEX ( report ( :len), ' FCST ' )
		                  IF ( iloc .le. 0 ) THEN
		                    iret = -6
			           ELSE
C
C*			           Shorten search for numeric following
C*			           'FCST' due to false flight level.
C
			           iloc = iloc - 4
			          END IF
				 ELSE
				  iloc = iloc - 5
				END IF
			       ELSE
			        iloc = iloc - 1
			      END IF
			     ELSE
		              iloc = iloc + 3
			    END IF
                           ELSE
                            iloc = iloc + 2
			  END IF
		         ELSE
		          iloc = iloc - 1
		        END IF
		      ELSE
		        iloc = iloc - 1
		    END IF
		  ELSE
		    iloc = iloc - 1
	        END IF
	    END IF
C
C*	    Look for numeric field following TOPS, TOP, BLW, BLO,
C*	    or FCST.
C
	    iloc = iloc + 5
	    loce = MIN (len, iloc + 7)
	    numfld = .false.
	    DO WHILE ( (iloc .lt. loce) .and. (.not. numfld) )
	        CALL ST_ALNM ( report ( iloc:iloc ), ityp, ier )
		  IF ( ityp .eq. 1 ) THEN
                    numfld = .true.
		    iloc = iloc - 3
    		   ELSE IF ( INDEX (report (iloc:iloc+3),' SFC')
     +                  .ne. 0 .and. INDEX (report (iloc+5:iloc+7),
     +			'WNDS') .ne . 0 .and. 
     +			INDEX (report (iloc+4:iloc+8), ' GUSTS' ) .ne.
     +			0 ) THEN
C
C*                  Allow for ' SFC' as a numfld
C
                    numfld = .true.
                    iloc = iloc - 3
    		   ELSE IF ( INDEX (report (iloc:iloc+5),' FZLVL')
     +                   .ne. 0 ) THEN
C
C*                  Allow for ' FZLVL' as a numfld
C
                    numfld = .true.
                    iloc = iloc - 3
		   ELSE
	            iloc = iloc + 1
		 END IF
	    END DO
	  ELSE
	    flpref = .true.
	    numfld = .true.
	    IF ( .not. abbrv ) iloc = iloc + 11
	END IF
	IF ( iret .eq. 0 .and. numfld ) THEN
C
C*	    Check the preceding word for a flag indicator.
C
	    IF ( iloc .ge. 2 ) THEN
		IF ( iloc .le. 31 ) THEN
		    ibf = 1
		  ELSE
		    ibf = iloc - 30
		END IF
		IF ( .not. flpref ) THEN
		    ilocf = iloc + 2
		  ELSE
		    ilocf = iloc - 1
		END IF
		flag = report ( ibf:ilocf)
		IF ( INDEX ( flag, 'BTN' ) .ne. 0 ) THEN
		    iflflg = 2
		  ELSE IF ( INDEX ( flag, 'BETWEEN' ) .ne. 0 ) THEN
		    iflflg = 2
		  ELSE IF ( INDEX ( flag, 'ABV' ) .ne. 0 ) THEN
		    iflflg = 1
		  ELSE IF ( INDEX ( flag, 'BLW' ) .ne. 0 ) THEN
		    iflflg = -1
		  ELSE IF ( INDEX ( flag, 'BLO' ) .ne. 0 ) THEN
		    iflflg = -1
		  ELSE IF ( iflflg .ne. IMISSD) THEN
		    CONTINUE
		  ELSE
		    iflflg = 0
		END IF
	    END IF
C
C*	    Decode the flight level(s).
C
	    more = .true.
	    ii   = 1
	    DO WHILE ( more )
	        iloc = iloc + 3
		IF ( report ( iloc:iloc ) .eq. ' ' ) iloc = iloc + 1
	        ibeg = iloc
	        done = .false.
	        DO WHILE ( .not. done )
C
C*	            Get the numeric field immediately following FL.
C
		    CALL ST_ALNM ( report ( iloc:iloc ), ityp, ier )
		    IF ( ityp .eq. 1 ) THEN
		        iloc = iloc + 1
		        IF ( iloc .gt. len ) THEN
			    done = .true.
		        END IF
		      ELSE IF ( ( report(iloc-2:iloc) .eq. 'SFC' .and.
     +			      report (iloc+3:iloc+5) .ne. 'WNDS' .and.
     +			      report (iloc+2:iloc+6) .ne. ' GUSTS') .or.
     +			      report(iloc-2:iloc+2) .eq. 'FZLVL' ) THEN
C
C*			       Found SFC or FZLVL
C
			IF (flpref) THEN
			    ibeg = iloc - 2
			    iloc = iloc + 1
			  ELSE
			    ibeg = iloc
			    IF ( report(iloc-2:iloc) .eq. 'SFC' ) THEN
                                iloc = iloc + 3
			      ELSE
                                iloc = iloc + 5
			    END IF
			 END IF
			 IF (iloc .gt. len) THEN
			     done = .true.
			END IF 
		      ELSE IF ( ( report(iloc:iloc+2) .eq. 'SFC' .and.
     +			      report (iloc+5:iloc+7) .ne. 'WNDS' .and.
     +			      report (iloc+4:iloc+8) .ne. ' GUSTS') .or.
     +			      report (iloc:iloc+4) .eq. 'FZLVL' ) THEN
			IF (flpref) THEN
			    ibeg = iloc - 2
			    iloc = iloc + 1
			  ELSE
			    ibeg = iloc
			    IF ( report(iloc:iloc+2) .eq. 'SFC' ) THEN
                                iloc = iloc + 3
			      ELSE
                                iloc = iloc + 5
			    END IF
			END IF
			IF (iloc .gt. len) THEN
			    done = .true.
			END IF 
		      ELSE
		        done = .true.
C
C*			Look for a second field separated from the first
C*			by '/' or '-'.
C
		        IF ( ityp .eq. 0 ) THEN
			    IF ( ( report ( iloc:iloc ) .eq. '/' ) .or.
     +			         ( report ( iloc:iloc ) .eq. '-' ) )THEN
			        iflflg = 3
			    END IF
			END IF
		    END IF
	        END DO
C
C*		Get the flight level.
C
		iend = iloc - 1
		IF ( iend .ge. ibeg ) THEN
		    CALL ST_INTG ( report (ibeg:iend), iflvl (ii), ier )
		    IF (ier .ne. 0 ) THEN
                        IF ( INDEX (report (ibeg:iend),'SFC')
     +                           .ne. 0 )  THEN
                            iflvl(ii) = -5000
			  ELSE IF ( INDEX (report (ibeg:iend),'FZLVL')
     +                           .ne. 0 )  THEN
                            iflvl(ii) = -6000
                        END IF
                    END IF
		  ELSE
		    iret = -6
		END IF
C
C*		Check to see if there is a second flight level.
C
		IF ( ( iflflg .lt. 2 ) .or. ( ii .eq. 2 ) ) THEN
		    more = .false.
		  ELSE
		    iend2 = MIN ( iend+16, len )
		    ifl = INDEX ( report ( iend:iend2 ), 'FL' )
		    IF ( ifl .ne. 0 ) THEN
			iloc = ifl + iend - 2
		      ELSE
			IF ( iflflg .eq. 3 ) THEN
			    iloc   = iloc - 2
			  ELSE
			    ifl = INDEX ( report ( iend:iend2 ), 'AND' )

			    IF ( ifl .ne. 0 ) THEN
				iloc = ifl + iend - 1
			      ELSE
				ifl = INDEX ( report (iend:iend2), 'TO' )
				IF ( ifl .ne. 0 ) THEN
				    iloc = ifl + iend - 1
				  ELSE
				    more = .false.
				END IF
			    END IF
			END IF
		    END IF
		    iflflg = 2
		    ii     = ii + 1
		END IF
	    END DO
	  ELSE
	    iret = -6
	END IF
C
C*	Set the flight level flag if it has not already been set.
C
	IF ( ( iflvl ( 1 ) .ne. IMISSD ) .and. ( iflflg .eq. IMISSD ) )
     +	       iflflg = 0
	IF ( ( iflvl (1) .ne. IMISSD ) .or. ( iflvl (2) .ne. IMISSD ) )
     +	       THEN
	    IF ( iflvl ( 1 ) .eq. iflvl ( 2 ) ) THEN
		IF ( iflflg .eq. 2 ) iflflg = 0  
	    END IF
	    iptr = iend + 1
	END IF
C*
	RETURN
	END
