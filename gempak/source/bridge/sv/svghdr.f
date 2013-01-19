	SUBROUTINE SV_GHDR  ( bultin, lenbul, icor, wtype,
     +			      iwtchn, state, iret )
C************************************************************************
C* SV_GHDR								*
C*									*
C* This subroutine gets the header information from a WMO bulletin.	*
C*									*
C* The SLS bulletin header information consists of several lines	*
C* following the first three lines of the general bulletin header.      *
C* The bulletin	should still have all control characters present for	*
C* this routine	to function properly.					*
C*									*
C* SV_GHDR  ( BULTIN, LENBUL, ICOR, WTYPE, IWTCHN, STATE, IRET )	*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		Bulletin after the third line	*
C*	LENBUL		INTEGER		Length of bulletin		*
C*									*
C* Output parameters:							*
C*	ICOR		INTEGER		Correction indicator		*
C*	WTYPE		CHAR*		Watch type			*
C*	IWTCHN		INTEGER		Watch number			*
C*	STATE		CHAR*		Watch state abbreviation	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = No watch number found	*
C*					 -3 = No type			*
C*					 -4 = 'SLS' not found		*
C*									*	
C**									*
C* Log:									*
C* F. J. Yen/NCEP	12/00						*
C* F. J. Yen/NCEP	 1/01	Removed county string processing.	*	
C* F. J. Yen/NCEP	 4/01	Obtained watch state abbreviation.	*
C* F. J. Yen/NCEP	 3/02	Changed error numbers.			*
C* m.gamazaychikov/SAIC	03/04	Improved handling of "OUTLINE FOR" line	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	bultin, wtype, state
C*
	CHARACTER	btin*(DCMXBF)
	LOGICAL		numfld 
C------------------------------------------------------------------------
	iret = 0
        icor = 0
        ibuldx = 0
        ireqstd = 0
        ifor    = 0
        iaof    = 0
	wtype = ' '
	iwtchn = IMISSD
C
	islsdx = INDEX ( bultin(:lenbul), 'SLS' )
	IF ( islsdx .gt. 0 ) THEN  
C
C*	    Get the state abbreviation following 'SLS'
C
	    state = bultin(islsdx+3:islsdx+5)
C
C*	    Get watch type
C
            ireqstd = INDEX ( bultin(:lenbul), 'BROADCAST REQUESTED')
            ifor    = INDEX ( bultin(:lenbul), 'OUTLINE FOR')
            ireqstd = ireqstd + 22
            ifor    = ifor    + 11
            call ST_LSTR(bultin(ireqstd:ifor), iaof, ier2)
	    IF ( iaof .gt. 0 ) THEN
                ibuldx = ireqstd+iaof
		IF ( ibuldx .lt. lenbul ) THEN
		    iend = MIN ( ibuldx + 80, lenbul )
		    itypdx = INDEX ( bultin(ibuldx:iend), 'TORNADO' )
		    IF ( itypdx .gt. 0 ) THEN
			wtype = 'TN'
			itypdx = itypdx + 7
		      ELSE
			itypdx = INDEX ( bultin(ibuldx:iend),
     +				    'SEVERE THUNDERSTORM' )
			IF ( itypdx .gt. 0 ) THEN 
			    wtype = 'TS'
			    itypdx = itypdx + 19
			  ELSE
			    iret = -3
			    RETURN
			END IF
		    END IF
		END IF
C
C*		Get watch number
C
		iloc = ibuldx + itypdx
		IF ( iloc .lt. lenbul ) THEN
		    iwndx = INDEX ( bultin(iloc:lenbul), 'WATCH NUMBER' )
		    IF ( iwndx .le. 0 ) THEN
		        iwndx = INDEX ( bultin(iloc:lenbul), 'WATCH #' )
		        IF ( iwndx .eq. 0 ) THEN
		            iret = -2
			    iwndx = 1
			  ELSE
		            iwndx = iwndx + 7
		        END IF
		      ELSE
		        iwndx = iwndx + 12
		    END IF
		END IF
C
C*		Look for numeric field.
C
		iloc = iloc + iwndx - 1
		ilocsv = iloc
		loce = MIN ( lenbul, iloc + 7 )
		numfld = .false.
		ityp = 0
		DO WHILE ( (iloc .lt. loce) .and. (.not. numfld) )
                    CALL ST_ALNM ( bultin ( iloc:iloc ), ityp, ier )
                    IF ( ityp .eq. 1 ) THEN
			ilocb = iloc
                        numfld = .true.
                    END IF
                    iloc = iloc + 1
                END DO
		IF ( .not. numfld ) THEN
		    iret = -2
		    iloc = ilocsv 
		END IF
		iloce = iloc
		ilocsv = iloc
		DO WHILE ( ( iloc .lt. loce ) .and. ( ityp .eq. 1 ) )
                    CALL ST_ALNM ( bultin ( iloc:iloc ), ityp, ier )

		    iloce = iloc
		    iloc = iloc + 1
		END DO
		iloce = iloce - 1
		IF ( numfld ) THEN
		    CALL ST_NUMB ( bultin(ilocb:iloce),iwtchn, ier )
		    IF ( ier .ne. 0 ) THEN
		        iret = -2
		        iloce = ilocsv
		    END IF
		END IF
C
C*		Get length of bultin and get rid of unprintable characters
C
		CALL ST_UNPR ( bultin, lenbul, btin, lenb, ier )
C
C*              Check for a correction indicator
C
 	        indx = INDEX ( btin(:lenbul), 'CORRECTION')
  	        IF (  indx .gt. 0  ) THEN
 		    icor = 1
 		  ELSE
 		    indx = INDEX ( btin(:lenbul), 'CORRECTED')
 		    IF (  indx .gt. 0  ) icor = 1
 		END IF
	    END IF
          ELSE
            iret = -4  
        END IF
C*
	RETURN
	END
