	SUBROUTINE WC_GHDR  ( bultin, lenbul, vtec, nvt, cnties, nchar, 
     +			      iret )
C************************************************************************
C* WC_GHDR								*
C*									*
C* This subroutine gets the VTEC section and county line string from a  *
C* watch county notification bulletin. 					*
C*									*
C* WC_GHDR  ( BULTIN, LENBUL, VTEC, NVT, CNTIES, NCHAR, IRET )		*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		WMO bulletin w/ control chars	*
C*	LENBUL		INTEGER		Length of bulletin		*
C*									*
C* Output parameters:							*
C*	VTEC		CHAR*		VTEC string			*
C*	NVT		INTEGER		Number of VTEC lines		*
C*	CNTIES		CHAR*		Counties in the warning		*
C*	NCHAR		INTEGER		Number of chars in the header	*
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal return		*
C*					 -1 = Error finding VTEC string *
C*					 -3 = Error with county string	*
C**									*
C* Log:									*
C* A. Hardy/NCEP	10/02						*
C* A. Hardy/NCEP         1/03 		Check location of '-'           *
C* A. Hardy/NCEP         2/03 		Made VTEC variables arrays	*
C* A. Hardy/NCEP         2/03 		Added chk for '/' in headerline;*
C*				        Changed error log level 2 -> 0  *
C* A. Hardy/NCEP         5/04 		Add chk new/old type VTEC lines *
C* A. Hardy/NCEP         5/04 		Add chk for county string 	*
C* A. Hardy/NCEP	 9/04           Fixed counter for index nslsh	*
C* A. Hardy/NCEP	10/04           Fixed error for cnty string     *
C* A. Hardy/NCEP	 3/05           Reworked finding VTEC lines	*
C* S. Chiswell/Unidata	 7/05		Added check for '>' range	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	bultin, vtec(*), cnties
        LOGICAL         done, finish
C------------------------------------------------------------------------
	iret = 0
        islash = 0
        cnties = ' '
        done   = .false.
        finish = .true.
C
C*	Find the VTEC line beginning and ending with '/'.
C
	islash  = INDEX ( bultin ( :lenbul), '/O.' )
        IF ( islash .eq. 0 ) THEN
	    islash  = INDEX ( bultin ( :lenbul), '/E.' )
            IF ( islash .eq. 0 ) THEN
	        islash  = INDEX ( bultin ( :lenbul), '/T.' )
                IF ( islash .eq. 0 ) THEN
	            islash  = INDEX ( bultin ( :lenbul), '/X.' )
                END IF
            END IF
        END IF
C
C*	Return if cant find VTEC section.
C
        nvt = 1
	IF ( islash .eq. 0 ) THEN
            iret = -1
            CALL DC_WLOG ( 0, 'DCWCN', iret, ' ', ier )
            RETURN
          ELSE
            vtec (nvt) = ' '
            IF (bultin(islash+46:islash+47) .eq. 'Z/' ) THEN
	        islend  = INDEX ( bultin (islash+1 :lenbul), 'Z/' )
                IF ( islend .gt. 0 ) THEN
                    vtec (nvt) = bultin (islash+1:islash+islend)
C
C*                  Check for multiple VTEC lines.
C
                   
                    inwbg = islash+islend
                    finish = .false.
                    DO WHILE ( .not. finish )
	              nslsh = INDEX ( bultin (inwbg:inwbg+6), '/O.')
                      IF ( nslsh .eq. 0 ) THEN
	                nslsh = INDEX ( bultin (inwbg:inwbg+6), '/E.')
                        IF ( nslsh .eq. 0 ) THEN
	                  nslsh = INDEX ( bultin (inwbg:inwbg+6), '/T.')
                          IF ( nslsh .eq. 0 ) THEN
	                    nslsh = INDEX(bultin(inwbg:inwbg+6), '/X.')
                          END IF
                        END IF
                      END IF
                      IF ( nslsh .gt. 0 ) THEN
                          nvt = nvt + 1
                          vtec (nvt) =  
     +                       bultin(inwbg+nslsh:inwbg+nslsh+islend-1)
                             inwbg = inwbg+nslsh+islend
                        ELSE
                          finish = .true.
                      END IF
                    END DO
                END IF
            END IF
        END IF
C
C*      Find the counties and purge time.
C
        CALL ST_NOCC (bultin(:islash), '-', 1, ictybeg, ier )
C
C*	Check for a range '>' which may preecede the first '-' character
        CALL ST_NOCC (bultin(:islash), '>', 1, inext, ier1 )
	IF ( ( ier1 .eq. 0 ) .and. ( inext .lt. ictybeg ) ) THEN
	    ictybeg = inext
	    ier = 0
	END IF
C
C*      If have county string, check for the '-' if it is found 
C*	within the county line.
C
        IF ( ier .eq. 0 ) THEN
            DO WHILE ( .not. done )
                CALL ST_ALNM (bultin ( ictybeg-6:ictybeg-6), ityp6, ier)
                CALL ST_ALNM (bultin ( ictybeg-5:ictybeg-5), ityp5, ier)
                CALL ST_ALNM (bultin ( ictybeg-4:ictybeg-4), ityp4, ier)
                CALL ST_ALNM (bultin ( ictybeg-3:ictybeg-3), ityp3, ier)
                CALL ST_ALNM (bultin ( ictybeg-2:ictybeg-2), ityp2, ier)
                CALL ST_ALNM (bultin ( ictybeg-1:ictybeg-1), ityp1, ier)
C
                IF ( ( ityp6 .eq. 2 ) .and. ( ityp5 .eq. 2 ) .and.
     +               ( ityp4 .eq. 2 ) .and. ( ityp3 .eq. 1 ) .and.
     +               ( ityp2 .eq. 1 ) .and. ( ityp1 .eq. 1 ) ) THEN
                    done = .true.
                  ELSE
C
C*		    Check for '-' or '>' within line 
C
		    inext = ictybeg+1
		    IF ( inext .lt. islash ) THEN
		       ictybeg = index ( bultin(inext:islash), '-' ) 
		       i2 = index ( bultin(inext:islash), '>' ) 
		       IF ( ( i2 .gt. 0 ) .and. 
     +					( i2 .lt. ictybeg ) ) THEN
			 ictybeg = i2
		       END IF
		    ELSE
		       ictybeg = 0
        	    END IF
C
                    IF ( ictybeg .eq. 0 ) THEN
                        done = .true.
		    ELSE
		        ictybeg = (inext - 1) + ictybeg 
                    END IF
                END IF
            END DO
C
            IF ( ictybeg .ne. 0 ) THEN
                cnties = bultin ( ictybeg-6:islash-1)
              ELSE
                iret = -3
                CALL DC_WLOG ( 0, 'DCWCN', iret, ' ', ier )
            END IF 
          ELSE
            iret = ier
            CALL DC_WLOG ( 0, 'ST', iret, ' ', ier )
        END IF
C*
	RETURN
	END

