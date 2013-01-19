	SUBROUTINE FA_VTCN  ( bultin, lenbul, pvtec, hvtec, nvt, cnties, 
     +			      iret )
C************************************************************************
C* FA_VTCN								*
C*									*
C* This subroutine gets the VTEC section and county line string from a  *
C* flash flood watch bulletin.						*
C*									*
C* FA_VTCN  ( BULTIN, LENBUL, PVTEC, HVTEC, NVT, CNTIES, IRET )		*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		FFA bulletin w/ control chars	*
C*	LENBUL		INTEGER		Length of bulletin		*
C*									*
C* Output parameters:							*
C*	PVTEC		CHAR*		PVTEC string			*
C*	HVTEC		CHAR*		corresponding HVTEC string	*
C*	NVT		INTEGER		Number of VTEC lines		*
C*	CNTIES		CHAR*		Counties in the warning		*
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal return		*
C*					 -1 = Error finding VTEC string *
C*					 -3 = Error with county string	*
C**									*
C* Log:									*
C* H. Zeng/SAIC		07/05		initial coding			*
C* H. Zeng/SAIC		08/05		Added check for '>' range	*
C* F. J. Yen/NCEP	04/06		Fix inf loop when '/' is found  *
C*					but no VTEC; fix error handling	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	bultin, pvtec(*), hvtec(*), cnties
        CHARACTER	vseg1*10, vseg2*10, vseg3*10
        LOGICAL         done, found, finish
C------------------------------------------------------------------------
	iret = 0
        islash = 0
        cnties = ' '
        done   = .false.
        found  = .false.
        finish = .false.
C
C*	Find the first PVTEC line beginning and ending with '/'.
C
	istart = 1
	nvt = 0
        DO WHILE ( .not. found .and. istart .lt. lenbul - 46)
	   islash  = INDEX ( bultin ( istart:lenbul ), '/' )
C
	   IF ( islash .eq. 0 ) THEN
C
C*	       No slash found, therefore no VTEC.  Calling
C*	       routine will write enhanced message to log.
C
               iret = -1
               RETURN
           END IF
C
	   vseg1 = bultin( istart + islash - 1 : istart + islash + 1 )
	   IF ( (vseg1 .eq. '/O.' .or. vseg1 .eq. '/E.' .or. 
     +	         vseg1 .eq. '/T.' .or. vseg1 .eq. '/X.') .and. 
     +	         bultin(istart+islash+45:istart+islash+46) .eq.
     +		 'Z/' ) THEN
C
	      nvt = 1
	      pvtec (nvt) = bultin (istart+islash:istart+islash+45)
	      istart = istart + islash + 47
	      found = .true.
	   ELSE
	      istart = istart + islash
	   END IF
        END DO
C
C*	Find the following PVTEC/HVTEC lines beginning and 
C*	ending with '/'.
C
        DO WHILE ( found .and. .not. finish .and. istart .lt. lenbul )
	   nslash  = INDEX ( bultin ( istart:istart+6 ), '/' )
C
	   IF ( nslash .eq. 0 ) THEN
	      finish = .true.
C
	   ELSE
C
	      vseg1 = bultin( istart+nslash-1 : istart+nslash+1)
	      vseg2 = bultin( istart+nslash+45: istart+nslash+46)
	      vseg3 = bultin( istart+nslash+49: istart+nslash+52)
C
	   IF ( (vseg1 .eq. '/O.' .or. vseg1 .eq. '/E.' .or. 
     +	         vseg1 .eq. '/T.' .or. vseg1 .eq. '/X.') .and. 
     +	        vseg2 .eq. 'Z/') THEN
C
	      nvt = nvt + 1
	      pvtec (nvt) = bultin (istart+nslash: istart+nslash+45)
	      istart = istart + nslash + 47
	   ELSE IF ( vseg1 .eq. '/00' .and. 
     +	          ( vseg3 .eq. '.NO/' .or. vseg3 .eq. '.NR/' .or.
     +              vseg3 .eq. '.UU/' .or. vseg3 .eq. '.OO/') ) THEN
C
	      hvtec (nvt) = bultin (istart+nslash: istart+nslash+51)
	      istart = istart + nslash + 53
	   ELSE
C
	      finish = .true.
	   END IF
C
	   END IF
        END DO
        IF ( nvt .eq. 0 ) THEN
C
C*	    Found a slash, but not VTEC.  Calling routine will
C*	    write enhanced message to log.
C
            iret = -1
	    RETURN
	END IF
C
C
C*      Find the counties and purge time.
C
        CALL ST_NOCC (bultin(:islash), '-', 1, ictybeg, ier )
C
C*	Check for a range '>' which may proceed the first '-'.
C
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
C*		  Check for '-' or '>' within line.
C
		  inext = ictybeg + 1
		  IF ( inext .lt. islash ) THEN
		    ictybeg = index ( bultin(inext:islash), '-')
		    i2 = index ( bultin(inext:islash), '>' )
		    IF ( ( i2 .gt. 0 ) .and. ( i2 .lt. ictybeg ) )
     +		      ictybeg = i2
C		  
		  ELSE
		    ictybeg = 0
	          END IF
C
		  IF ( ictybeg .eq. 0 ) THEN
		    done = .true.
	          ELSE
		    ictybeg = (inext - 1) + ictybeg
		  END IF
C
                END IF
            END DO
C
            IF ( ictybeg .ne. 0 ) THEN
                cnties = bultin ( ictybeg-6:islash-1)
            ELSE
                iret = -3
                CALL DC_WLOG ( 0, 'DCFFA', iret, ' ', ier )
            END IF 
          ELSE
            iret = ier
            CALL DC_WLOG ( 0, 'ST', iret, ' ', ier )
        END IF
C*
	RETURN
	END
