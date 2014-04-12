	SUBROUTINE TF_SHDR  ( rpt, lenr, iptrh, irday, irhr, irmin,
     +			      iret )
C************************************************************************
C* TF_SHDR							        *
C*								        *
C* This subroutine gets the report time from the beginning of a TAF     *
C* report.                                          		        *
C*								        *
C* TF_SHDR  ( RPT, LENR, IPTRH, IRDAY, IRHR, IRMIN, IRET )              *
C*								        *
C* Input parameters:						        *
C*	RPT		CHAR*		Report    		        *
C*	LENR		INTEGER		Report length                   *
C*								        *
C* Output parameters:						        *
C*	IPTRH		INTEGER		Pointer in report to end of hdr *
C*	IRDAY		INTEGER		Report day		        *
C*	IRHR		INTEGER		Report hour		        *
C*	IRMIN		INTEGER 	Report minute		        *
C*	IRET		INTEGER		Return code		        *
C*					  1 = no rpt tm; valid tm found *
C*					  0 = normal return	        *
C*					 -1 = bad report time           *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP       9/02                                           *
C* L. Lin/NCEP           4/08 Fixed bugs - IPTRH may out of range in rpt*
C* L. Hinson/AWC         12/13  Fix decode issue with Milatary TAFS     *
C************************************************************************
	CHARACTER*(*)	rpt
C*
	CHARACTER	strdat*11, carr (5)*20
	LOGICAL 	numer6 (2)
        LOGICAL         dttmchk
C------------------------------------------------------------------------
        iret  = -1 
	iptrh = 1
C
C*      Get the lens of rpt and make sure locidx is not
C*      out of range from rpt
C
        CALL ST_LSTR ( rpt , lens1, ier1 )
        IF ( lens1 .gt. 8 ) THEN
             locidx = 8
        ELSE
             locidx = lens1 - 1
        END IF
C
C*	Skip over station id:  first string in report.
C
	iblk = INDEX ( rpt ( iptrh:iptrh + locidx ), ' ' )
	iptrh = iptrh + iblk 
C
C*      Get the lens of report time and make sure locidx is not
C*      out of range from rpt
C
        CALL ST_LSTR ( rpt ( iptrh: ), lens2, ier2 )
        IF ( lens2 .gt. 9 ) THEN
             locidx = 9
        ELSE
             locidx = lens2 - 1
        END IF
C
C*	Get date/time from report.  It should end in 'Z'.
C
	locz = INDEX ( rpt ( iptrh:iptrh + locidx ), 'Z ')
	IF ( locz .ge. 7 ) THEN
	    strdat = rpt (iptrh:iptrh + locz )
	    CALL ST_RMBL ( strdat, strdat, lendat, ier )
	    IF ( lendat .eq. 7 ) THEN
		CALL BR_DTTM ( strdat, lendat, irday, irhr, irmin, ier )
		IF ( ier .eq. 0 ) THEN
		    iptrh = iptrh + locz + 1
		    iret  = 0
		END IF
	    END IF
	  ELSE IF ( locz .eq. 0 ) THEN
C
C*	    Check for a date/time string not ending in 'Z'.
C
	    lenchk = MIN0 ( lenr, iptrh + 20 )
	    CALL ST_CLST ( rpt ( iptrh:lenchk ), ' ', ' ', 5, carr, num,
     +			   ier )
	    IF ( num .lt. 2 ) RETURN
	    DO ii = 1, 2
                numer6 ( ii ) = .true.
                CALL ST_LSTR ( carr ( ii ), lens, ier )
                IF ( lens .eq. 6 ) THEN
                    DO jj = 1, 6
                        CALL ST_ALNM ( carr (ii) (jj:jj), ityp, ier )
                        IF ( ityp .ne. 1 ) numer6 ( ii ) = .false.
                    END DO
		  ELSE
		    numer6 ( ii ) = .false.
                END IF
            END DO
C
	    IF ( numer6 ( 1 ) .and. numer6 ( 2 ) ) THEN
C
C*	        Append a Z to the first group to get the report time.
C
		strdat = carr ( 1 ) // 'Z'
		CALL BR_DTTM ( strdat, 7, irday, irhr, irmin, ier )
		IF ( ier .eq. 0 ) THEN
		    iptrh = INDEX ( rpt ( :lenr ), carr ( 2 ) ( :6 ) )
		    iret  = 0
		END IF
	      ELSE IF ( numer6 ( 1 ) ) THEN
C
C*		No report time is present, but a valid forecast time
C*		group was apparently found.
C
		iptrh = INDEX ( rpt ( :lenr ), carr ( 1 ) ( :6 ) )
		iret  = 1
	    END IF
            IF ( iret .eq. -1 ) THEN
C*            Check if Military TAF format, where we have starting forecast
C*            time group in DDHH/DDHH format.
              CALL ST_LSTR ( carr ( 1 ), lens, ier )
              dttmchk = .true.
              IF ( lens .eq. 9 ) THEN
                 DO jj = 1, 4
                    CALL ST_ALNM (carr (1) (jj:jj), ityp, ier )
                    IF (ityp .ne. 1) dttmchk = .false.
                 ENDDO
                 DO jj = 6, 9        
                   CALL ST_ALNM (carr(1) (jj:jj), ityp, ier )
                   IF (ityp .ne. 1) dttmchk = .false.
                 END DO
                 IF (carr(1) (5:5) .ne. '/') dttmchk = .false.
              ELSE
                dttmchk = .false.
              END IF
              IF (dttmchk) THEN
                 iptrh = INDEX ( rpt ( :lenr ), carr ( 1 ) ( :8 ) )
                 iret = 1
              END IF
            END IF              
	END IF
C*
	RETURN
	END
