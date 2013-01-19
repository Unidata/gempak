	SUBROUTINE CS_CONV ( lunf, conar, lenc, lenr, iotarr, iptr, 
     +                       astart, icor, inon, reg, iret )
C************************************************************************
C* CS_CONV 								*
C*									*
C* This subroutine processes a convective sigmet section from a report. *
C*                                                                      *
C* CS_CONV ( LUNF, CONAR, LENC, LENR, IOTARR, IPTR, ASTART, ICOR, 	*
C*           INON, REG, IRET )						*
C*									*
C* Input parameters:							*
C*      LUNF		INTEGER         File unit number		*
C*	CONAR		CHAR*		Convective sigmet bulletin      *
C*	LENC		INTEGER		Length of convective section	*
C*	LENR		INTEGER		Length of bulletin              *
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*	IPTR		INTEGER		Position in bulletin		*
C*	ASTART		CHAR*		Start time string		*
C*	ICOR 		INTEGER		Correction flag                 *
C*	INON 		INTEGER		Convection flag			*
C*                                         = 0 - convective areas	*
C*                                         > 0 - no convective areas	*
C*      REG		CHAR*		Valid Region			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	 8/02	Created					*
C* A. Hardy/NCEP	 1/04   Modified to decode nil type reports	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	conar, astart, reg
	INTEGER		iotarr (*)
C*
	PARAMETER	( MAXPTS = 40 )
        CHARACTER	type*2, strtim*20, stptim*20, strbuf*250, 
     +			flvl (2)*4, seq*3, dir*3, spd*2, dist*3, 
     +			day*2, astop*20, intsy*7
	REAL		rlat (MAXPTS), rlon (MAXPTS)
	INTEGER		jarr(5)
C*
	LOGICAL		done, more
C------------------------------------------------------------------------
	iret   = 0
        ierr   = 0 
        iend   = 0
        ifrom  = 0
	dir     = ' '
        spd     = ' '
        dist    = ' '
        intsy   = ' '
        flvl(1) = ' '
        flvl(2) = ' '
	done   = .false.
	more   = .false.
	strbuf = conar ( :160 )
C
C*      Break apart the Convective Sigmet section.
C
 	DO WHILE ( ( ierr .eq. 0 ) .and. ( .not. done ) ) 
C
C*          Have convective outlook area(s).
C
            IF ( inon .eq. 0 ) THEN
C
C*              Get convective ID name.
C
                seq = conar ( iptr+18: iptr+21 )
                iptr = iptr + 22
C
C*              Get stop time.
C
                ival = INDEX ( conar ( iptr:lenr ), 'VALID UNTIL' ) 
                astop = conar(iptr+ival+11: iptr+ival+14)
                CALL ST_INCH ( iotarr(3), day, ier )
                CALL ST_LSTR ( day, lens, ier )
                IF ( lens .eq. 1 ) THEN
                    day = '0'// day
                END IF
                astop = day // astop
C
C*              Get the start and stop time in GEMPAK format.
C
                strtim = ' '
                stptim = ' '
                CALL CS_CTIM ( astart, astop, iotarr, strtim, stptim, 
     +                         ier )
 	        IF ( strtim .eq. ' ' ) THEN
                    ierr = -3
 	            CALL DC_WLOG (2, 'DCCSIG', ierr, strbuf (:72), ier)
                END IF
 	        IF ( stptim .eq. ' ' ) THEN
                    ierr = -4
 	            CALL DC_WLOG (2, 'DCCSIG', ierr, strbuf (:72), ier)
                END IF
C
C*              Get the bounds.
C
                ifrom = INDEX ( conar ( iptr:lenr ), 'FROM' ) 

                iisol = INDEX ( conar ( iptr:lenr ), 'ISOL' ) 
                IF ( iisol .eq. 0 ) THEN
                    iptr = iptr + ifrom - 1
                END IF
C
                type  = ' '
                intsy = ' '
                CALL CS_CBND ( conar(iptr:lenr), MAXPTS, type, 
     +			       intsy, dist, npt, rlat, rlon, 
     +			       inxt, ier )
C
C*	        Check if distance is a number.
C
                IF ( dist .ne. ' ' ) THEN
                    CALL ST_NUMB ( dist, idist, ier1 )
 	            IF ( ier1 .ne. 0 ) THEN
                        dist = ' '
                        strbuf = seq
                        ierr = -10
 	                CALL DC_WLOG ( 2, 'DCCSIG', ierr, strbuf, ier )
                    END IF
                END IF
C
C*              Get the storm movement.
C
                dir = ' '
                spd = ' '
                itop = INDEX ( conar ( iptr:lenc), 'TOPS' ) 
                ikt = INDEX ( conar ( iptr:iptr+itop), 'KT.' ) 
                IF ( ikt .eq. 0 ) THEN
                    ikt = INDEX ( conar ( iptr:iptr+itop), 'KT ' )
                    IF ( ikt .eq. 0 ) THEN
                        ikt = INDEX ( conar ( iptr:iptr+itop), 'KT' )
                    END IF
                END IF
                IF ( ikt .ne. 0 ) THEN
                    dir = conar ( iptr+ikt-6:iptr+ikt-4)
                    spd = conar ( iptr+ikt-3:iptr+ikt-2)
C
C*	            Check if direction and speed are numbers.
C
                    CALL ST_NUMB ( dir, idir, ier1 )
 	            IF ( ier1 .ne. 0 ) THEN
                        dir = ' '
                        strbuf = seq
                        ierr = -11
 	                CALL DC_WLOG ( 2, 'DCCSIG', ierr, strbuf, ier )
                    END IF
                    CALL ST_NUMB ( spd, ispd, ier1 )
 	            IF ( ier1 .ne. 0 ) THEN
                        spd = ' '
                        strbuf = seq
                        ierr = -12
 	                CALL DC_WLOG ( 2, 'DCCSIG', ierr, strbuf, ier )
                    END IF
                END IF
C
C*              Get the flight level.
C
                flvl(1) = ' '
                flvl(2) = ' '
                CALL CS_FLVL ( conar(iptr:lenc), flvl, inxt, ier)
                CALL ST_NUMB ( flvl(1), iflvl, ier1 )
C
C*	        Check if flight level (1) is a number.
C
 	        IF ( ier1 .ne. 0 ) THEN
                    strbuf = seq
                    ierr = -13
 	            CALL DC_WLOG ( 2, 'DCCSIG', ierr, strbuf, ier )
                END IF
                iptr = iptr + inxt 
C                
C*	        Check for another convective sigmet in the report.
C
	        icon = INDEX ( conar( iptr:lenc ), 'CONVECTIVE SIGMET' )
                iptr = iptr + icon - 1
C
C*              Write out the convective sigmet.
C
                IF ( ( type .ne. ' ' ) .and. ( seq .ne. ' ' ) .and.
     +             ( strtim .ne. ' ' ) .and. ( stptim .ne. ' ' ) ) THEN
                    more = .true.
              	  ELSE
                    more = .false.
                    ier1 = -3
                    strbuf = ' '
 	            CALL DC_WLOG ( 2, 'DCCSIG', ier1, strbuf, ier )
                END IF
C
                IF ( more ) THEN
 	            CALL CS_OUT ( lunf, type, strtim, stptim, seq, 
     +			          intsy, dir, spd, flvl, dist, icor, 
     +			          rlat, rlon,  npt, ier )
                END IF
C
	        IF ( ( ierr .lt. 0 ) .or. ( icon .eq. 0 ) ) THEN
                    done = .true.
                END IF
              ELSE
C
C*              No active convective areas for a region.
C
                rlat(1) = RMISSD
                rlon(1) = RMISSD
                seq  = '0' // reg
                npt  = 0
                type = 'CS'
C
C*              Add 1 hour to the start time to get the stop time.
C
                astop = astart
                CALL CS_CTIM (astart, astop, iotarr, strtim, stptim, 
     +                         ier ) 
                CALL TI_CTOI ( stptim, jarr, ier )
                CALL TI_ADDM ( jarr, 60, jarr, ier )
                CALL TI_ITOC ( jarr, stptim, ier )
C
 		CALL CS_OUT ( lunf, type, strtim, stptim, seq, 
     +			      intsy, dir, spd, flvl, dist, icor, 
     +			      rlat, rlon, npt, ier )
                done = .true.
            END IF
C
 	END DO
C*
	RETURN
	END
