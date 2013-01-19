	SUBROUTINE CS_OTLK ( lunf, outar, leno, reg, iotarr, icor, 
     +			     inon, iret )
C************************************************************************
C* CS_OTLK 								*
C*									*
C* This subroutine processes a convective outlook report.		*
C*                                                                      *
C* CS_OTLK ( LUNF, OUTAR, LENO, REG, IOTARR, ICOR, INON, IRET )		*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		Unit file number                *
C*	OUTAR		CHAR*		Convective outlook section	*
C*	LENO		INTEGER		Length of sectioni              *
C*	REG		CHAR*		Valid region                    *
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*	ICOR		INTEGER		Correction flag                 *
C*	INON		INTEGER		Convection flag                 *
C*					  = 0 - convection area(s)	*
C*					  > 0 - no convection area(s)	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	 8/02	Created					*
C* A. Hardy/NCEP 	 8/02   Revised method for finding end of FROM  *
C* A. Hardy/NCEP 	10/02   Fixed typo on length of string - leno	*
C* A. Hardy/NCEP	 1/04   Modified to decode nil type reports	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	outar, reg
	INTEGER		iotarr (*)
C*
	PARAMETER	( MAXPTS = 40 )
	CHARACTER	strbuf*250, type*2, strtim*20, stptim*20, 
     +			astart*20, astop*20, flvl (2)*4, seq*3, 
     +			dir*3, spd*2, dist*3, intsy*7
	REAL		rlat (MAXPTS), rlon (MAXPTS)
	LOGICAL		done, more
C------------------------------------------------------------------------
	iret  = 0
C
C*	Ensure that report is upper case.
C
	CALL ST_LCUC ( outar, outar, ier )
C
        ierr    = 0
        iend    = 0
        ilvl    = 0
	ibeg    = 1
        iptr    = 1
        type    = 'OL'
        dir     = ' '
        spd     = ' '
        dist    = ' '
        intsy   = ' '
        flvl(1) = ' '
        flvl(2) = ' '
	done    = .false.
        more    = .true. 
	strbuf  = outar( :160 )
C
C*      Break apart the Convective Outlook section.
C
        iarea = INDEX ( outar ( iptr:leno ), 'AREA' )
	DO WHILE ( ( ierr .eq. 0 ) .and. ( .not. done ) ) 
            IF (iptr .eq. 1 ) THEN
C
C*              Get start and stop times.
C
                astart = outar(iptr+14: iptr+19)
                astop =  outar ( iptr+21:iptr+26)
C
C*              Make GEMPAK times from the convective start and 
C*		stop times.
C
                CALL WW_CTIM ( astart, astop, iotarr, strtim, 
     +                                 stptim, ier )
 	        IF ( strtim .ne. ' ' ) THEN
                    CALL TI_ITOC ( iotarr, strtim, ier )
                  ELSE
                    ierr = -3
 	            CALL DC_WLOG ( 2, 'DCCSIG', ierr, 
     +                                     strbuf ( :72 ), ier )
                END IF
 	        IF ( stptim .eq. ' ' ) THEN
                    ierr = -4
 	            CALL DC_WLOG ( 2, 'DCCSIG', ierr, 
     +                                     strbuf ( :72 ), ier )
                END IF
            END IF 
C
C*          Have convective outlook area(s).
C
            IF ( inon .eq. 0 ) THEN
C
C*              Get convective ID area.
C
                IF ( iarea .eq. 0 ) THEN
                    seq = '1' // reg
                  ELSE
                    iptr = iptr + iarea - 1 
                    seq =  outar(iptr+4:iptr+5) // reg
                END IF
C
C*              Get the bounds.
C
                iwst= INDEX ( outar ( iptr:leno ), 'WST' )
                IF ( iwst .ne. 0 ) THEN
                    iocnl= INDEX ( outar ( iptr:leno ), 'OCNL WST' )
                    IF ( ( iocnl .ne. 0 ) .and. 
     +					  (iocnl .lt. iwst ) ) THEN 
                        iwst = iocnl
                    END IF
                    iref= INDEX ( outar ( iptr:leno ), 'REF WW' )
                    IF ( ( iref .ne. 0 ) .and. ( iref .lt. iwst ) ) THEN
                        iwst = iref
                    END IF
                  ELSE
                    iwst = leno
                END IF
C
                CALL CS_OBND ( outar(iptr:iptr+iwst-2), MAXPTS, 
     +			      npt, rlat, rlon, inxt, ier )
                iptr = iptr + iwst - 1
C
C*              Write the outlook section to an ASCII file.
C
                IF ( ( type .eq. ' ' ) .or. ( seq .eq. ' ' ) .or.
     +               ( strtim .eq. ' ' ) .or. ( stptim .eq. ' ' ) ) THEN
                    more = .false.
                END IF
C
                IF ( more ) THEN
 	            CALL CS_OUT ( lunf, type, strtim, stptim, seq, 
     +			          intsy, dir, spd, flvl, dist, icor, 
     +			          rlat, rlon, npt, ier ) 
                END IF
C
C*	        Check for another convective outlook area in the report.
C
	        iarea = INDEX ( outar( iptr:leno ), 'AREA' )
                IF ( iarea .eq. 0 ) THEN
                    done = .true.
                END IF
	        IF ( ierr .lt. 0 ) done = .true.
              ELSE
C
C*              No active convective areas for a region.
C
                rlat(1) = RMISSD
                rlon(1) = RMISSD
                seq  = '0' // reg
                npt  = 0
		CALL CS_OUT ( lunf, type, strtim, stptim, seq, 
     +			      intsy, dir, spd, flvl, dist, icor, 
     +			      rlat, rlon, npt, ier )
                done = .true.
            END IF
	END DO
C*
	RETURN
	END
