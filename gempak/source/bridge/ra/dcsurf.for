	PROGRAM DCSURF
C************************************************************************
C* PROGRAM DCUAIR							*
C*									*
C* This program decodes surface airways data in real time.		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/89	GEMPAK 5				*
C* J. Whistler/SSAI	 2/91	Modified to read in more parms		*
C* K. Brill/NMC		 8/93	stid*4 -> stid*8			*
C************************************************************************
	INCLUDE		'GEMINC:GEMPRM.PRM'
C*
	CHARACTER	sffile*72, systim*16, rtime*16, stid*8,
     +			bultin*10000, report*512, wcod*8, subtyp*8,
     +			parms (MMPARM)*4, wmohdr*8, bhdr*50, btime*12,
     +			oristn*8, wmorpt*8, autop*8, coun*4, rpttyp*4
	REAL		rdata (MMPARM)
	INTEGER		istarr (5), irtarr (5)
	LOGICAL		done, more, good, corflg, datflg, addstn, cirflg
C*
	PARAMETER	( MAXCLD = 5 )
	REAL		cldtyp (MAXCLD), cldhgt (MAXCLD)
C------------------------------------------------------------------------
C*      Initialize common areas.
C
	CALL IN_BDTA  ( ier )
C
C*      Initialize real time system.
C
        CALL RG_DINT  ( 'DCSURF', sffile, iret )
        IF  ( iret .ne. 0 )  STOP
C
C*	Open file and check for open error.
C
	CALL SF_OPNR  ( sffile, isffln, iflsrc, nparm, parms,
     +			iret ) 
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SF', iret, sffile, ier )
	    STOP
	END IF
C
C*	Check location of parameters in file.
C
	CALL ST_FIND  ( 'PMSL', parms, nparm, ipmsl, ier )
	IF  ( ipmsl .eq. 0 )  ipmsl = nparm + 1
	CALL ST_FIND  ( 'ALTI', parms, nparm, ialti, ier )
	IF  ( ialti .eq. 0 )  ialti = nparm + 1
	CALL ST_FIND  ( 'TMPF', parms, nparm, itmpf, ier )
	IF  ( itmpf .eq. 0 )  itmpf = nparm + 1
	CALL ST_FIND  ( 'DWPF', parms, nparm, idwpf, ier )
	IF  ( idwpf .eq. 0 )  idwpf = nparm + 1
	CALL ST_FIND  ( 'SKNT', parms, nparm, isknt, ier )
	IF  ( isknt .eq. 0 )  isknt = nparm + 1
	CALL ST_FIND  ( 'DRCT', parms, nparm, idrct, ier )
	IF  ( idrct .eq. 0 )  idrct = nparm + 1
	CALL ST_FIND  ( 'GUST', parms, nparm, igust, ier )
	IF  ( igust .eq. 0 )  igust = nparm + 1
	CALL ST_FIND  ( 'WNUM', parms, nparm, iwnum, ier )
	IF  ( iwnum .eq. 0 )  iwnum = nparm + 1
	CALL ST_FIND  ( 'CHC1', parms, nparm, ichc1, ier )
	IF  ( ichc1 .eq. 0 )  ichc1 = nparm + 1
	CALL ST_FIND  ( 'CHC2', parms, nparm, ichc2, ier )
	IF  ( ichc2 .eq. 0 )  ichc2 = nparm + 1
	CALL ST_FIND  ( 'CHC3', parms, nparm, ichc3, ier )
	IF  ( ichc3 .eq. 0 )  ichc3 = nparm + 1
	CALL ST_FIND  ( 'VSBY', parms, nparm, ivsby, ier )
	IF  ( ivsby .eq. 0 )  ivsby = nparm + 1
	CALL ST_FIND  ( 'PT03', parms, nparm, ipt03, ier )
	IF  ( ipt03 .eq. 0 )  ipt03 = nparm + 1
	CALL ST_FIND  ( 'P03I', parms, nparm, ip03i, ier )
	IF  ( ip03i .eq. 0 )  ip03i = nparm + 1
	CALL ST_FIND  ( 'HSUN', parms, nparm, ihsun, ier )
	IF  ( ihsun .eq. 0 )  ihsun = nparm + 1
	CALL ST_FIND  ( 'SNOW', parms, nparm, isnow, ier )
	IF  ( isnow .eq. 0 )  isnow = nparm + 1
	CALL ST_FIND  ( 'TMPX', parms, nparm, itmpx, ier )
	IF  ( itmpx .eq. 0 )  itmpx = nparm + 1
	CALL ST_FIND  ( 'P24I', parms, nparm, ip24i, ier )
	IF  ( ip24i .eq. 0 )  ip24i = nparm + 1
C
C*	Set ADDSTN to .true. to add new stations to the file.
C*	Set CIRFLG to .true. to use file as a circular file.
C
	addstn = .true.
	cirflg = .true.
C
C*	Loop reading next bulletin.
C
	done = .false.
	DO  WHILE  ( .not. done )
C
C*	    Get next bulletin.
C
            CALL RG_GBUL  ( wmohdr, subtyp, bultin, ibpnt, lenb, iret )
C
C*	    Check for a problem ( probably buffer too small ).
C
	    IF  ( iret .ne. 0 )  THEN
		more = .false.
		WRITE  (6,*)  'ERROR FROM RG_GBUL:', iret
		IF  ( iret .eq. -99 )  done = .true.
	      ELSE
		more  = .true.
		lens  = lenb - ibpnt + 1
C
C*		Get bulletin header which contains the bulletin time
C*		which will be used to compute station time.
C
		CALL ST_UNPR  ( bultin ( ibpnt: ibpnt+50 ), 50,
     +				bhdr, lenhdr, ier )
		CALL RG_GHDR  ( bhdr, wmorpt, oristn, btime, ier )
		coun = wmorpt (3:4)
		IF  ( ier .ne. 0 )  more = .false.
	    END IF
C
C*          Get current time; convert to integer; convert to Greenwich.
C
	    IF  ( more )  THEN 
		CALL TI_GTIM  ( systim, ier1 )
		CALL TI_CTOI  ( systim, istarr, ier2 )
		CALL TI_GREN  ( istarr, istarr, ier3 )
		IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or.
     +		      ( ier3 .ne. 0 ) )  more = .false.
	    END IF
C
C*	    Loop through reports.
C
	    DO  WHILE  ( more )
C
C*		Get next report.
C
		CALL RA_GRPT ( bultin, lenb, ibpnt, report, lenr, iret )
		irpnt = 1
		good  = .true.
C
C*		Check for the end of the bulletin.
C
		IF  ( iret .ne. 0 )  THEN
		    more = .false.
		    good = .false.
		  ELSE
C
C*		    Break report into fields which are saved in common.
C
		    CALL RA_GFLD  ( report, lenr, iret )
C                                    
C*		    Get station header.
C
		    IF  ( iret .eq. 0 )  THEN
			CALL RA_RHDR  ( irpntr, stid, rpttyp, corflg, 
     +					autop, irhour, irmin, iret )
		    END IF
		    IF  ( iret .ne. 0 )  good = .false.
		END IF
C
C*		Get time to assign to this bulletin.
C
		IF  ( good )  THEN
C
C*		    Get observation time adjusted to the nearest hour.
C
		    CALL RA_RTIM  ( istarr, btime, irhour, irmin,
     +				    irtarr, rtime, ier )
		    ihhmm = irhour * 100 + irmin
C
C*		    Compute difference between observation and system
C*		    times.
C
		    CALL TI_MDIF  ( irtarr, istarr, imdif, ier1 )
C
C*		    Check that the time is within 2 hours before
C*		    the system time.
C
		    IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or.
     +			  ( imdif .gt. 15 ) .or. ( imdif .lt. -300 ) )
     +				good = .false.
		END IF
C
C*		Set station and time in file.
C
		IF  ( good )  THEN
		    CALL RA_TMST  ( isffln, rtime, stid, addstn, cirflg,
     +				    datflg, iret )
C
C*		    Check for error.
C
		    IF  ( iret .ne. 0 )  THEN
			good = .false.
C
C*			If the data has already been decoded and this
C*			is not a correction, do not decode again.
C
		      ELSE IF  ( datflg .and. ( .not. corflg ) )  THEN
			good = .false.
C
C*			Do not process specials here.
C
		      ELSE IF  ( rpttyp .eq. 'SP' )  THEN
			good = .false.
		    END IF
		END IF
C
C*		Decode data and write to sounding file.
C
		IF  ( good )  THEN
		    CALL RA_DECD  ( irpntr, coun, maxcld, irtarr(4),
     +				    cldtyp, cldhgt, ncld, vsby,
     +				    wcod, wnum, pres, tmpf, dwpf,
     +				    sknt, drct, gust, alti, pt03,
     +				    p03i, hsun, snow, tmpx, p24i,
     +				    iret )
		    IF  ( iret .eq. 0 )  THEN
C
C*			Compute low, mid, high clouds.
C
			CALL RA_CLEV  ( cldtyp, cldhgt, ncld, 
     +					chc1, chc2, chc3, ier )
C
C*			Move data into array.
C
			rdata ( ipmsl ) = pres
			rdata ( ialti ) = alti
			rdata ( itmpf ) = tmpf
			rdata ( idwpf ) = dwpf
			rdata ( isknt ) = sknt
			rdata ( idrct ) = drct
			rdata ( igust ) = gust
			rdata ( iwnum ) = wnum
			rdata ( ichc1 ) = chc1
			rdata ( ichc2 ) = chc2
			rdata ( ichc3 ) = chc3 
			rdata ( ivsby ) = vsby
			rdata ( ipt03 ) = pt03
			rdata ( ip03i ) = p03i
			rdata ( ihsun ) = hsun
			rdata ( isnow ) = snow
			rdata ( itmpx ) = tmpx
			rdata ( ip24i ) = p24i
C
C*			Write data to output file.
C
			CALL SF_WDAT  ( isffln, ihhmm, rdata, iret )
		    END IF
		END IF
	    END DO
	END DO
C
C*	Close surface file.
C
	CALL SF_CLOS  ( isffln, ier )
C*
	STOP
	END		
