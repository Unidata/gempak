	PROGRAM DCUAIR
C************************************************************************
C* PROGRAM DCUAIR							*
C*									*
C* This program decodes upper air data in real time.			*
C*									*
C**									*
C* Log:									*
C* B. Doty/RDS		10/87						*
C* M. desJardins/GSFC	12/87						*
C************************************************************************
	INCLUDE		'GEMINC:GEMPRM.PRM'
C*
	CHARACTER	snfile*72, systim*15, dattim*15
	CHARACTER	bultin*10000, report*10000, part*4
	CHARACTER	parms (MMPARM)*4, subtyp*2
	INTEGER		istarr (5), iotarr (5)
	LOGICAL		wnknot, mrgdat, done, more, good
C------------------------------------------------------------------------
C*      Initialize common areas.
C
	CALL GD_INIT  ( ier )
C
C*      Initialize real time system.
C
        CALL RL_DINI ( 'DCUAIR', 20, iret )
        IF ( iret .ne. 0 ) STOP
	CALL TI_GTIM  ( systim, ier )
        WRITE ( 6, * ) ' HELLO FROM DCUAIR ', systim
C
C*      Get output sounding file name.
C                              
        CALL RL_GKEY  ( 'UAIRFL', snfile, iret ) 
        IF ( iret .ne. 0 ) STOP
C
C*	Open file and check for open error.
C
        WRITE ( 6, * ) 'SNFILE:', snfile
	CALL SN_OPNR  ( snfile, isnfln, iflsrc, nparm, parms,
     +			ivert,  mrgdat, iret ) 
        WRITE ( 6, * ) 'SN_OPNR--isnfln,iret:', isnfln, iret
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SN', iret, snfile, ier )
	    STOP
	  ELSE IF  ( mrgdat )  THEN
	    WRITE ( 6, * ) 'Sounding file is not unmerged'
	    STOP
	END IF
C
C*	Loop reading next bulletin.
C
	done = .false.
	DO  WHILE  ( .not. done )
C
C*	    Get next bulletin.
C
            CALL RL_DINT ( subtyp, bultin, ibpnt, lenb, iret )
C
C*	    Check for a problem ( probably buffer too small ).
C
	    IF  ( iret .ne. 0 )  THEN
		more = .false.
		WRITE (6,*) 'ERROR FROM RL_DINT:',IRET
		IF  ( iret .eq. -30 )  done = .true.
C
C*		Remove unprintable characters from bulletin.
C
	      ELSE
		more  = .true.
		lens  = lenb - ibpnt + 1
		CALL ST_UNPR  ( bultin ( ibpnt : ), lens,
     +				bultin ( 1 : ),     lennew, ier )
		lenb  = lennew
		ibpnt = 1
	    END IF
C
C*          Get current time; convert to integer; convert to Greenwich.
C 
            CALL TI_GTIM  ( systim, ier1 )
	    CALL TI_CTOI  ( systim, istarr, ier2 )
	    CALL TI_GREN  ( istarr, istarr, ier3 )
	    IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or.
     +		  ( ier3 .ne. 0 ) )  more = .false.
C
C*	    Loop through reports.
C
	    DO  WHILE  ( more )
C
C*		Get next report.
C
		CALL RU_GRPT ( bultin, lenb, ibpnt, report, lenr, iret )
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
C*		    Decode station header.
C
		    CALL RU_SHDR  ( report, lenr, irpnt, part, istnm,
     +				    iday,  ihour, wnknot, itopwn, iret )
		    IF  ( iret .ne. 0 )  good = .false.
		END IF
C
C*		Get time to assign to this bulletin.
C
		IF  ( good )  THEN
C
C*		    Get observation time.
C
		    CALL RU_RTIM  ( istarr, iday, ihour, iotarr, ier )
		    ihhmm = iotarr (4) * 100
C
C*		    Compute difference between observation and system
C*		    times.
C
		    CALL TI_MDIF  ( iotarr, istarr, imdif, ier1 )
C
C*		    Adjust the time to the nearest three hours.
C
		    CALL RU_ADJT  ( iotarr, dattim, ier2 )
C
C*		    Check that the time is within 31 hours before
C*		    the system time.
C
		    IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or.
     +			  ( imdif .gt. 90 ) .or. ( imdif .lt. -1880 ) )
     +								THEN 
			good = .false.
			WRITE (6,*) 'INVALID DATTIM: ', dattim, systim,
     +					iday, ihour, imdif, istnm,
     +					report ( 1 : 20 )
		    ENDIF
		END IF
C
C*		Decode data and write to sounding file.
C
		IF  ( good )  THEN
		    CALL RU_DECD  ( isnfln, dattim, istnm, part, itopwn,
     +				    wnknot, ihhmm,  report, lenr, irpnt,
     +				    iret )
		END IF
	    END DO
	END DO
C
C*	Close sounding file.
C
	CALL SN_CLOS  ( isnfln, ier )
	WRITE (6,*) 'Sounding file was closed--isnfln,ier: ',isnfln,ier
C*
	STOP
	END		
