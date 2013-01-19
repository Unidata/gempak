	SUBROUTINE DCSELS ( gemfil, prmfil, maxtim, curtim, stntbl, iret)
C************************************************************************
C* DCSELS								*
C*									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	gemfil, prmfil, curtim, stntbl
C*
	INTEGER		maxtim, iret
C*
	INTEGER		iflsrc, maxfil, iperr
	INTEGER		lenbul, ifdtyp, ibpnt, nchar
	CHARACTER	bultin*(DCMXBF), seqnum*4, wmorpt*8, oristn*8,
     +                  btime*12, bbb*8, pilhdr*12, errstr*80
	CHARACTER   	sysdt*12, dattmp*12, filnam*132, dattim*15
	INTEGER  	istarr (5), iotarr (5)
	INTEGER		RTYPE
	LOGICAL		more, proces

	INTEGER		NUMVARS
	parameter	(NUMVARS=6)
        REAL            rdata(NUMVARS)
	INTEGER 	nparms
	CHARACTER*4	parms(MMPARM)
C------------------------------------------------------------------------
C
	iflsrc = 2
	maxfil = 3

	CALL DC_FINT ( maxfil, iflsrc, prmfil, iperr )
C
C*	Loop until a timeout occurs.
C
        iperr = 0
	DO WHILE  ( iperr .eq. 0 )
C
C*	   Get the bulletin.
C
	   CALL DC_GBUL ( bultin, lenbul, ifdtyp, iperr )
	   IF ( iperr .eq. 0 )  THEN
C
C*	      Parse the header info from the bulletin.
C
	      IF ( ifdtyp .eq. 0 )  THEN
		 CALL DC_GHDR  ( bultin, lenbul, seqnum, wmorpt,
     +			         oristn, btime, bbb, nchar, ierr )
	      ELSE
	         CALL DC_GPIL  ( bultin, lenbul, pilhdr, wmorpt,
     +				    oristn, btime, nchar, ierr )
	      END IF
C
	      IF  ( ierr .ne. 0 )  THEN
		    CALL DC_WLOG ( 1, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin(:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 1, ' ', 0, errstr, ier )
	      END IF

              RTYPE = 0

              IF (wmorpt(1:6) .eq. 'WWUS60') RTYPE = 1

              IF  ( ( wmorpt(1:5) .eq. 'NWUS2' ) .and. 
     +		   ( oristn(1:4) .eq. 'KWNS' ) ) RTYPE = 2

              IF (RTYPE .gt. 0) THEN
                 write(errstr,*) wmorpt, oristn, btime
		 CALL DC_WLOG (2, 'DCSTORM', 0 ,errstr, ier)   
C
C*		 Set the bulletin pointer to one character past the end
C*		 of the WMO header.
C
		 ibpnt = nchar + 1
C
C*		 Get the system time, and make a standard GEMPAK time
C*		 from the "current" time.
C
		 itype = 1
		 CALL CSS_GTIM  ( itype, sysdt, ier )
		 IF  ( curtim .eq. 'SYSTEM' )  THEN
		     dattmp = sysdt
		 ELSE
		     CALL TI_STAN ( curtim, sysdt, dattmp, ier )
		 END IF
		 CALL TI_CTOI ( dattmp, istarr, ier )

                 more = .true.
                 DO WHILE (more)
                    iotarr(1) = IMISSD
                    iotarr(2) = IMISSD
                    iotarr(3) = IMISSD
                    iotarr(4) = IMISSD
                    iotarr(5) = IMISSD
                    do i=1,NUMVARS
                       rdata(i) = RMISSD
                    end do
		    proces = .false.
C
                    CALL read_storm (bultin, lenbul, ibpnt, rdata, ier)
C
		    IF ( ier .lt. 0 ) THEN
		       more = .false.
		    ELSE IF ( ier .eq. 0 ) THEN
		       proces = .true.
                       iotarr(3) = rdata(3)
                       iotarr(4) = rdata(4) / 100
                       CALL RU_RTIM ( istarr, iotarr(3), iotarr(4), 
     +                    iotarr, ier )
		       IF ( ier .ne. 0 ) proces = .false.
		    END IF 
C
		    IF ( proces ) THEN   
                       iotarr(5) = mod(int(rdata(4)),100)
                       CALL TI_ITOC  ( iotarr, dattim, ier )
		       IF ( ier .ne. 0 ) proces = .false.
		    END IF

                    IF ( proces ) THEN
C
C*          Make a file name from the template and the time.
C*          Open the file as part of the open file list.
C
                        CALL FL_MNAM ( dattim, gemfil, filnam, ier )
C
                        CALL DC_FCYL  ( filnam, iflsrc, stntbl,
     +                         1, maxtim, iflno, nparms, parms, ier)
                        IF (ier .eq. 0) THEN
                            CALL dcsels_out(iflno, dattim, rdata, ier)
			ELSE
			    proces = .false.
                            CALL DC_WLOG ( 0, 'DCSELS', ier, filnam, 
     +				iret)
                        END IF
                    ELSE IF ( .not. more ) THEN
                        CALL DC_WLOG(1, 'DCSELS', ier, wmorpt, iret)
                    END IF
                    
                 END DO
              END IF
C
C
	   ELSE
C
C*		Write an error to the decoder log file.
C
	      CALL DC_WLOG ( 0, 'DC', iperr, ' ', iret )
	   END IF
	END DO
C
	CALL DC_FCLS ( ier )
C*
	RETURN
	END
