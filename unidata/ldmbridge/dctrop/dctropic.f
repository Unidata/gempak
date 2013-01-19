	SUBROUTINE DCTROPIC ( gemfil, prmfil, maxtim, curtim, iret )
C************************************************************************
C* DCTROPIC								*
C*									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	gemfil, prmfil, curtim
C*
	INTEGER		maxtim, iret
C*
	INTEGER		iflsrc, maxfil, iperr
	INTEGER		lenbul, ifdtyp, ibpnt, nchar
	CHARACTER	bultin*(DCMXBF), seqnum*4, wmorpt*8, oristn*8,
     +                  btime*12, bbb*8, pilhdr*12, errstr*80
	CHARACTER   	sysdt*12, dattmp*12, filnam*132, dattim*15
	CHARACTER   	name*80, testname*80, stname*80
	INTEGER  	istarr (5), iotarr (5)
	INTEGER		RTYPE

	INTEGER		MAX_HOURS, NUMVARS
	parameter	(MAX_HOURS=20, NUMVARS=3)
	REAL		slat(MAX_HOURS), slon(MAX_HOURS) 
        REAL            rdata(NUMVARS,MAX_HOURS), report(NUMVARS)
	INTEGER 	nparms, discuss, start_index
	CHARACTER*4	parms(MMPARM)
	CHARACTER*8	stid
C------------------------------------------------------------------------
	CALL IN_BDTA ( iret )
C
	iflsrc = 2
	maxfil = 2

	CALL DC_FINT ( maxfil, iflsrc, prmfil, iperr )
C
C*	Loop until a timeout occurs.
C
        iperr = 0
	DO WHILE  ( iperr .eq. 0 )
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL ( bultin, lenbul, ifdtyp, iperr )
	    IF  ( iperr .eq. 0 )  THEN
C
C*		Parse the header info from the bulletin.
C
		IF  ( ifdtyp .eq. 0 )  THEN
		    CALL DC_GHDR  ( bultin, lenbul, seqnum, wmorpt,
     +				    oristn, btime, bbb, nchar, ierr )
		  ELSE
		    CALL DC_GPIL  ( bultin, lenbul, pilhdr, wmorpt,
     +				    oristn, btime, nchar, ierr )
		END IF
		IF  ( ierr .ne. 0 )  THEN
		    CALL DC_WLOG ( 1, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin(:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 1, ' ', 0, errstr, ier )
		END IF

                RTYPE = 0
                if(wmorpt(1:5).eq.'WTNT4') RTYPE = 1
                if(wmorpt(1:5).eq.'WTPZ4') RTYPE = 2
                if(wmorpt(1:5).eq.'WTPA4') RTYPE = 3
                if(wmorpt(1:5).eq.'WTPN3') RTYPE = 4

                IF(RTYPE .gt. 0) then
                   write(errstr,*) wmorpt, oristn, btime
		   CALL DC_WLOG (2, 'DCTROP', 0 ,errstr, ier)   
C
C*		   Set the bulletin pointer to one character past the end
C*		   of the WMO header.
C
		   ibpnt = nchar + 1
C
C*		   Get the system time, and make a standard GEMPAK time
C*		   from the "current" time.
C
		   itype = 1
		   CALL CSS_GTIM  ( itype, sysdt, ier )
		   IF  ( curtim .eq. 'SYSTEM' )  THEN
		       dattmp = sysdt
		   ELSE
		       CALL TI_STAN ( curtim, sysdt, dattmp, ier )
		   END IF
		   CALL TI_CTOI ( dattmp, istarr, ier )

                   iotarr(1) = IMISSD
                   iotarr(2) = IMISSD
                   iotarr(3) = IMISSD
                   iotarr(4) = IMISSD
                   iotarr(5) = IMISSD
                   do j=1,MAX_HOURS
                      slat(j) = RMISSD
                      slon(j) = RMISSD
                      do i=1,NUMVARS
                         rdata(i,j) = RMISSD
                      end do
                   end do
                   if(RTYPE.le.3) call read_nhc (bultin, lenbul, ibpnt, 
     +					NUM_HOURS, slat, slon, rdata, 
     +					iotarr, RTYPE,
     +					name,name_len,discuss)
                   if(RTYPE.eq.4) call read_guam (bultin, lenbul, ibpnt,
     +					NUM_HOURS, slat, slon, rdata, 
     +					iotarr, RTYPE,
     +					name,name_len,discuss)
                   if(RTYPE.gt.0) then
                      CALL RU_RTIM ( istarr, iotarr(3), iotarr(4), 
     +                               iotarr, ier )
                      CALL TI_ITOC  ( iotarr, dattim, iret )
                      
C
C*          Make a file name from the template and the time.
C*          Open the file as part of the open file list.
C
                      start_index = INDEX(gemfil,'@@')
                      if(start_index.gt.0) then
                         CALL ST_UCLC(name,stname,iret)
                         testname = gemfil(1:start_index-1) //
     +                      stname(1:name_len) //
     +                      gemfil(start_index+2:len(gemfil))
                      else
                         testname = gemfil
                      endif
C
                      CALL FL_MNAM ( dattim, testname, filnam, ier )
C
                      CALL DC_FCYL  ( filnam, iflsrc, 'sfmetar_sa.tbl',
     +                         1, maxtim,
     +                         iflno, nparms, parms, ier)
                      if(ier.ne.0) then
                         CALL DC_WLOG ( 0, 'DC', ier, filnam, iret)
                      else
                         CALL DC_WLOG(1,'DC',ier,filnam,iret)
                         stid = name(1:8)
                         do j=1,NUM_HOURS
                            report(1) = rdata(1,j)
                            report(2) = rdata(2,j)
                            report(3) = rdata(3,j)
                            if((slat(j).ne.RMISSD).and.
     +                         (slon(j).ne.RMISSD)) then
                               CALL SF_WSDD (iflno, dattim, stid, 
     +                            discuss, slat(j), slon(j), 0,
     +                            '--', '--', iotarr(3), report,
     +                            ier)
                               if(ier.ne.0) 
     +                            CALL DC_WLOG(0,'SF',ier,filnam,iret)
                            endif
                         end do
                      endif
                   else
                      CALL DC_WLOG(0,'READ_NHC',RTYPE,wmorpt,iret)
                   endif
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
