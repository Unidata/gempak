	SUBROUTINE DCSELS_OUT ( iflno, dattim, rdata, iret)
C************************************************************************
C* DCSELS								*
C*									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C
	INTEGER		iflno,iret
	CHARACTER	dattim*(*)
	REAL		rdata(*)
C
	REAL		rdout(4)
	CHARACTER	stype,errstr*132
	INTEGER		ihour,idno
	CHARACTER	stid*8,ostid*8
	REAL		slat,slon,selv
	CHARACTER	state*2,country*2

	DATA	state,country /'--','--'/
C************************************************************************
        iret = 0
C
	IF (rdata(1).eq.1) THEN
           stype = 'T'
        ELSE IF (rdata(1).eq.2) THEN
	   stype = 'W'
        ELSE IF (rdata(1).eq.3) THEN
	   stype = 'A'
	ELSE IF (rdata(1).eq.4) THEN
	   stype = 'G'
	ELSE
	   stype = 'U'
	END IF
C
        idno = 0
        ihour = int(rdata(4))
C
C*      See if time is already in file	  
C 
	CALL SF_STIM (iflno, dattim, ier)
C
        IF (ier.ne.0) THEN
           write(stid,1000) stype,ihour,idno
        ELSE
           DO WHILE ((ier.eq.0).and.(idno.lt.1000))
              write(stid,1000) stype,ihour,idno
1000          FORMAT(1A,I4.4,I3.3)
              CALL SF_SSTN (iflno, stid, ostid, isnum, slat, slon,
     +                      selv, ispri, ier)
              IF (ier.eq.0) THEN
                 IF ((abs(slat-rdata(5)).gt.0.012).or.
     +              (abs(slon-rdata(6)).gt.0.012)) THEN
                    idno = idno + 1
                 ELSE
                    idno = 1000
                 END IF
              END IF
           END DO
        END IF

        IF (idno.eq.1000) THEN
           write(errstr,1001) stid, rdata(5), rdata(6)
1001	   FORMAT('Duplicate report ',A8,' Lat = ',F6.2,' Lon = ',F7.2)
           CALL DC_WLOG(2,'DCSELSOUT',0,errstr,iout)
        ELSE
           write(errstr,1002) stid, rdata(5), rdata(6)
1002	   FORMAT('Storm: ',A8,' Lat = ',F6.2,' Lon = ',F7.2)
           CALL DC_WLOG(1,'DCSELSOUT',0,errstr,iout)
           slat = rdata(5)
           slon = rdata(6)
           selv = 0
           isnum = rdata(3)*10000 + rdata(4)
	   rdout(1) = rdata(1)
           rdout(2) = rdata(2)
           rdout(3) = rdata(3)
           rdout(4) = rdata(4)
           CALL SF_WSDD (iflno, dattim, stid, isnum, slat, slon,
     +                   selv,state,country,ihour,rdout,iret)
        END IF
C        
	RETURN
	END
