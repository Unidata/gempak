	PROGRAM TESTRS
c
c  This program reads obs from a file and prints out the desired parameters
c  It cannot distinguish bulletin headers from obs, so it simply looks
c  for SM or SN at the start of a line
c  John Nielsen/MIT  2/89
c  John Nielsen/TAMU 2/92
c
	CHARACTER*50 FILE
	character*95 mask
	CHARACTER*5000 BULL,NEWBULL
	INTEGER BULLEN,MAXLIN,BINDEX,BULLIN
	character*400 ob
	INTEGER OBLEN
	INTEGER DEFAULT,DAY,HOUR
	INTEGER WNDCOD
	CHARACTER*4 OBTYPE
	CHARACTER*7 NAME
	INTEGER DATA(200),PLACES(200),IT
	LOGICAL rs_RFIL
	REAL MISSING
	REAL RDATA(5)

	DEFAULT=-9999
	MISSING=-9999.99
	maxlin=5000
	maxrpt=200
	DO I=1,200
	  PLACES(I)=0
	END DO
	maxrpt = 400
	TYPE *,'FILE?'
	READ(*,'(A)') FILE
c	file = '/data1/ddplus/synop.12'
	OPEN(UNIT=1,NAME=FILE,STATUS='OLD')
	type *, 'print decoder mask? (1=yes)'
	read(*,'(i)') imask
	type *,'print undecoded obs? (1=yes)'
	read(*,'(i)') iund
100	FORMAT(I3)
	is=1

1	IF(.NOT.RS_RFIL(BULL,MAXLIN,BULLEN,NEWBULL,NEWLEN,1)) STOP
c	type *,bullen, bull(:20)

	DAY=DEFAULT
	HOUR=DEFAULT
	BINDEX=1
	BULLIN=1
	CALL rs_DHDR(BULL,BULLEN,default,BINDEX,DAY,HOUR,obtype,iret)
	CALL rs_DLN2(BULL,bullen,default,BINDEX,DAY,HOUR,
	2		OBTYPE,WNDCOD,IRET)
	type *,bull(1:10), '   ',obtype
c	type *,bull(bindex:bindex+10), day, hour
2	call rs_grpt(BULL,bullen,MAXrpt,BINDEX,OB,OBLEN,iret) 

	IF ( iret .ne. 0 )  GOTO 1
	CALL rs_DECO(OB,OBLEN,OBTYPE,DAY,HOUR,WNDCOD,default,NAME,
	2	DATA,iret)
c	CALL rs_REAL(DATA,PLACES,RDATA,DEFAULT,MISSING)
	l = 68
	if(oblen.lt.60)l=oblen
	IF(iret.eq.0)THEN
	 if(imask.eq.1)then
c	  WRITE(*,'(1X,A9,A<L>)') 'DECODED: ',OB(1:L)
c**	  WRITE(*,'(1X,A7,5(1X,F12.4))') NAME,(RDATA(I),I=1,5)
c	  type *,(data(k),k=15,18)
	  DO i = 1, 92
	    IF  ( data(i) .eq. default )  THEN
		mask(i:i) = '_'
	    ELSE
		    mask(i:i) = 'X'
	    ENDIF
	  END DO
	  write(*,'(1x,A6,1x,A22,1x,A14,1x,A8,1x,A27,1x,a14)') 
     +	  mask(1:6),mask(7:28),mask(29:42),mask(43:50),mask(51:77),
     +	  mask(78:91)
	 endif
	ELSE IF((IS.EQ.1).and.(iund.eq.1)) THEN
	  WRITE(*,'(1X,A11,A<L>)') 'UNDECODED: ',OB(1:L)
	END IF
	GOTO 2
	END 
C
C***********************************************************************
C
	LOGICAL FUNCTION rs_RFIL(BULL,maxlin,BULLEN,NEWBULL,
	2		NEWLEN,LUN_OB)  	

!  Crude testing program to pull a simulated ob from a file

	CHARACTER*(*)	BULL,NEWBULL
	INTEGER		maxlin,BULLEN,NEWLEN
	INTEGER		LUN_OB

	rs_RFIL=.TRUE.
	BULLEN=0
	IF(NEWBULL(1:1).EQ.'S') THEN
	    BULL=NEWBULL
	    LINES=1
	    BULL(NEWLEN+1:NEWLEN+1)=CHAR(10)
	    BULLEN=NEWLEN+1
	END IF
	NEWBULL=' '
	NEWLEN=0
1	IF  ( bullen + 100 .gt. maxlin )  GOTO 900
	READ(LUN_OB,101,END=940)INLEN,BULL(BULLEN+1:BULLEN+100)
101	FORMAT(Q,A<INLEN>)
	IF(INLEN.LT.10) THEN
	   LINES=LINES+1
	   BULLEN=BULLEN+INLEN+1
	   BULL(BULLEN:BULLEN)=CHAR(10)
	   GOTO 1
	END IF
	IF(	(BULL(BULLEN+1:BULLEN+2).EQ.'SM').OR. 
     +		(BULL(BULLEN+1:BULLEN+2).EQ.'SN').OR.
     +		(BULL(BULLEN+1:BULLEN+2).EQ.'SX')) THEN
	   NEWBULL(1:INLEN)=BULL(BULLEN+1:BULLEN+INLEN)
	   NEWLEN=INLEN
	   RETURN
	END IF
	LINES=LINES+1
	BULLEN=BULLEN+INLEN+1
	BULL(BULLEN:BULLEN)=CHAR(10)
	GOTO 1

900	READ(lun_ob,101,end=940)inlen,newbull
	IF(	(newbull(1:2) .eq. 'SM') .or.
     +		(newbull(1:2) .eq. 'SN') .or.
     +		(newbull(1:2) .eq. 'SX') ) then
	    newlen = inlen
	    return
	end if
	goto 900

940	IF(BULLEN.EQ.0) rs_RFIL=.FALSE.
990	RETURN
	END

