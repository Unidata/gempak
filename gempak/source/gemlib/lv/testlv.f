	PROGRAM TESTLV
C************************************************************************
C* TESTLV								*
C*									*
C* This program tests the LEVEL library subroutines.			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/86						*
C* M. desJardins/GSFC	 9/88	Rewritten for GEMPAK4			*
C* M. desJardins/NMC	10/91	Add test for LV_CCRD			*
C* S. Jacobs/NMC	 3/95	Update call to LV_INPT			*
C************************************************************************
	REAL		rlevel (100)
	CHARACTER	vparm*8, clevel*72, vcord*8
	CHARACTER	start*20, stop*20, inc*20
	LOGICAL		mandat
C-------------------------------------------------------------------------
	CALL IN_BDTA ( iret )
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
	    WRITE  ( 6, 20 )
20	    FORMAT 
     +  (/'  1 = LV_CORD   2 = LV_DECD   3 = LV_DUPL   4 = LV_GRNG'/
     +    '  5 = LV_INPT   6 = LV_MANL   7 = LV_SORT   8 = LV_VASL'/
     +    '  9 = LV_CCRD' )
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier )
	IF ( ier .eq. 2 ) THEN
           iostat = -1
           numsub = -1
	END IF
C--------------------------------------------------------------------------
	    IF  ( numsub .eq. 1 )  THEN
		WRITE (6,*) 'ENTER VCORD'
		READ  (5,2)  VCORD
		CALL LV_CORD  ( VCORD, VPARM, IVERT, IRET )
		WRITE (6,*) 'VPARM,IVERT,IRET: ',VPARM,IVERT,IRET
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 2 )  THEN
		WRITE (6,*) 'ENTER CLEVEL'
		READ  (5,2)  CLEVEL
2		FORMAT (A)
		CALL LV_DECD  ( CLEVEL, RLEV, IRET )
		WRITE (6,*) 'RLEV,IRET: ',RLEV,IRET
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 3 )  THEN
		WRITE (6,*) 'ENTER NLEV'
		READ  (5,*)  NLEV
		WRITE (6,*) 'ENTER',NLEV,'LEVELS'
		READ  (5,*) (RLEVEL (J),J=1,NLEV)
		CALL LV_DUPL  ( NLEV, RLEVEL, IRET )
		WRITE (6,*) 'NLEV, IRET = ', NLEV, IRET
		WRITE (6,*) 'RLEVEL: ', (RLEVEL(I),I=1,NLEV)
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 4 )  THEN
		WRITE (6,*) 'ENTER START'
		READ  (5,2)  START
		WRITE (6,*) 'ENTER STOP'
		READ  (5,2)  STOP
		WRITE (6,*) 'ENTER INC'
		READ  (5,2)  INC
		WRITE (6,*) 'ENTER NEXP'
		READ  (5,*)  NEXP
		CALL LV_GRNG ( START, STOP, INC, NEXP, RLEVEL, NLEV,
     +				IRET )
		WRITE (6,*) 'NLEV,IRET: ',NLEV, IRET
		WRITE (6,*) 'RLEVEL:',(RLEVEL(I),I=1,NLEV)
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 5 )  THEN
		WRITE (6,*) 'ENTER CLEVEL'
		READ  (5,2)  CLEVEL
		WRITE (6,*) 'ENTER VCOORD'
		READ  (5,2)  VCORD
		WRITE (6,*) 'ENTER NEXP'
		READ  (5,*)  NEXP
		CALL  LV_INPT  ( CLEVEL, NEXP, VCORD, NLEV, RLEVEL,
     +				 LEVTYP, VPARM, IVERT, MANDAT, IRET )
		WRITE (6,*) 'NLEV,LEVTYP,IVERT,MANDAT,IRET',
     +				NLEV,LEVTYP,IVERT,MANDAT,IRET
		IF  ( levtyp .ne. 2 )  THEN
		    WRITE (6,*) 'RLEVEL: ',(RLEVEL(I),I=1,NLEV)
		  ELSE
		    WRITE (6,*) 'RLEVEL: ',(RLEVEL(I),I=1,2)
		END IF
		WRITE (6,*) 'VPARM: ',VPARM
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 6 )  THEN
		WRITE (6,*) 'ENTER NEXP'
		READ  (5,*)  NEXP
		CALL LV_MANL  ( NEXP, NLEV, RLEVEL, IRET )
		WRITE (6,*) 'NLEV,IRET: ',NLEV,IRET
		WRITE (6,*) 'RLEVEL: ',(RLEVEL(I),I=1,NLEV)
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 7 )  THEN
		WRITE (6,*) 'ENTER IVERT,NLEV'
		READ  (5,*)  IVERT,NLEV
		WRITE (6,*) 'ENTER RLEVEL: '
		READ  (5,*)  (RLEVEL(I),I=1,NLEV)
		CALL LV_SORT ( IVERT, NLEV, RLEVEL, IRET )
		WRITE (6,*) 'IRET= ', IRET
		WRITE (6,*) 'RLEVEL: ', (RLEVEL(I),I=1,NLEV) 
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 8 )  THEN
		WRITE (6,*) 'ENTER NEXP'
		READ  (5,*)  NEXP
		CALL LV_VASL ( NEXP, NLEV, RLEVEL, IRET)
		WRITE (6,*) 'NLEV,IRET: ',NLEV,IRET
		WRITE (6,*) 'RLEVEL: ',(RLEVEL(I),I=1,NLEV)
C-------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 9 )  THEN
		WRITE (6,*) 'ENTER IVCORD'
		READ  (5,*)  IVCORD
		CALL LV_CCRD ( IVCORD, VCORD, IRET )
		WRITE (6,*) 'IVCORD, VCOORD, IRET: ', IVCORD, ' ', VCORD, 
     +				IRET
C-------------------------------------------------------------------------
	    END IF
	END DO
	END
