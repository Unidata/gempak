	SUBROUTINE SNECPR( ptnam, prmlst, numprm, zwind, kntflg, 
     +                     ddpflg, iret )
C************************************************************************
C* SNECPR								*
C*									*
C* This subroutine checks the validity of the unmerged parameter list   *
C* and returns flags describing the parameter options for a part	*
C*									*
C* SNECPR ( PTNAM, PRMLST, NUMPRM, ZWIND, KNTFLG, DDPFLG, IRET)		*
C*									*
C* Input Parameters:							*
C* 	PTNAM		CHAR*		Part name			*
C*	PRMLST		CHAR*		Parameter list from part 	*
C*					header				*
C* Output Parameters:							*
C*      NUMPRM		INTEGER		Number of parameters		*
C*	ZWIND		LOGICAL 	Flag for sig wind in Z coord    *
C*	KNTFLG		LOGICAL         Flag for SKNT parameter		*
C*      DDPFLG		LOGICAL         Flag for DDPC parameter		*
C*      IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = invalid list for part	*
C**									*
C* Log:									*
C* S. Schotz/GSC	11/89						*
C* D. Kidwell/NCEP	 2/01	Added more parts, bug fix for sig wind  *
C************************************************************************
	LOGICAL 	kntflg, ddpflg, zwind
	CHARACTER	ptnam*(*), prmlst*(*)
C*
	PARAMETER       ( NMANPR = 6 , NSIGPR = 3, NTRPPR = 5 )
	CHARACTER	prmarr(NMANPR)*4, manlst(3,NMANPR)*4,
     +            	sigtmp(3,NSIGPR)*4, sigwnd(2,NSIGPR)*4,
     +			manwnd(2,NSIGPR)*4
	LOGICAL		done
C*
        DATA            manlst / 'PRES', 'PRES', 'PRES', 'TMPC', 'TMPC',
     +                           'TEMP', 'DWPC', 'DWPT', 'DDPC', 'DRCT',
     +                           'DRCT', 'DRCT', 'SPED', 'SPED', 'SKNT',
     +                           'HGHT', 'HGHT', 'HGHT'/
C*
	DATA 		sigtmp / 'PRES', 'PRES', 'PRES', 'TMPC', 'TEMP',
     +                           'TEMP', 'DWPC', 'DWPT', 'DDPC' /
C*
	DATA		sigwnd / 'PRES', 'HGHT', 'DRCT', 'DRCT', 'SPED',
     +                           'SKNT' /
C*
	DATA		manwnd / 'PRES', 'PRES', 'DRCT', 'DRCT', 'SPED',
     +                           'SKNT' /
C------------------------------------------------------------------------
	iret = 0
	kntflg = .false. 
	ddpflg = .false.
	zwind = .false.
	CALL ST_LCUC ( prmlst, prmlst, ierr )
C
C*	Decode parameter list into array of parameter names
C
	CALL ST_CLST ( prmlst, ' ', ' ', NMANPR, prmarr, numprm, ierr )
	IF ( ierr .ne. 0) THEN
	    iret = -1
	    RETURN
	END IF
C
C*      Check parameters against valid list for part
C
	IF ( ptnam .eq. 'TTAA' .or. ptnam .eq. 'TTCC' ) THEN
C
C*         Mandatory part
C
	   IF ( numprm .eq. NMANPR ) THEN
	      done = .false.
              ip = 1
C
C*            Check validity of each parameter
C
	      DO WHILE ( ( .not. done ) .and. ( ip .le. NMANPR ) )
	         CALL ST_FIND ( prmarr(ip), manlst(1,ip), NMANPR, ipos, 
     +                          ierr )
		 IF ( ipos .gt. 0 ) THEN
C
C*                  Parameter valid, check for DDPC case or SKNT case
C
                    IF ( ip .eq. 3 ) THEN
			IF ( prmarr(ip) .eq. 'DDPC' ) ddpflg = .true.
	              ELSE IF ( ip .eq. 5 ) THEN
                        IF ( prmarr(ip) .eq. 'SKNT' ) kntflg = .true.
		    END IF
                   ELSE
		    iret = -1
                    done = .true.
	         END IF
		 ip = ip + 1
              END DO
            ELSE
C
C*            Invalid number of parameters
C
	      iret = -1
	   END IF
C
         ELSE IF ( ptnam .eq. 'TTBB' .or. ptnam .eq. 'TTDD' ) THEN
C
C*	   Significant temperatures part
C
	   IF ( numprm .eq. NSIGPR ) THEN
	      done = .false.
              ip = 1
C
C*            Check validity of each parameter
C
	      DO WHILE ( ( .not. done ) .and. ( ip .le. NSIGPR ) )
	         CALL ST_FIND ( prmarr(ip), sigtmp(1,ip), NSIGPR, ipos, 
     +                          ierr )
		 IF ( ipos .gt. 0 ) THEN
C
C*                  Parameter valid, check for DDPC 
C
                    IF ( ip .eq. 3 ) THEN
			IF ( prmarr(ip) .eq. 'DDPC' ) ddpflg = .true.
		    END IF
                  ELSE
		    iret = -1
                    done = .true.
	         END IF
		 ip = ip + 1
              END DO
            ELSE
C
C*            Invalid number of parameters
C
	      iret = -1
	   END IF
C
         ELSE IF ( ptnam .eq. 'PPBB' .or. ptnam .eq. 'PPDD' ) THEN
C
C*	   Significant winds part
C
	   IF ( numprm .eq. NSIGPR ) THEN
	      done = .false.
              ip = 1
C
C*            Check validity of each parameter
C
	      DO WHILE ( ( .not. done ) .and. ( ip .le. NSIGPR ) )
	         CALL ST_FIND ( prmarr(ip), sigwnd(1,ip), NSIGPR, ipos, 
     +                          ierr )
		 IF ( ipos .gt. 0 ) THEN
C
C*                  Parameter valid, check for SKNT, set hght surf flag
C
		    IF ( ip .eq. 1 ) THEN
			IF ( prmarr(ip) .eq. 'HGHT' ) zwind = .true.
                      ELSE IF ( ip .eq. 3 ) THEN
			IF ( prmarr(ip) .eq. 'SKNT' ) kntflg = .true.
		    END IF
                  ELSE
		    iret = -1
                    done = .true.
	         END IF
		 ip = ip + 1
              END DO
            ELSE
C
C*            Invalid number of parameters
C
	      iret = -1
	   END IF
C
         ELSE IF ( ptnam .eq. 'PPAA' .or. ptnam .eq. 'PPCC'  .or.
     +		   ptnam .eq. 'MXWA' .or. ptnam .eq. 'MXWC' ) THEN
C
C*	   Mandatory winds only or maximum wind part
C
	   IF ( numprm .eq. NSIGPR ) THEN
	      done = .false.
              ip = 1
C
C*            Check validity of each parameter
C
	      DO WHILE ( ( .not. done ) .and. ( ip .le. NSIGPR ) )
	         CALL ST_FIND ( prmarr(ip), manwnd(1,ip), NSIGPR, ipos, 
     +                          ierr )
		 IF ( ipos .gt. 0 ) THEN
C
C*                  Parameter valid, check for SKNT
C
                    IF ( ip .eq. 3 ) THEN
			IF ( prmarr(ip) .eq. 'SKNT' ) kntflg = .true.
		    END IF
                  ELSE
		    iret = -1
                    done = .true.
	         END IF
		 ip = ip + 1
              END DO
            ELSE
C
C*            Invalid number of parameters
C
	      iret = -1
	   END IF
C
	 ELSE IF ( ptnam .eq. 'TRPA' .or. ptnam .eq. 'TRPC' ) THEN
C
C*         Tropopause part
C
	   IF ( numprm .eq. NTRPPR ) THEN
	      done = .false.
              ip = 1
C
C*            Check validity of each parameter
C
	      DO WHILE ( ( .not. done ) .and. ( ip .le. NTRPPR ) )
	         CALL ST_FIND ( prmarr(ip), manlst(1,ip), NTRPPR, ipos, 
     +                          ierr )
		 IF ( ipos .gt. 0 ) THEN
C
C*                  Parameter valid, check for DDPC case or SKNT case
C
                    IF ( ip .eq. 3 ) THEN
			IF ( prmarr(ip) .eq. 'DDPC' ) ddpflg = .true.
	              ELSE IF ( ip .eq. 5 ) THEN
                        IF ( prmarr(ip) .eq. 'SKNT' ) kntflg = .true.
		    END IF
                   ELSE
		    iret = -1
                    done = .true.
	         END IF
		 ip = ip + 1
              END DO
            ELSE
C
C*            Invalid number of parameters
C
	      iret = -1
	   END IF
C
	END IF
C*
	RETURN
	END
