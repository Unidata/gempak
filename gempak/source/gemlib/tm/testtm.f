	PROGRAM TESTTM
C************************************************************************
C* TESTTM								*
C*									*
C* This program tests the TERMINAL library subroutines			*
C**									*
C* Log:									*
C* M. Goodman/RDS 	 4/84	Original source code			*
C* M. desJardins/GSFC	 5/84						*
C* I. Graffman/RDS	 3/87	GEMPAK4 update				*
C* S. Schotz/GSC	 5/90	Removed TM_WMSG				*
C* M. desJardins/GSFC	 9/90	Cleaned up				*
C* T. Piper/SAIC	10/04	Removed TM_WAIT (see SS_WAIT)		*
C* T. Piper/SAIC	10/04	Removed TM_PAGE (see SS_PAGE)		*
C************************************************************************
	LOGICAL		pagflg, newlin
	CHARACTER	string*80, chr (80)*10
	INTEGER		int (80)
	REAL		rl (80)
C-------------------------------------------------------------------------
	CALL IN_BDTA ( iret )
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
	    WRITE  (6,20)
20	    FORMAT (
     +     '  1 = TM_ACCP    2 = TM_CHAR    3 = TM_RCHR    4 = TM_STR'/
     +     '  5 = TM_INT     6 = TM_WCR     7 = TM_PROM    8 = TM_REAL')
C
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +			  .false., 1, numsub, n, ier )
	IF  ( ier .eq. 2 )  THEN
           iostat = -1
           numsub = -1
	END IF
30	FORMAT (A)
C------------------------------------------------------------------------
	    IF  ( numsub .eq. 1 )  THEN
		WRITE (6,*) 'TM_ACCP has no input.  It waits for <CR>.'
		CALL TM_ACCP  ( iret )
		WRITE (6,*) 'IRET = ', iret
C-------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 2 )  THEN
		WRITE (6,*) 'Enter MESSG'
		READ  (5,30) string
		WRITE (6,*) 'Enter pagflg, newlin, nexp'
		READ  (5,*)  pagflg, newlin, nexp
		CALL TM_CHAR ( string, pagflg, newlin, nexp, chr, n,
     +                         iret )
		WRITE (6,*) 'NSTR, IRET:', n, iret
		IF  ( n .gt. 0 )  THEN
		    WRITE (6,*) 'CHRSTR: ', ( chr (j), j=1,n )
		END IF
C-------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 3 )  THEN
	        WRITE (6,*) 'Enter STRING for TM_RCHR to read'
		CALL TM_RCHR  ( string, iret )
	        WRITE (6,*) 'IRET, STRING: ', iret, ' ', string
C-------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 4 )  THEN
		WRITE (6,*) 'Enter MESSG'
		READ  (5,30) string
		WRITE (6,*) 'Enter PAGFLG, NEWLIN'
		READ  (5,*)  pagflg, newlin
		CALL TM_STR ( string, pagflg, newlin, string, iret )
		WRITE (6, *) 'IRET, STRING: ', iret, ' ', string
C-------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 5 )  THEN
		WRITE (6,*) 'Enter MESSG: '
		READ  (5,30) string
		WRITE (6,*) 'Enter PAGFLG, NEWLIN, NEXP'
		READ  (5,*)  pagflg, newlin, nexp
		CALL TM_INT ( string, pagflg, newlin, nexp, int, n,
     +                        iret )
		WRITE (6,*) 'IRET, NINT:', iret, n
		IF  ( n .gt. 0 )  THEN
		    WRITE (6,*) 'INTEG: ', ( int (j), j = 1, n )
		END IF
C-------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 6 )  THEN
		CALL TM_WCR  ( iret )
		WRITE  (6,*) 'IRET:', iret
C-------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 7 )  THEN
		WRITE (6,*) 'Enter MESSG'
		READ  (5,30) string
		WRITE (6,*) 'Enter PAGFLG, NEWLIN'
		READ  (5,*)  pagflg, newlin
		CALL TM_PROM  ( string, pagflg, newlin, iret )
		WRITE (6,*)  'IRET:', iret
C-------------------------------------------------------------------------
	     ELSE IF  ( numsub .eq. 8 )  THEN
		WRITE (6,*) 'Enter MESSG'
		READ  (5,30) string
		WRITE (6,*) 'Enter PAGFLG, NEWLIN, NEXP'
		READ  (5,*)  pagflg, newlin, nexp
		CALL TM_REAL (string, pagflg, newlin, nexp, rl, n ,iret)
		WRITE (6,*) 'NREAL, IRET:', n, iret
		IF  ( n .gt. 0 )  THEN
		    WRITE  (6,*) 'RLNOS: ', ( rl (j), j=1,n )
		END IF
C-------------------------------------------------------------------------
	    END IF
	END DO
C*
	END
