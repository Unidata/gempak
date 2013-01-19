	PROGRAM TESTIP
C************************************************************************
C* TESTIP								*
C* This program tests the INPUT PARAMETER library subroutines.		*
C*									*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/84						*
C* G. Huffman/GSC	10/88	Modify no-TAE dump			*
C* K. Tyle/GSC		 7/96	Merged NT-library subroutines into IP-	*
C* S. Jacobs/NCEP	 4/97	Added calls to ST_LCUC; Update & clean	*
C* D.W.Plummer/NCEP	 6/97	Increased string lengths from 72 to 128	*
C* S. Jacobs/NCEP	12/97	Added IP_REST				*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER  	parm*128, pname*8, mapfil*128, device*12
	CHARACTER	filnam*128, string*128, file*128
	LOGICAL		done, respnd, logval, pagflg, lstflg
C------------------------------------------------------------------------
	CALL IN_BDTA ( iret )
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
	    WRITE  ( 6, 20 )
20	    FORMAT ('  1 = IP_INIT  2 = IP_IDNT  3 = IP_DYNM '/
     +              '  4 = IP_EXIT  5 = IP_STR   6 = IP_LOG  '/
     +              '  7 = IP_HELP  8 = IP_LIST  9 = IP_PUTV '/
     +              ' 10 = IP_MFIL 11 = IP_SVAR 12 = IP_RQST '/
     +              ' 13 = IP_VERS 14 = IP_STRP 15 = IP_REST '/
     +              ' 20 = GINITP  21 = GSDEVA '/
     +              ' 30 = DUMP COMMON ' /)
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier )
	    IF ( ier .eq. 2 ) THEN
	       iostat = -1
	       numsub = -1
	    END IF
C------------------------------------------------------------------------
	    IF  ( numsub .eq. 1 )  THEN
		CALL IP_INIT ( respnd, iret )
		WRITE ( 6, * ) 'RESPND, IRET = ', respnd, iret
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 2 )  THEN
	        WRITE (6,*) 'Enter program name'
	        READ (5,2) parm
		CALL IP_IDNT  ( parm, iret )
		WRITE ( 6, * ) 'IRET = ', iret
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 3 )  THEN
		CALL IP_DYNM  ( done, iret )
		WRITE ( 6, * ) 'DONE, IRET = ', done, iret
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 4 )  THEN
		CALL IP_EXIT  ( iret )
		WRITE ( 6, * ) 'IRET = ', iret
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 5 )  THEN
		WRITE  ( 6, * )  'Enter variable name'
		READ   ( 5, 2 )  pname
2		FORMAT ( A )
		CALL IP_STR  ( pname, parm, iret )
		WRITE  ( 6, * ) 'PARM, IRET = ', parm, iret
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 6 )  THEN
		WRITE  ( 6, * )  'Enter PNAME'
		READ   ( 5, 2 )  pname
		CALL IP_LOG  ( pname, logval, iret )
		WRITE  ( 6, * ) 'IRET = ', iret
		WRITE  ( 6, * ) 'LOGVAL = ', logval
C------------------------------------------------------------------------
		ELSE IF (numsub .eq. 7 ) THEN
		WRITE (6,*) 'Enter PNAME'
		READ  (5,2)  pname
		WRITE (6,*) 'Enter PAGFLG'
		READ  (5,*)  pagflg
		CALL ST_LCUC ( pname, pname, ier )
		CALL IP_HELP ( pname, pagflg, iret )
	        WRITE (6,*) 'IRET = ', iret
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 8 ) THEN
		WRITE (6,*) 'Enter PNAME'
		READ  (5,2)  pname
		WRITE (6,*) 'Enter LSTFLG'
		READ  (5,*)  lstflg
		CALL ST_LCUC ( pname, pname, ier )
		CALL IP_LIST ( pname, lstflg, iret )
	        WRITE (6,*) 'IRET = ', iret
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 9 )  THEN
		WRITE  ( 6, * ) 'Enter PNAME'
		READ   ( 5, 2 ) pname
		WRITE  ( 6, * ) 'Enter parameter value'
		READ   ( 5, 2 ) parm
		CALL IP_PUTV  ( pname, parm, iret )
		WRITE  ( 6, * )  'IRET = ', iret
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 10 )  THEN
		CALL IP_MFIL  ( mapfil, iret )
		WRITE  ( 6, * )  'IRET = ', iret
		WRITE  ( 6, * )  'MAPFIL = ', mapfil
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 11 ) THEN
	        WRITE (6,*) ' Enter input string'
	        READ  (5,2)  string
		CALL IP_SVAR  ( string, iret )
	        WRITE (6,*) 'IRET = ', iret
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 12 ) THEN
		CALL IP_RQST  ( iret )
	        WRITE (6,*) 'IRET = ', iret
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 13 ) THEN
		CALL IP_VERS  ( iret )
	        WRITE (6,*) 'IRET = ', iret
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 14 )  THEN
	        WRITE (6,*) 'Enter program name'
	        READ (5,2) parm
		CALL IP_STRP  ( parm, iret )
		WRITE ( 6, * ) 'IRET = ', iret
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 15 )  THEN
	        WRITE (6,*) 'Enter restore file name'
	        READ (5,2) file
		CALL IP_REST  ( file, iret )
		WRITE ( 6, * ) 'IRET = ', iret
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 20 )  THEN
		CALL GINITP  ( 1, istat, ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 21 )  THEN
		device = 'XW'
		iunit  = 1
		filnam = 'GEMPAK'
		itype  = 0
		xsize  = -9999.0
		ysize  = -9999.0
		CALL GSDEVA  ( device, iunit, filnam, itype,
     +				xsize, ysize, ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 30 )  THEN
		WRITE (6,*) ' IPLUN, CPROGM = ', iplun, ' ', cprogm
		WRITE (6,*) ' NCPARM = ', ncparm
		DO  i = 1, ncparm
		    WRITE (6,1000 ) i, cparmn (i), cparmv (i) (1:50),
     +				    iplink (i)
		    WRITE (6,1001)  chelp1 (i)
1001		    FORMAT ( 6X, A )
1000		    FORMAT ( 1X, I3, 2X, A, 2X, A, 2X, I3 )
		END DO
C------------------------------------------------------------------------
	    END IF
	END DO
	END
