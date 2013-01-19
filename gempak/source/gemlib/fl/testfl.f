	PROGRAM TESTFL
C************************************************************************
C* TESTFL								*
C*									*
C* This program tests the FILE library subroutines.			*
C*									*
C**									*
C* Log:									*
C* I. Graffman/RDS	 6/84						*
C* I. Graffman/RDS	 9/88						*
C* G. Krueger/EAI	 3/94  	Changed length of char to 132		*
C* M. desJardins/NMC	 8/94  	Cleanup					*
C* S. Jacobs/NMC	 8/94  	Removed FL_PERR				*
C* D. Keiser/GSC	12/95  	Added FL_TBOP				*
C* K. Tyle/GSC		 1/96  	Changed iostat to ios in FL_IRET test	*
C* S. Jacobs/NCEP	 7/96  	Removed FL_TOPN				*
C* G. Krueger/EAI	 8/96  	Added FL_MDAT; Cleaned-up output	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL				*
C* D.W.Plummer/NCEP	11/96	Added FL_MFLS				*
C* S. Maxwell/GSC	12/96	Removed FL_IRET				*
C* D.W.Plummer/NCEP	 1/98	Added FL_TMPL				*
C* D.W.Plummer/NCEP	 1/98	Added FL_MNMR				*
C* S. Jacobs/NCEP	 8/98	Added FL_SCND, FL_FFIL			*
C* D.W.Plummer/NCEP	 9/98	Added maxnms to option #25		*
C* T. Lee/GSC		 4/99	Removed FL_FFIL				*
C* T. Lee/GSC		 7/99	Added CYCLE to calling seq of FL_MFLS	*
C* S. Jacobs/NCEP	 8/99	Added sorting order to call for FL_SCND	*
C* S. Jacobs/NCEP	 8/99	Changed format of data.tbl (FL_TMPL)	*
C* S. Jacobs/NCEP	 9/99	Changed call to FL_MDAT			*
C* S. Jacobs/NCEP	11/99	Added FL_PATH				*
C* M. Li/GSC		 5/00	Changed length to filnms		*
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* S. Jacobs/NCEP	 5/01	Added FL_TINQ				*
C* A. Hardy/SAIC         2/02   Added nexp to FL_SCND call sequence	*
C* S. Jacobs/NCEP	 7/03	Added FL_FPTH				*
C* T. Lee/SAIC		 9/04	Removed FL_TMPL				*
C************************************************************************
C
	INCLUDE         'GEMPRM.PRM'
	PARAMETER	( MAXDIR = 132 * 50 )
C
	CHARACTER 	filnam*132, newfil*132, dattim*24, templt*72
	CHARACTER	type*72, file*132, filtyp*72, path*64, tmplt*48
	CHARACTER	cycle*20, defdat*24
	INTEGER		iarr (1000)
	LOGICAL 	wrtflg, exist
	CHARACTER*(MXFLSZ)	filnms ( MXNMFL )
	CHARACTER	fulnam*132, dirnam*132, basnam*132
	CHARACTER	dirpth*(MAXDIR)
C------------------------------------------------------------------------
	CALL IN_BDTA  ( ier )
	iostat = 0
	DO WHILE ( iostat .eq. 0 )
	    WRITE (6,20)
   20	    FORMAT ('  1 = FL_DCRE    2 = FL_DOPN      3 = FL_DSOP '/
     +              '  4 = FL_CLOS    5 = FL_CDEL      6 = FL_READ '/
     +              '  7 = FL_RSHR    8 = FL_WRIT      9 = FL_PATH '/
     +              ' 10 = FL_TREW   11 = FL_SOPN     12 = FL_SWOP '/
     +              ' 13 = FL_BKSP   14 = FL_REWD     15 = FL_TINQ '/
     +              ' 16 = FL_MNAM   17 = FL_GLUN     18 = FL_FLUN '/
     +              ' 19 = FL_GNAM   20 = FL_INQR     21 = FL_CLAL '/
     +              ' 22 = FL_TBOP   23 = FL_MDAT     24 = FL_MFIL '/
     +              ' 25 = FL_MFLS   27 = FL_SCND     28 = FL_FPTH'  )
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier )
	IF ( ier .eq. 2 ) THEN
	   iostat = -1
           numsub = -1
	END IF
C------------------------------------------------------------------------
	    IF (numsub .EQ. 1) THEN
		WRITE(6,*)' Enter file name'
		READ (5, 5000) filnam
	        WRITE (6,*)' Enter record size '
	        READ (5, *) irecsz
		CALL FL_DCRE ( filnam, irecsz, lun, iret )		
		WRITE (6,*) 'lun, iret', lun, iret
	        CALL ER_WMSG ( 'FL',  iret, filnam, ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 2) THEN
		WRITE(6,*) 'Enter file name'
		READ (5, 5000) filnam
	        WRITE (6,*) 'Enter WRTFLG'
		READ  (5,*)  wrtflg
	        WRITE (6,*) 'Enter IRCSIZ'
	        READ  (5,*)  ircsiz
		CALL FL_DOPN ( filnam, ircsiz, wrtflg, lun, iret )
		WRITE (6,*) 'LUN, IRET ', lun, iret
		CALL ER_WMSG ( 'FL', iret, filnam, ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 3) THEN
		WRITE(6,*)' Enter file name'
		READ (5, 5000) filnam
	        WRITE (6,*)' Enter record size (words): '
	        READ (5,*) ircsz
		CALL FL_DSOP ( filnam, ircsz, lun, iret )		
		WRITE (6,*) 'lun, iret', lun, iret
		CALL ER_WMSG ( 'FL', iret, filnam, ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 4) THEN
		WRITE (6,*) 'Enter LUN'
		READ (5,*) lun
		CALL FL_CLOS ( lun, iret )			
		WRITE (6,*) 'iret = ', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 5) THEN
		WRITE (6,*) 'Enter LUN'
		READ (5,*) lun
		CALL FL_CDEL ( lun, iret )			
		WRITE (6,*) 'iret = ', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 6) THEN
	        WRITE (6,*)' Enter lun, rec no, len in words'
	        READ (5,*) lun, irec, len
	        CALL FL_READ (lun, irec, len, iarr, iret)
	        WRITE (6,*) 'iarray', (iarr (i), i=1, len)
	        WRITE (6,*) 'iret', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 7) THEN
	        WRITE (6,*)' Enter lun, rec no, len in words'
	        READ (5,*) lun, irec, len
	        CALL FL_RSHR (lun, irec, len, iarr, iret)
	        WRITE (6,*) 'iarray', (iarr (i), i=1, len)
	        WRITE (6,*) 'iret', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 8) THEN
	        WRITE (6,*)' Enter lun, rec no, len in words'
	        READ (5,*) lun, irec, len
	        WRITE (6,*)' Enter ', len, ' data values'
	        READ (5,*) (iarr (i), i=1, len)
	        CALL FL_WRIT (lun, irec, len, iarr, iret)
	        WRITE (6,*) 'iret', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 9) THEN
	        WRITE (6,*)' Enter a full file path'
		READ (5,5000) fulnam
		CALL FL_PATH ( fulnam, dirnam, basnam, iret )
		CALL ST_LSTR ( dirnam, lend, ier )
		WRITE (6,*) 'dirnam = ', dirnam(:lend)
		CALL ST_LSTR ( basnam, lenb, ier )
		WRITE (6,*) 'basnam = ', basnam(:lenb)
	        WRITE (6,*) 'iret', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 10) THEN
		WRITE (6,*) 'Enter lun'
		READ (5,*) lun
		CALL FL_TREW ( lun, iret )
		WRITE (6,*) 'iret = ', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 11) THEN
		WRITE(6,*)' Enter file name'
		READ (5, 5000) filnam
		CALL FL_SOPN ( filnam, lun, iret )			
		WRITE (6,*) 'lun, iret', lun, iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, 
     +	                                        filnam, ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 12) THEN
		WRITE(6,*)' Enter file name'
		READ (5, 5000) filnam
		CALL FL_SWOP ( filnam, lun, iret )			
		WRITE (6,*) 'lun, iret', lun, iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, 
     +	                                        filnam, ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 13) THEN
		WRITE (6,*) 'Enter lun'
		READ (5,*) lun
		CALL FL_BKSP ( lun, iret )
		WRITE (6,*) 'iret = ', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 14) THEN
		WRITE (6,*) 'Enter lun'
		READ (5,*) lun
		CALL FL_REWD ( lun, iret )
		WRITE (6,*) 'iret', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 15) THEN
		WRITE(6,*)' Enter file name'
		READ (5, 5000) filnam
		WRITE (6,*)' Enter the file type'
		READ (5, 5000) type
		CALL FL_TINQ ( filnam, type, exist, newfil, iret )
		WRITE (6,*) 'IRET = ', iret, '   EXIST = ', exist
		WRITE (6,*) 'NEWFIL = ', newfil
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 16) THEN
		WRITE(6,*)' Enter a full date string'
		READ (5, 5000) dattim
		WRITE(6,*)' Enter a file name template'
		READ (5, 5000) templt
		CALL FL_MNAM ( dattim, templt, filnam, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ST_LSTR ( filnam, lf, ier )
		WRITE (6,*) 'filnam = ', filnam(:lf)
		CALL ER_WMSG ( 'FL', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 17) THEN
		CALL FL_GLUN ( lun, iret )
		WRITE (6,*) 'iret, lun', iret, lun
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 18) THEN
		WRITE (6,*) 'Enter lun'
		READ (5,*) lun
		CALL FL_FLUN ( lun, iret )
		WRITE (6,*) 'iret = ', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 19) THEN
	        WRITE (6,*)' Enter LUN '
	        READ (5,*) lun
		CALL FL_GNAM (lun, filnam, iret )			
		WRITE (6,*) ' filnam = ', filnam
		WRITE (6,*) ' iret = ', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'FL', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 20) THEN
		WRITE (6,*)' Enter file name '
		READ (5, 5000) filnam
		CALL FL_INQR ( filnam, exist, newfil, iret )
		WRITE (6,*) 'IRET = ', iret, '   EXIST = ', exist
		WRITE (6,*) 'NEWFIL = ', newfil
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 21) THEN
		CALL FL_CLAL  ( iret )
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'FL', iret, filnam, ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 22) THEN
		WRITE(6,*)' Enter file name'
		READ (5, 5000) filnam
		WRITE (6,*)' Enter the file type'
		READ (5, 5000) type
		CALL FL_TBOP ( filnam, type, lun, iret )		
		WRITE (6,*) 'lun, iret ', lun, iret
		IF (iret .ne. 0) CALL ER_WMSG ('FL', iret, filnam, ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 23) THEN
		WRITE(6,*)' Enter a file path'
		READ (5, 5000) filnam
		WRITE(6,*)' Enter a file name template'
		READ (5, 5000) templt
		WRITE(6,*)' Enter a default full GEMPAK date/time'
		READ (5, 5000) defdat
		CALL FL_MDAT ( filnam, templt, defdat, dattim, iret )
		WRITE (6,*) 'dattim, iret ', dattim, iret
		CALL ER_WMSG ( 'FL', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 24) THEN
		WRITE(6,*)' Enter a file type'
		READ (5, 5000) filtyp
		WRITE(6,*)' Enter a dattim (full GEMPAK time or blank) '
		READ (5, 5000) dattim
		CALL FL_MFIL ( filtyp, dattim, file, iret )
		WRITE (6,*) 'file, iret ', file, iret
		CALL ER_WMSG ( 'FL', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 25) THEN
		WRITE(6,*)' Enter a file type'
		READ (5, 5000) filtyp
		WRITE(6,*)' Enter a dattim (full or part GEMPAK time)'
		READ (5, 5000) dattim
		WRITE(6,*)' Enter cycle (* or blank returns all cycles)'
		READ (5, 5000) cycle
		WRITE(6,*)' Enter the maximum number of file names',
     +			  ' to return '
		READ (5, *) maxnms
		maxnms = MIN ( maxnms, LLMXTM )
		CALL FL_MFLS ( filtyp, dattim, cycle, maxnms,
     +			       filnms, nfiles, tmplt, iret )
		WRITE (6,*) 'nfiles, iret ', nfiles, iret
		DO  i = 1, nfiles
		    CALL ST_LSTR ( filnms(i), len, iret )
		    WRITE (6,*)  filnms(i)(:len)
		END DO
		IF ( tmplt .ne. ' ' )
     +		    WRITE (6,*) 'template = ', tmplt
		CALL ER_WMSG ( 'FL', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 27) THEN
		WRITE(6,*) ' Enter a directory path'
		READ (5, 5000) path
		WRITE(6,*) ' Enter a file name template'
		READ (5, 5000) templt
		WRITE(6,*) ' Enter sort order: 1=Alpha,-1=Rvrs Alpha):'
		READ (5, *) isort
		WRITE(6,*) ' Enter the number of expected returned ' //
     +                     'files' 
		READ (5, *) nexp
		CALL FL_SCND ( path, templt, isort, nexp, filnms, 
     +			       nfile, iret )
		WRITE (6,*) 'nfile, iret ', nfile, iret
		DO  i = 1, nfile
		    CALL ST_LSTR ( filnms(i), lenf, iret )
		    WRITE (6,*)  filnms(i)(:lenf)
		END DO
C
		CALL ER_WMSG ( 'FL', iret, path, ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .EQ. 28) THEN
		WRITE(6,*) ' Enter the directory search path'
		READ (5, 5000) dirpth
		WRITE(6,*) ' Enter a file name to find'
		READ (5, 5000) filnam
		CALL FL_FPTH ( dirpth, filnam, fulnam, iret )
		WRITE (6,*) 'Output file name = ', fulnam
		WRITE (6,*) 'iret = ', iret
	        CALL ER_WMSG ( 'FL',  iret, dirpth, ierr)
C------------------------------------------------------------------------
	    END IF
	END DO
C
 5000	FORMAT (A)
	END
