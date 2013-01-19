	PROGRAM TESTST
C************************************************************************
C* TESTST								*
C*									*
C* This program tests all of the subroutines in the string library.	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 3/87	GEMPAK4					*
C* M. desJardins/GSFC	 5/89	Cleaned up ST_CRNM, ST_NUMB		*
C* S. Schotz/GSC	 1/90	Added ST_RMST				*
C* K. Brill/NMC		 9/91	Added ST_CKNR; ST_CKNI			*
C* M. desJardins/NMC	 8/92	Fixed input error to ST_ITOC		*
C* S. Jacobs/NMC	 8/94	Added ST_FLST				*
C* K. Tyle/GSC		 2/96	Increased character variable size	*
C* D. Plummer/NCEP	 3/96	Added ST_ILSR				*
C* S. Jacobs/NCEP	 3/96	Added ST_CLS2; ST_PELT			*
C* K. Tyle/GSC 		 7/96	Added ST_LSTC; ST_LSTI; ST_LSTF		*
C* D. Keiser/GSC	 8/96	Added ST_RXBL				*
C* S. Jacobs/NCEP	11/96	Added ST_RMNM				*
C* K. Tyle/GSC          12/96   Added ST_NULL				*
C* D.W.Plummer/NCEP      1/97   Added ST_RPST                           *
C* S. Maxwell/GSC	 4/97	Added ST_CRND				*
C* S. Jacobs/NCEP	 8/98	Added ST_CLSL				*
C* A. Hardy/GSC          8/98   Added ST_ETOA; ST_ATOE                  *
C* A. Hardy/GSC          9/98   Added a neg. number check for ST_ETOA   *
C* D.W.Plummer/NCEP	 3/00	Added ST_GTST				*
C* D. Kidwell/NCEP	 3/00	Added ST_RPSL				*
C* S. Jacobs/NCEP	 3/01	Added ST_SORT				*
C* D. Kidwell/NCEP	10/01	Added ST_WORD				*
C* M. Li/SAIC		02/02	Added ST_ISTP				*
C* T. Lee/SAIC		12/04	Added ST_OPCL				*
C************************************************************************
	CHARACTER	string*160, str1*160, strarr (10)*160, single*1
	CHARACTER	substr*160, str4 (10)*4, filarr (30)*72
        CHARACTER       repstr*160, outstr*160, instr*160, delim*1
	CHARACTER	first*10, last*10, inc*10, four (10)*4
	CHARACTER	inpsrt (20)*160, outsrt (20)*160
        BYTE            barray (60)
	INTEGER 	intg (10) 
	REAL 		rlnum (10)
	LOGICAL 	abbr, istp
C------------------------------------------------------------------------
	CALL IN_BDTA ( iret )
	iostat = 0
	DO WHILE ( iostat .eq. 0 )
	string = ' '
	str1   = ' '
	WRITE (6, 10)
10	FORMAT ( 
     +  '  1 = ST_ABBR    2 = ST_ALNM    3 = ST_C2C     4 = ST_C2I'/
     +  '  5 = ST_C2R     6 = ST_CLST    7 = ST_CRNM    8 = ST_CTOI'/
     +  '  9 = ST_FIND   10 = ST_ILST   11 = ST_INCH   12 = ST_ITOC'/
     +  ' 13 = ST_ITOS   14 = ST_LCUC   15 = ST_LDSP   16 = ST_INLN'/
     +  ' 17 = ST_LSTR   18 = ST_NUMB   19 = ST_RANG   20 = ST_RLCH'/
     +  ' 21 = ST_RLST   22 = ST_RMBL   23 = ST_RNAN   24 = ST_STOI'/
     +  ' 25 = ST_UTAB   26 = ST_UNPR   27 = ST_FWRD   28 = ST_NXTS'/
     +  ' 29 = ST_INTG   30 = ST_RMST   31 = ST_FLST   32 = ST_ILSR'/
     +  ' 33 = ST_CLS2   34 = ST_PELT   35 = ST_LSTC   36 = ST_LSTI'/
     +  ' 37 = ST_LSTF   38 = ST_RXBL   39 = ST_RMNM   40 = ST_RPST'/
     +  ' 41 = ST_NULL   42 = ST_CRND   43 = ST_CLSL   44 = ST_ETOA'/
     +  ' 45 = ST_ATOE   46 = ST_GTST   47 = ST_RPSL   48 = ST_SORT'/
     +  ' 49 = ST_WORD   50 = ST_ISTP   51 = ST_OPCL')
	CALL TM_INT ( 'Select a subroutine number', .false.,
     +		       .false., 1, numsub, n, ier )
	IF ( ier .eq. 2 ) THEN
           iostat = -1
           numsub = -1
	END IF
2	FORMAT (A)
C------------------------------------------------------------------------
	IF (numsub .eq. 1) THEN
	    WRITE (6,*) 'Enter STRING: '
	    READ  (5,2)  string
	    WRITE (6,*) 'Enter STABBR:'
	    READ  (5,2)  str1
	    CALL ST_ABBR  ( string, str1, abbr, iret )
	    WRITE (6,*) 'ABBR, IRET: ', abbr, iret
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 2) THEN
	    WRITE (6,*) 'Enter CHRSTR:'
	    READ  (5,2)  single
	    CALL ST_ALNM  ( single, ityp, iret )
	    WRITE (6,*) 'ITYP, IRET: ', ityp, iret
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 3) THEN
	    WRITE (6,*) 'Enter STRING:'
	    READ  (5,2)  string
	    WRITE (6,*) 'Enter NEXP:'
	    READ  (5,*)  nexp
	    CALL ST_C2C  ( string, nexp, strarr, num, iret )
	    WRITE (6,*) 'IRET, NUM: ', iret, num
	    WRITE (6,*) ( strarr (i), ' ' ,i = 1, num)
	    CALL ER_WMSG  ( 'ST', iret, ' ', ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 4) THEN
	    WRITE (6,*)' Enter number expected'
	    READ (5,*) nexp
	    WRITE (6,*)'Enter', nexp ,'integer strings with separators:'
	    READ (5,2) string
	    CALL ST_C2I (string, nexp, intg, num, iret)
	    WRITE (6,*) 'iret and number ret. ', iret, num
	    IF (num .GT.0) WRITE (6,*) (intg (i),i=1,num)
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 5) THEN
	    WRITE (6,*)' Enter number expected'
	    READ (5,*) nexp
	    WRITE (6,*)'Enter ', nexp ,'real strings with separators:'
	    READ (5, 2) string
	    CALL ST_C2R (string, nexp, rlnum, num, iret)
	    WRITE (6,*) 'iret, number returned ', iret, num
	    IF (num .gt. 0) WRITE (6,*) (rlnum (i),i = 1,num)
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 6) THEN
	    WRITE (6, *)' Enter a string:'
	    READ (5, 2) string
	    WRITE (6, *)' Enter a separator:'
	    READ (5, 2) single
	    WRITE (6, *)' Enter the default value:'
	    READ (5, 2) str1
	    WRITE (6,*)' Enter number expected'
	    READ (5,*) nexp
	    CALL ST_CLST (string, single, str1, nexp, strarr, 
     +	                  num, iret)
	    WRITE (6,*) 'iret, number in list ', iret, num
	    IF (num .gt. 0) WRITE (6,*) (strarr (i),i = 1,nexp)
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 7) THEN
	    WRITE (6,*) 'Enter string:'
	    READ  (5,2)  string
	    CALL ST_CRNM  ( string, value, iret )
	    WRITE (6,*) 'VALUE, IRET: ', value, iret
	    CALL ER_WMSG  ( 'ST', iret, ' ', ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 8) THEN
	    WRITE (6, *)' Enter number of items (max 10):'
	    READ (5, *) nitem
	    WRITE (6, *)' Enter character array'
	    READ (5, 2) (four (i), i = 1, nitem)
	    CALL ST_CTOI (four, nitem, intg, iret)
	    WRITE (6, *) 'iret', iret
	    WRITE (6, *) (intg (i), i = 1, nitem)
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 9) THEN
	    WRITE (6, *)' Enter number in list (max 10):'
	    READ (5, *) num
	    WRITE (6, *)' Enter string array:'
	    READ (5, 2) (strarr (i), i = 1, num)
	    WRITE (6, *) 'Enter string to find:'
	    READ (5, 2) string
	    CALL ST_FIND (string, strarr, num, ipos, iret)
	    WRITE (6, *)' Position = (0 = not found) ', ipos
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 10) THEN
	    WRITE (6, *)' Enter a string:'
	    READ (5, 2) string
	    WRITE (6, *)' Enter a separator:'
	    READ (5, 2) single
	    WRITE (6, *)' Enter the default value:'
	    READ (5, *) idef
	    WRITE (6,*)' Enter number expected'
	    READ (5,*) nexp
	    CALL ST_ILST (string, single, idef, nexp, intg, 
     +	                  num, iret)
	    WRITE (6,*) 'iret, number in list ', iret, num
	    IF (num .gt. 0) WRITE (6,*) (intg (i),i = 1,nexp)
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 11) THEN
	    int=0
	    WRITE (6, *)'Enter an integer:'
	    READ (5, *) int
	    CALL ST_INCH (int, string, iret)
	    WRITE (6,*) 'string, iret ', string, iret
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 12) THEN
	    WRITE (6,*) 'Enter number of packed integers (max 10):'
	    READ  (5,*)  nexp
	    WRITE (6,*) 'Enter ', nexp, ' integers:'
	    READ  (5,*) (intg (i), i = 1, nexp)
	    CALL ST_ITOC  ( intg, nexp, str4, iret )
	    WRITE (6, 125) (str4 (i), i = 1, nexp)
125	    FORMAT (' ', 10(' ', A))
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 13) THEN
	    WRITE (6, *) 'Enter number of integers (max 10):'
	    READ (5,  *) nexp
	    WRITE (6, *)' Enter integer array:'
	    READ (5, *) (intg (i), i = 1, nexp)
	    CALL ST_ITOS (intg, nexp, nc, string, iret)
	    WRITE (6,*) ' iret and num chars: ', iret, nc
	    WRITE (6, *) ' string = ', string (1:nc)
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 14) THEN
	    WRITE (6,*)' Enter a string: '
	    READ (5, 2) string
	    CALL ST_LCUC (string, str1, iret)
	    WRITE (6, *) str1
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 15) THEN
	    WRITE (6,*)'Enter a string'
	    READ (5,2) string
	    CALL ST_LDSP (string, str1, ncout, iret)
	    WRITE (6,*) 'ncout, string ', ncout
	    WRITE (6,*) str1
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 16) THEN
	    WRITE (6,*)'Enter an integer'
	    READ (5,*) int
	    CALL ST_INLN (int, string, lens, iret)
	    WRITE (6,*)' lens, string ', lens, string
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 17) THEN
	    WRITE (6,*) ' Enter a string: '
	    READ (5, 2) string
	    CALL ST_LSTR (string, isiz, iret)
            WRITE (6,*) 'isiz: ',isiz
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 18) THEN
	    WRITE (6,*) 'Enter STRING'
	    READ  (5,2)  string
	    CALL ST_NUMB ( string, number, iret )
	    WRITE (6 ,*) 'computed number, iret ', number, iret
	    CALL ER_WMSG  ( 'ST', iret, ' ', ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 19) THEN
	    WRITE (6, *) 'Enter a string range (use - ):'
	    READ (5, 2) string
	    CALL ST_RANG (string, first, last, inc, ityp, iret)
	    WRITE (6, *) ' first, last, inc, type, iret'
	    WRITE (6, *) first, last, inc, ityp, iret
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 20) THEN
	    WRITE (6,*)' Enter a real number and number of places:'
	    READ (5, *) rl, np
	    CALL ST_RLCH (rl, np, string, iret)
	    WRITE (6, *)' iret, string ', iret, string
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 21) THEN
	    WRITE (6, *)' Enter a string:'
	    READ (5, 2) string
	    WRITE (6, *)' Enter a separator:'
	    READ (5, 2) single
	    WRITE (6, *)' Enter the default value:'
	    READ (5, *) def
	    WRITE (6,*)' Enter number expected'
	    READ (5,*) nexp
	    CALL ST_RLST (string, single, def, nexp, rlnum, 
     +	                  num, iret)
	    WRITE (6,*) 'iret, number in list ', iret, num
	    IF (num .gt. 0) WRITE (6,*) (rlnum (i),i = 1,nexp)
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 22) THEN
	    WRITE (6, *)' Enter a string:'
	    READ (5, 2) string
	    CALL ST_RMBL (string, str1, isiz, iret)
	    WRITE (6, *)' size and returned string: ', isiz
	    WRITE (6,*) str1
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 23) THEN
	    WRITE (6,*) 'Enter a string'
	    READ (5, 2) string
	    CALL ST_RNAN (string, str1, ncout, iret)
	    WRITE (6,*) 'string, nc, iret ', str1, ncout, iret
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 24) THEN
	    WRITE (6, *)' Enter string'
	    READ (5, 2) string
	    WRITE (6,*)' Enter number of characters in string:'
	    READ (5,*) nc
	    CALL ST_STOI (string, nc, nint, intg, iret)
	    WRITE (6, *) 'iret, nint ', iret, nint
	    IF (nint .gt. 0) WRITE (6, *) (intg (i), i = 1, nint)
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 25) THEN
	    WRITE (6, *)' Enter a string:'
	    READ (5,2) string
	    WRITE (6,*) 'Enter number of characters in string'
	    READ (5,*)nc
	    CALL ST_UTAB (string, nc, str1, ier)
	    WRITE (6,*) str1
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 26) THEN
	    WRITE (6,*)' Enter string:'
	    READ (5, 2) string
	    WRITE (6,*) 'Enter length of string'
	    READ (5,*) nn
	    CALL ST_UNPR (string, nn, str1, lenout, iret)
	    WRITE (6,*)' lenout, outstr ', lenout, str1
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 27) THEN
	    WRITE (6, *) 'Enter a string:'
	    READ (5, 2) string
	    WRITE (6,*) 'Enter first, last, word number'
	    READ (5,*) ifirst, ilast, iword
	    CALL ST_FWRD (string, ifirst, ilast, iword, is, ie, iret)
	    WRITE (6,*)' start and end: ', is, ie
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 28) THEN
	    WRITE (6, *) 'Enter a string:'
	    READ (5, 2) string
	    WRITE (6,*) 'Enter first, last'
	    READ (5,*) ifirst, ilast
	    WRITE (6,*) 'Enter number of keywords (max = 10)'
	    READ (5, *) nexp
	    WRITE (6,*)' Enter ', nexp, 'keywords'
	    READ (5, 2) (strarr (i), i = 1, nexp)
	    DO i = 1, nexp
	        CALL ST_LSTR (strarr (i), intg (i), ier)
	    END DO
	    CALL ST_NXTS (string, ifirst, ilast, strarr, intg, nexp, 
     +	                  ipos, istr, iret)
	    WRITE (6, *) ' ipos, istrg ', ipos, istr
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 29) THEN
	    WRITE (6, *)' Enter a string'
	    READ (5, 2) string
	    CALL ST_LSTR  ( string, lens, ier )
	    CALL ST_INTG (string (1:lens), num, iret)
	    WRITE (6,*) ' Integer = ', num
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 30) THEN
	    WRITE (6, *) ' Enter STRING'
	    READ  (5, 2)   string 
            WRITE (6, *) ' Enter SUBSTR'
	    READ  (5, 2)   substr
	    CALL ST_RMST ( string, substr, ipos, string, iret )
	    WRITE (6,*)  'OUTSTR: ',string, ':'
	    WRITE (6,*)  ' IPOS, IRET: ', ipos, iret
	    IF (iret .ne. 0) CALL ER_WMSG ( 'ST', iret, ' ', ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 31) THEN
	    WRITE (6, *)' Enter a string:'
	    READ (5, 2) string
	    WRITE (6, *)' Enter a separator:'
	    READ (5, 2) single
	    WRITE (6, *)' Enter the default value:'
	    READ (5, 2) str1
	    WRITE (6,*)' Enter number expected'
	    READ (5,*) nexp
	    CALL ST_FLST (string, single, str1, nexp, filarr, 
     +	                  num, iret)
	    WRITE (6,*) 'iret, number in list ', iret, num
	    IF (num .gt. 0) WRITE (6,*) (filarr (i),i = 1,nexp)
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 32) THEN
	    WRITE (6, *)' Enter a string:'
	    READ (5, 2) string
	    WRITE (6, *)' Enter a separator:'
	    READ (5, 2) single
	    WRITE (6, *)' Enter the default value:'
	    READ (5, *) idef
	    WRITE (6,*)' Enter number expected'
	    READ (5,*) nexp
	    CALL ST_ILSR (string, single, idef, nexp, intg, 
     +	                  num, iret)
	    WRITE (6,*) 'iret, number in list ', iret, num
	    IF (num .gt. 0) WRITE (6,*) (intg (i),i = 1,nexp)
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 33) THEN
	    WRITE (6, *)' Enter a string:'
	    READ (5, 2) string
	    WRITE (6, *)' Enter a separator:'
	    READ (5, 2) single
	    WRITE (6, *)' Enter the default value:'
	    READ (5, 2) str1
	    WRITE (6,*)' Enter number expected'
	    READ (5,*) nexp
	    CALL ST_CLS2 (string, single, str1, nexp, strarr, 
     +	                  num, iret)
	    WRITE (6,*) 'iret, number in list ', iret, num
	    IF (num .gt. 0) WRITE (6,*) (strarr (i),i = 1,nexp)
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 34) THEN
	    WRITE (6, *)' Enter a string:'
	    READ (5, 2) string
	    WRITE (6,*)' Enter item number:'
	    READ (5,*) nelmt
	    WRITE (6, *)' Enter the default value:'
	    READ (5, 2) str1
	    CALL ST_PELT (string, nelmt, str1, num, substr, 
     +	                  iret)
	    WRITE (6,*) 'iret, number in list ', iret, num
	    WRITE (6,*) substr
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
	  ELSE IF ( numsub .eq. 35 ) THEN
	    WRITE (6, *) 'Enter number of elements in the array:'
	    READ (5, *) num
	    DO I = 1, num
	      WRITE (6, *) 'Enter string #',i,':'
	      READ (5, 2) strarr ( i )
	    END DO
	    WRITE (6, *) 'Enter a separator:'
	    READ (5, 2) single
	    CALL ST_LSTC ( strarr, num, single, string, iret )
	    WRITE ( 6, * ) 'String: ', string
	    WRITE ( 6, * ) 'iret = ', iret
	    IF ( iret .ne. 0 ) CALL ER_WMSG  ( 'ST', iret, ' ', ier )
C------------------------------------------------------------------------
	  ELSE IF ( numsub .eq. 36 ) THEN
	    WRITE (6, *) 'Enter number of elements in the array:'
	    READ (5, *) num
	    DO I = 1, num
	      WRITE (6, *) 'Enter integer #',i,':'
	      READ (5, *) intg ( i )
	    END DO
	    WRITE (6, *) 'Enter a separator:'
	    READ (5, 2) single
	    CALL ST_LSTI ( intg, num, single, string, iret )
	    WRITE ( 6, * ) 'String: ', string
	    WRITE ( 6, * ) 'iret = ', iret
	    IF ( iret .ne. 0 ) CALL ER_WMSG  ( 'ST', iret, ' ', ier )
C------------------------------------------------------------------------
	  ELSE IF ( numsub .eq. 37 ) THEN
	    WRITE (6, *) 'Enter number of elements in the array:'
	    READ (5, *) num
	    DO I = 1, num
	      WRITE (6, *) 'Enter real #',i,':'
	      READ (5, *) rlnum ( i )
	    END DO
	    WRITE (6, *) 'Enter a separator:'
	    READ (5, 2) single
	    WRITE (6, *) 'Enter number of decimal places to include:'
	    READ (5, *) np
	    CALL ST_LSTF ( rlnum, num, single, np, string, iret )
	    WRITE ( 6, * ) 'String: ', string
	    WRITE ( 6, * ) 'iret = ', iret
	    IF ( iret .ne. 0 ) CALL ER_WMSG  ( 'ST', iret, ' ', ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 38) THEN
	    WRITE (6, *)' Enter a string:'
	    READ (5, 2) string
	    CALL ST_RXBL (string, str1, isiz, iret)
	    WRITE (6, *)' size and returned string: ', isiz
	    WRITE (6,*) str1
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 39) THEN
	    WRITE (6,*) 'Enter a string'
	    READ (5, 2) string
	    CALL ST_RMNM (string, str1, nncr, ncout, iret)
	    WRITE (6,*) 'string, nncr, nc, iret: ',
     +			str1, nncr, ncout, iret
C------------------------------------------------------------------------
          ELSE IF (numsub .eq. 40) THEN
            WRITE (6, *) ' Enter STRING'
            READ  (5, 2)   string
            WRITE (6, *) ' Enter SUBSTR'
            READ  (5, 2)   substr
            WRITE (6, *) ' Enter REPSTR'
            READ  (5, 2)   repstr
            CALL ST_RPST ( string, substr, repstr, ipos, string, iret )
            WRITE (6,*)  'OUTSTR: ',string
            WRITE (6,*)  ' IPOS, IRET: ', ipos, iret
            IF (iret .ne. 0) CALL ER_WMSG ( 'ST', iret, ' ', ier )
C------------------------------------------------------------------------
          ELSE IF (numsub .eq. 41) THEN
            WRITE (6,*) 'Enter a string'
            READ (5, 2) string
            CALL ST_NULL (string, str1, isiz, iret)
            WRITE (6, *)' size and returned string: ', isiz
            WRITE (6,*) str1
C------------------------------------------------------------------------
          ELSE IF (numsub .eq. 42) THEN
            WRITE (6,*) 'Enter string:'
            READ  (5,2)  string
            CALL ST_CRND  ( string, value, ndec, iret )
            WRITE (6,*) 'VALUE, NDEC, IRET: ', value, ndec, iret
            CALL ER_WMSG  ( 'ST', iret, ' ', ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 43) THEN
	    WRITE (6, *)' Enter a string:'
	    READ (5, 2) string
	    WRITE (6, *)' Enter a separator:'
	    READ (5, 2) single
	    WRITE (6, *)' Enter the default value:'
	    READ (5, 2) str1
	    WRITE (6,*)' Enter number expected'
	    READ (5,*) nexp
	    CALL ST_CLSL (string, single, str1, nexp, strarr, 
     +	                  num, iret)
	    WRITE (6,*) 'iret, number in list ', iret, num
	    IF (num .gt. 0) WRITE (6,*) (strarr (i),i = 1,nexp)
	    IF (iret .ne. 0) CALL ER_WMSG ('ST', iret, ' ', ier)
C------------------------------------------------------------------------
          ELSE IF (numsub .eq. 44) THEN
	    WRITE (6, *) 'Enter the number of elements in the array:'
	    READ (5, *) num
	    DO I = 1, num
	        WRITE (6, *)' Enter EBCDIC code # ',i,':'
	        READ (5, *) barray(I)
	    END DO
            CALL ST_ETOA ( barray, num, outstr, iret )
            IF (num .gt. 0)  WRITE(6,*)'ASCII string:  ',outstr(1:num)
            IF ( num .lt. 0 ) WRITE(6,*)'Invalid number of elements.' 
C------------------------------------------------------------------------
          ELSE IF (numsub .eq. 45) THEN
	    WRITE (6, *)' Enter the ASCII characters.'
	    READ (5, 2) instr
            CALL ST_LSTR ( instr, num, iret )
            CALL ST_ATOE ( instr, num, barray, iret )
            write(6,*)'NUM ', num
            write(6,*)'BYTE values: ', (barray(i), i = 1, num )
C------------------------------------------------------------------------
          ELSE IF (numsub .eq. 46) THEN
            WRITE (6, *) ' Enter STRING (template)'
            READ  (5, 2)   string
            WRITE (6, *) ' Enter SUBSTR'
            READ  (5, 2)   substr
            WRITE (6, *) ' Enter DELIMITER'
            READ  (5, 2)   delim
            WRITE (6, *) ' Enter STRING (extract)'
            READ  (5, 2)   repstr
            CALL ST_GTST ( string, substr, delim, repstr, 
     +			   string, len, iret )
            WRITE (6,*)  'OUTSTR: ',string
            WRITE (6,*)  'LENGTH: ',len
            WRITE (6,*)  ' IRET: ', iret
            IF (iret .ne. 0) CALL ER_WMSG ( 'ST', iret, ' ', ier )
C------------------------------------------------------------------------
          ELSE IF (numsub .eq. 47) THEN
            WRITE (6, *) ' Enter STRING'
            READ  (5, 2)   string
            WRITE (6, *) ' Enter SUBSTR'
            READ  (5, 2)   substr
            WRITE (6, *) ' Enter LENSUB'
            READ  (5, *)   lensub
            WRITE (6, *) ' Enter REPSTR'
            READ  (5, 2)   repstr
            WRITE (6, *) ' Enter LENREP'
            READ  (5, *)   lenrep
            CALL ST_RPSL ( string, substr, lensub, repstr, lenrep, 
     +			   ipos, string, iret )
            WRITE (6,*)  'OUTSTR: ',string
            WRITE (6,*)  ' IPOS, IRET: ', ipos, iret
            IF (iret .ne. 0) CALL ER_WMSG ( 'ST', iret, ' ', ier )
C------------------------------------------------------------------------
          ELSE IF (numsub .eq. 48) THEN
            WRITE (6, *) ' Enter the type of sort'
            READ  (5, *)   itype
            WRITE (6, *) ' Enter number of input strings (<=20)'
            READ  (5, *)   nstr
	    DO  i = 1, nstr
		WRITE (6, *) ' Enter STRING', i
		READ  (5, 2)   inpsrt(i)
	    END DO
	    CALL ST_SORT ( itype, nstr, inpsrt, nout, outsrt, iret )
            WRITE (6,*)  ' IRET: ', iret
            IF (iret .ne. 0) CALL ER_WMSG ( 'ST', iret, ' ', ier )
	    WRITE (6,*)  ' Sorted array:'
	    DO  i = 1, nout
	    	WRITE (6,*) outsrt(i)
	    END DO
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 49) THEN
	    WRITE (6,*) 'Enter CHRSTR:'
	    READ  (5,2)  string
	    CALL ST_WORD  ( string, ityp, iret )
	    WRITE (6,*) 'ITYP, IRET: ', ityp, iret
C------------------------------------------------------------------------
          ELSE IF (numsub .eq. 50) THEN
            WRITE (6,*) 'Enter STRING: '
            READ  (5,2)  string
            CALL ST_ISTP  ( string, istp, iret )
            WRITE (6,*) 'ISTP, IRET: ', istp, iret
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 51) THEN
	    WRITE (6, *) ' Enter INSTR '
            READ  (5, 2)   instr
            WRITE (6, *) ' Enter LOCOPN '
            READ  (5, *)   locopn
            CALL ST_OPCL ( instr, locopn, outstr, loccls, iret )
            WRITE (6,*)  'OUTSTR: ', outstr
            WRITE (6,*)  'LOCCLS: ', loccls
            WRITE (6,*)  ' IRET: ', iret
            IF (iret .ne. 0) CALL ER_WMSG ( 'ST', iret, ' ', ier )
	END IF
	END DO
C*
	END
