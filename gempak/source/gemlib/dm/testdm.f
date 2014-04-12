	PROGRAM TESTDM
C************************************************************************
C* PROGRAM TESTDM							*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/87						*
C* K. Brill/NMC		 8/93	Add DM_LSSF for surface file keys	*
C*				DM_LSTN does only sounding file keys	*
C* T. Lee/GSC		10/97	Added SWFO and WFO2 in DM_LSSF calling	*
C*				sequences				*
C* S. Jacobs/NCEP	 8/13	Added printing part information in 	*
C*				DMPART common variable section		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
	INCLUDE		'GMBDTA.CMN'
	INTEGER		IHEADR (MMKEY), IDTHDR (100)
	CHARACTER	FILNAM*80, STRING*600, INPUT*20, COMMN*20
	LOGICAL		WRTFLG, ADDFLG,SFLAG,DATFLG, SHRFLG
	INTEGER		KEYLOC (MMKEY), KEYVAL (MMKEY)
	CHARACTER*4	KEYROW (MMKEY), KEYCOL (MMKEY), PRTNAM (MMPRT),
     +			FHDNAM (MMFHDR), PART
	INTEGER		IFHLEN (MMFHDR), IFHTYP (MMFHDR), 
     +			LENHDR (MMPRT), ITYPRT (MMPRT), NPARMS (MMPRT)
	PARAMETER	(M = MMPARM * MMPRT )
	CHARACTER*4	PRMNAM (M)
	INTEGER		ISCALE (M), IOFFST (M), NBITS (M)
C
	PARAMETER	( MMKEY3 = 3 * MMKEY )
	CHARACTER*4	CHARRY (20), TYPE, KEYNAM
	CHARACTER*15	TIMLST (1000)
	INTEGER		ILOVAL (MMKEY3), IHIVAL (MMKEY3)
	INTEGER		IARRAY (20), IDATA (100000)
	REAL		DATA (100000)
	EQUIVALENCE	(DATA, IDATA)
C------------------------------------------------------------------------
	CALL IN_BDTA  ( IER )
C
	DO I = 1, 100
	    IDTHDR (I) = I
	END DO
	iostat = 0
	DO WHILE ( iostat .eq. 0 )
C*
	    CALL TM_CHAR ( 'ENTER SUBROUTINE NAME', .false.,
     +                  .false., 1, input, n, ier )
	    IF ( ier .eq. 2 ) THEN
               input  = ' '
               iostat = -1
	    END IF	     
	    CALL ST_LCUC ( INPUT, INPUT, IER )
	    IF  ( INPUT (1:4) .EQ. 'DUMP' ) THEN
		COMMN = INPUT (6:)
		INPUT = 'DUMP'
	    END IF
C*
	    IF  ( INPUT .EQ. 'HELP' ) THEN
		WRITE (6,1001)
1001		FORMAT ( ' Subroutine names: '/
     +          '  DM_CRET    DM_OPEN    DM_CLOS    DM_LSTN    DM_LTIM'/
     +          '  DM_GTIM    DM_WRWH    DM_WCLH    DM_RRWH    DM_RCLH'/
     +          '  DM_WFHI    DM_WFHC    DM_WFHR    DM_RFHI    DM_RFHC'/
     +          '  DM_RFHR    DM_WDTI    DM_WDTC    DM_WDTR    DM_RDTI'/
     +          '  DM_RDTC    DM_RDTR    DM_KEYS    DM_FKEY    DM_PNAM'/
     +          '  DM_PART    DM_BEGS    DM_PSRC    DM_CSRC    DM_DALL'/
     +          '  DM_DDAT    DM_DPSR    DM_DCSR    DM_NEXT    DM_COND'/
     +          '  DM_WORD    DM_GSFC    DM_FSPC    DM_AFRE    DM_EFRE'/
     +          '  ST_CTOI    ST_ITOC    DM_RINT    DM_WINT    DM_RFLT'/
     +          '  DM_WFLT    DM_RCH4    DM_WCH4    DM_RSTR    DM_WSTR'/
     +          '  DM_FWRT    DM_QDAT    DM_SRCH    DM_CLOP    DM_DRWH'/
     +          '  DM_DCLH    DM_LSSF    DUMP' )
C
	      ELSE IF ( INPUT .EQ. 'DM_CRET' ) THEN
		WRITE (6,*) 'ENTER FILE NAME'
		READ  (5,2) FILNAM
2		FORMAT (A)
		WRITE (6,*) 'ENTER IFTYPE,IFSRCE,MAXPRM'
		READ  (5,*) IFTYPE,IFSRCE,MAXPRM
		WRITE (6,*) 'ENTER NROW,NCOL,NPRT'
		READ  (5,*) NROW,NCOL,NPRT
		WRITE (6,*) 'ENTER NRKEYS,NCKEYS,NFHDRS'
		READ  (5,*) NRKEYS,NCKEYS,NFHDRS
		WRITE (6,*) 'ENTER ROW KEYS -SEPARATE LINES'
		READ  (5,2) (KEYROW(I),I=1,NRKEYS)
		WRITE (6,*) 'ENTER COL KEYS - SEPARATE LINES'
		READ  (5,2) (KEYCOL(I),I=1,NCKEYS)
		WRITE (6,*) 'ENTER FILE HDR NAMES - SEPARATE LINES'
		READ  (5,2) (FHDNAM(I),I=1,NFHDRS)
		WRITE (6,*) 'ENTER IFHLEN, THEN IFHTYP'
		READ  (5,*) (IFHLEN(I),I=1,NFHDRS),
     +				(IFHTYP(I),I=1,NFHDRS)
		DO  I = 1, NPRT
		    WRITE (6,*) 'PART ',I,'  ENTER PART NAME'
		    READ  (5,2) PRTNAM (I)
		    WRITE (6,*) 'ENTER LENHDR, ITYPRT, NPARMS'
		    READ  (5,*) LENHDR (I), ITYPRT (I), NPARMS (I)
		    II = ( I - 1 ) * MAXPRM
		    DO  J = 1, NPARMS (I)
			WRITE (6,*) 'PARMS ',J,' ENTER PARM NAME'
			READ  (5,2) PRMNAM (II+J)
			WRITE (6,*) 'ENTER ISCALE,IOFFST,NBITS'
			READ  (5,*) ISCALE(II+J),IOFFST(II+J),NBITS(II+J)
		    END DO
		END DO
		CALL DM_CRET (FILNAM,IFTYPE,IFSRCE,NFHDRS,FHDNAM,
     +			IFHLEN,IFHTYP,NROW,NRKEYS,KEYROW,NCOL,NCKEYS,
     +			KEYCOL,NPRT,PRTNAM,LENHDR,ITYPRT,NPARMS,MAXPRM,
     +			PRMNAM,ISCALE,IOFFST,NBITS,IFLNO,IRET)
		WRITE (6,*) 'IFLNO,IRET: ',IFLNO,IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,FILNAM,IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_OPEN' ) THEN
		WRITE (6,*) 'ENTER FILE NAME'
		READ  (5,2) FILNAM
		WRITE (6,*) 'ENTER WRTFLG'
		READ  (5,*) WRTFLG
		WRITE (6,*) 'ENTER SHRFLG'
		READ  (5,*) SHRFLG
		CALL DM_OPEN ( FILNAM,WRTFLG,SHRFLG,IFLNO,IFTYPE,
     +	                       IFSRCE,NROW,NCOL,NPRT,NFHDRS,IRET)
		WRITE (6,*) 'IFLNO,IFTYPE,IFSRCE,NROW,NCOL,NPRT,',
     +				'NFHDRS,IRET',IFLNO,IFTYPE,IFSRCE,
     +				NROW,NCOL,NPRT,NFHDRS,IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,FILNAM,IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_CLOS' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		CALL DM_CLOS (IFLNO, IRET )
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_LSTN' ) THEN
		CALL DM_LSTN ( IFLNO, TYPE, ILSTID, ILSTNM, ILSLAT,
     +				ILSLON, ILSELV, ILSTAT, ILCOUN,
     +				ILSTD2, IRET )
		WRITE (6,*)'STTYPE,ILSTID,ILSTNM,ILSLAT,ILSLON,ILSELV,',
     +				'ILSTAT,ILCOUN,ILSTD2,IRET'
		WRITE (6,*) TYPE,ILSTID,ILSTNM,ILSLAT,ILSLON,ILSELV,
     +				ILSTAT,ILCOUN,ILSTD2,IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_LSSF' ) THEN
		CALL DM_LSSF ( IFLNO, TYPE, ILSTID, ILSTNM, ILSLAT,
     +				ILSLON, ILSELV, ILSTAT, ILCOUN,
     +				ILSTD2, ILSPRI, ILSWFO, ILWFO2, IRET )
		WRITE (6,*)'STTYPE,ILSTID,ILSTNM,ILSLAT,ILSLON,ILSELV,',
     +				'ILSTAT,ILCOUN,ILSTD2,ILSPRI,ILSWFO,',
     +				'ILWFO2,IRET' 
		WRITE (6,*) TYPE,ILSTID,ILSTNM,ILSLAT,ILSLON,ILSELV,
     +				ILSTAT,ILCOUN,ILSTD2,ILSPRI,ILSWFO,
     +				ILWFO2,IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_LTIM' ) THEN
		CALL DM_LTIM ( IFLNO, TYPE, ILDATE, ILTIME, IRET )
		WRITE (6,*) 'DTTYPE,ILDATE,ILTIME,IRET'
		WRITE (6,*)    TYPE,ILDATE,ILTIME,IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_GTIM' ) THEN
		WRITE (6,*) 'ENTER IFLNO, MAXTIM'
		READ  (5,*) IFLNO, MAXTIM
		CALL DM_GTIM ( IFLNO,MAXTIM,NTIME,TIMLST,IRET)
		WRITE (6,*) 'NTIME, IRET: ',NTIME,IRET
		WRITE (6,*) (TIMLST(I),'    ',I=1,NTIME)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WRWH' ) THEN
		WRITE (6,*) 'ENTER IFLNO, IPOS'
		READ  (5,*) IFLNO,IPOS
		WRITE (6,*) 'ENTER',KRKEYS(IFLNO),' HEADER VALUES'
		READ  (5,*) (IHEADR (I),I=1,KRKEYS(IFLNO))
		CALL DM_WRWH (IFLNO,IPOS,IHEADR,JPOS,IRET)
		WRITE (6,*) 'JPOS = ', JPOS, '  IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WCLH' ) THEN
		WRITE (6,*) 'ENTER IFLNO, IPOS'
		READ  (5,*) IFLNO,IPOS
		WRITE (6,*) 'ENTER',KCKEYS(IFLNO),' HEADER VALUES'
		READ  (5,*) (IHEADR (I),I=1,KCKEYS(IFLNO))
		CALL DM_WCLH (IFLNO,IPOS,IHEADR,JPOS,IRET)
		WRITE (6,*) 'JPOS = ', JPOS, '  IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_RRWH'  ) THEN
		WRITE (6,*) 'ENTER IFLNO, IPOS'
		READ  (5,*) IFLNO,IPOS
		CALL DM_RRWH (IFLNO,IPOS,IHEADR,IRET)
		WRITE (6,*) 'IRET = ', IRET
		WRITE (6,*) 'IHEADR = ',(IHEADR(I),I=1,KRKEYS(IFLNO))
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_RCLH' ) THEN
		WRITE (6,*) 'ENTER IFLNO, IPOS'
		READ  (5,*) IFLNO,IPOS
		CALL DM_RCLH (IFLNO,IPOS,IHEADR,IRET)
		WRITE (6,*) ' IRET = ', IRET
		WRITE (6,*) 'IHEADR = ',(IHEADR(I),I=1,KCKEYS(IFLNO))
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WFHR' ) THEN
		WRITE (6,*) 'ENTER IFLNO, NWORD'
		READ  (5,*) IFLNO, NWORD
		WRITE (6,*) 'ENTER FHDNAM'
		READ  (5,2) FHDNAM (1)
		WRITE (6,*) 'ENTER DATA, RINC'
		READ  (5,*) RRR, RINC
		DO  I = 1, NWORD
		    DATA (I) = RRR
		    RRR  = RRR + RINC
		END DO
		CALL DM_WFHR ( IFLNO, FHDNAM, DATA, NWORD, IRET )
		WRITE (6,*) ' IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WFHI' ) THEN
		WRITE (6,*) 'ENTER IFLNO, NWORD'
		READ  (5,*) IFLNO, NWORD
		WRITE (6,*) 'ENTER FHDNAM'
		READ  (5,2) FHDNAM (1)
		WRITE (6,*) 'ENTER DATA, INC'
		READ  (5,*) IRRR, IRINC
		DO  I = 1, NWORD
		    IDATA (I) = IRRR
		    IRRR  = IRRR + IRINC
		END DO
		CALL DM_WFHI ( IFLNO, FHDNAM, IDATA, NWORD, IRET )
		WRITE (6,*) ' IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WFHC' ) THEN
		WRITE (6,*) 'ENTER IFLNO, NCHAR'
		READ  (5,*) IFLNO, NCHAR
		WRITE (6,*) 'ENTER FHDNAM'
		READ  (5,2) FHDNAM (1)
		WRITE (6,*) 'ENTER STRING'
		READ  (5,2) STRING
		CALL DM_WFHC ( IFLNO, FHDNAM, STRING, NCHAR, IRET )
		WRITE (6,*) ' IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_RFHR' ) THEN
		WRITE (6,*) 'ENTER IFLNO, MXWORD'
		READ  (5,*) IFLNO, MXWORD
		WRITE (6,*) 'ENTER FHDNAM'
		READ  (5,2) FHDNAM (1)
		CALL DM_RFHR (IFLNO,FHDNAM,MXWORD,DATA,NWORD,IRET)
		WRITE (6,*) ' IRET = ', IRET, '  NWORD = ', NWORD
		WRITE (6,*) 'DATA: ', (DATA(I),I=1,NWORD)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_RFHI' ) THEN
		WRITE (6,*) 'ENTER IFLNO, MXWORD'
		READ  (5,*) IFLNO, MXWORD
		WRITE (6,*) 'ENTER FHDNAM'
		READ  (5,2) FHDNAM (1)
		CALL DM_RFHI (IFLNO,FHDNAM,MXWORD,IDATA,NWORD,IRET)
		WRITE (6,*) ' IRET = ', IRET, '  NWORD = ',NWORD
		WRITE (6,*) 'IDATA: ',(IDATA(I),I=1,NWORD)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_RFHC' ) THEN
		WRITE (6,*) 'ENTER IFLNO, MXCHAR'
		READ  (5,*) IFLNO, MXCHAR
		WRITE (6,*) 'ENTER FHDNAM'
		READ  (5,2) FHDNAM (1)
		CALL DM_RFHC (IFLNO,FHDNAM,MXCHAR,STRING,NCHAR,IRET)
		WRITE (6,*) ' IRET = ', IRET, ' NCHAR = ', NCHAR
		WRITE (6,*) 'STRING: ',STRING (1:NCHAR)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WDTI' ) THEN
		WRITE (6,*) 'ENTER IFLNO,IROW,ICOL,NWORD'
		READ  (5,*) IFLNO, IROW, ICOL,NWORD
		WRITE (6,*) 'ENTER PART'
		READ  (5,2) PART
		WRITE (6,*) 'ENTER DATA, INC'
		READ  (5,*) IRRR, IRINC
		DO  I = 1, NWORD
		    IDATA (I) = IRRR
		    IRRR  = IRRR + IRINC
		END DO
		CALL DM_WDTI (IFLNO,IROW,ICOL,PART,IDTHDR,
     +				IDATA,NWORD,IRET)
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WDTR' ) THEN
		WRITE (6,*) 'ENTER IFLNO,IROW,ICOL,NWORD'
		READ  (5,*) IFLNO, IROW, ICOL,NWORD
		WRITE (6,*) 'ENTER PART'
		READ  (5,2) PART
		WRITE (6,*) 'ENTER DATA, INC'
		READ  (5,*) RRR, RINC
		DO  I = 1, NWORD
		    DATA (I) = RRR
		    RRR  = RRR + RINC
		END DO
		CALL DM_WDTR (IFLNO,IROW,ICOL,PART,IDTHDR,
     +					DATA,NWORD,IRET)
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WDTC' ) THEN
		WRITE (6,*) 'ENTER IFLNO,IROW,ICOL,NCHAR'
		READ  (5,*) IFLNO, IROW, ICOL,NCHAR
		WRITE (6,*) 'ENTER PART'
		READ  (5,2) PART
		WRITE (6,*) 'ENTER STRING'
		READ  (5,2) STRING
		CALL DM_WDTC (IFLNO,IROW,ICOL,PART,IDTHDR,
     +					STRING,NCHAR,IRET)
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_RDTI' ) THEN
		WRITE (6,*) 'ENTER IFLNO,IROW,ICOL'
		READ  (5,*) IFLNO, IROW, ICOL
		WRITE (6,*) 'ENTER PART'
		READ  (5,2) PART
		CALL DM_RDTI (IFLNO,IROW,ICOL,PART,IDTHDR,
     +					IDATA,NWORD,IRET)
		WRITE (6,*) 'IRET = ', IRET, '  NWORD = ', NWORD
		WRITE (6,*) 'DATA:',(IDATA(I),I=1,NWORD)
		WRITE (6,*) 'HEADER:',(IDTHDR(I),I=1,10)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_RDTR' ) THEN
		WRITE (6,*) 'ENTER IFLNO,IROW,ICOL'
		READ  (5,*) IFLNO, IROW, ICOL
		WRITE (6,*) 'ENTER PART'
		READ  (5,2) PART
		CALL DM_RDTR (IFLNO,IROW,ICOL,PART,IDTHDR,
     +					DATA,NWORD,IRET)
		WRITE (6,*) 'IRET = ', IRET,  '  NWORD = ', NWORD
		WRITE (6,*) 'DATA:',(DATA(I),I=1,NWORD)
		WRITE (6,*) 'HEADER:',(IDTHDR(I),I=1,10)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_QDAT' ) THEN
		WRITE (6,*) 'ENTER IFLNO,IROW,ICOL'
		READ  (5,*) IFLNO, IROW, ICOL
		WRITE (6,*) 'ENTER PART'
		READ  (5,2) PART
		CALL DM_QDAT (IFLNO,IROW,ICOL,PART,DATFLG,IRET)
		WRITE (6,*) 'DATFLG, IRET: ', DATFLG, ' ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_RDTC' ) THEN
		WRITE (6,*) 'ENTER IFLNO,IROW,ICOL'
		READ  (5,*) IFLNO, IROW, ICOL
		WRITE (6,*) 'ENTER PART'
		READ  (5,2) PART
		CALL DM_RDTC (IFLNO,IROW,ICOL,PART,IDTHDR,
     +					STRING,NCHAR,IRET)
		WRITE (6,*) 'IRET = ', IRET,'  NCHAR = ', NCHAR
		WRITE (6,*) 'STRING:',STRING (1:NCHAR)
		WRITE (6,*) 'HEADER:',(IDTHDR(I),I=1,10)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_KEYS' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		CALL DM_KEYS (IFLNO,NRKEYS,KEYROW,NCKEYS,KEYCOL,IRET)
		WRITE (6,*) 'NRKEYS,NCKEYS,IRET',NRKEYS,NCKEYS,IRET
		WRITE (6,*) 'KEYROW: ',(KEYROW(I),' ',I=1,NRKEYS)
		WRITE (6,*) 'KEYCOL: ',(KEYCOL(I),' ',I=1,NCKEYS)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_FKEY' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		WRITE (6,*) 'ENTER KEY'
		READ  (5,2) KEYNAM
		CALL DM_FKEY (IFLNO,KEYNAM,TYPE,LOC,IRET )
		WRITE (6,*) 'TYPE,LOC,IRET: ',TYPE,' ',LOC,IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_PNAM' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		CALL DM_PNAM (IFLNO,NPRT,PRTNAM,IRET)
		WRITE (6,*) 'NPRT,IRET = ',NPRT,IRET
		WRITE (6,*) 'PRTNAM: ',(PRTNAM(I),' ',I=1,NPRT)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_PART' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		WRITE (6,*) 'ENTER PRTNAM'
		READ  (5,2) PRTNAM (1)
		CALL DM_PART (IFLNO,PRTNAM,LENHDR,ITYPRT,NPARMS,
     +				PRMNAM,ISCALE,IOFFST,NBITS,IRET)
		WRITE (6,*) 'LENHDR,ITYPRT,NPARMS,IRET: ',LENHDR(1),
     +				ITYPRT(1),NPARMS(1),IRET
		DO  J = 1, NPARMS (1)
		    WRITE (6,*) 'PRMNAM,ISCALE,IOFFST,NBITS:',
     +				PRMNAM(J),ISCALE(J),IOFFST(J),NBITS(J)
		END DO
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_BEGS' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		CALL DM_BEGS (IFLNO,IRET )
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_SRCH' ) THEN
		WRITE (6,*) 'ENTER IFLNO, NKEY'
		READ  (5,*) IFLNO, NKEY
		WRITE (6,*) 'ENTER TYPE'
		READ  (5,2) TYPE
		WRITE (6,*) 'ENTER KEYLOC'
		READ  (5,*) (KEYLOC(I),I=1,NKEY)
		WRITE (6,*) 'ENTER KEYVAL'
		READ  (5,*) (KEYVAL(I),I=1,NKEY)
		CALL DM_SRCH ( IFLNO,TYPE,NKEY,KEYLOC,KEYVAL,IRWCL,IRET)
		WRITE (6,*) 'IRET, IRWCL: ',IRET, IRWCL
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_CLOP' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*)  IFLNO
		CALL DM_CLOP  ( IFLNO, IRET )
		WRITE (6,*) 'IRET = ', IRET
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_PSRC' ) THEN
		WRITE (6,*) 'ENTER IFLNO, NKEYS'
		READ  (5,*) IFLNO,NKEYS
		WRITE (6,*) 'ENTER ',NKEYS,' KEY NAMES'
		READ  (5,2) (KEYROW(I),I=1,NKEYS)
		WRITE (6,*) 'ENTER ',NKEYS,' LOW VALUES'
		READ  (5,*) (ILOVAL(I),I=1,NKEYS)
		WRITE (6,*) 'ENTER ',NKEYS,' HIGH VALUES'
		READ  (5,*) (IHIVAL(I),I=1,NKEYS)
		CALL DM_PSRC (IFLNO,NKEYS,KEYROW,ILOVAL,IHIVAL,IRET)
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_CSRC' ) THEN
		WRITE (6,*) 'ENTER IFLNO, NKEYS'
		READ  (5,*) IFLNO,NKEYS
		WRITE (6,*) 'ENTER ADDSRC'
		READ  (5,*) ADDFLG
		WRITE (6,*) 'ENTER ',NKEYS,' KEY NAMES'
		READ  (5,2) (KEYROW(I),I=1,NKEYS)
		WRITE (6,*) 'ENTER ',NKEYS,' LOW VALUES'
		READ  (5,*) (ILOVAL(I),I=1,NKEYS)
		WRITE (6,*) 'ENTER ',NKEYS,' HIGH VALUES'
		READ  (5,*) (IHIVAL(I),I=1,NKEYS)
		CALL DM_CSRC (IFLNO,ADDFLG,NKEYS,KEYROW,ILOVAL,
     +					IHIVAL,IRET)
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_DALL' ) THEN
		WRITE (6,*) 'ENTER IFLNO, NKEYS'
		READ  (5,*) IFLNO,NKEYS
		WRITE (6,*) 'ENTER ',NKEYS,' KEY NAMES'
		READ  (5,2) (KEYROW(I),I=1,NKEYS)
		WRITE (6,*) 'ENTER ',NKEYS,' LOW VALUES'
		READ  (5,*) (ILOVAL(I),I=1,NKEYS)
		WRITE (6,*) 'ENTER ',NKEYS,' HIGH VALUES'
		READ  (5,*) (IHIVAL(I),I=1,NKEYS)
		CALL DM_DALL (IFLNO,NKEYS,KEYROW,ILOVAL,IHIVAL,IRET)
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	     ELSE IF ( INPUT .EQ. 'DM_DDAT' ) THEN
		WRITE (6,*) 'ENTER IFLNO,IROW,ICOL'
		READ  (5,*) IFLNO, IROW, ICOL
		WRITE (6,*) 'ENTER PART NAME'
		READ  (5,2) PRTNAM(1)
		CALL DM_DDAT (IFLNO,IROW,ICOL,PRTNAM(1),IRET)
		WRITE (6,*) 'IRET = ', IRET		
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	     ELSE IF ( INPUT .EQ. 'DM_DPSR' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		CALL DM_DPSR ( IFLNO, IRET )
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	     ELSE IF ( INPUT .EQ. 'DM_DCSR' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		CALL DM_DCSR ( IFLNO, IRET )
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_NEXT' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		CALL DM_NEXT (IFLNO,IROW,ICOL,IRET)
		WRITE (6,*) 'IROW,ICOL,IRET = ', IROW,ICOL,IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_COND' ) THEN
		WRITE (6,*) 'ENTER IFLNO,ISRCH,IROW,ICOL'
		READ  (5,*) IFLNO,ISRCH,IROW,ICOL
		CALL DM_COND (IFLNO,ISRCH,IROW,ICOL,SFLAG,IRET)
		WRITE (6,*) 'SFLAG,IRET: ',SFLAG,IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WORD' ) THEN
		WRITE (6,*) 'ENTER IWORD'
		READ  (5,*) IWORD
		CALL DM_WORD ( IWORD, IREC, JWORD, IRET )
		WRITE (6,*) 'IREC,JWORD,IRET: ',IREC,JWORD,IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_GSPC' ) THEN
		WRITE (6,*) 'ENTER IFLNO, NWORD'
		READ  (5,*) IFLNO, NWORD
		CALL DM_GSPC ( IFLNO, NWORD, ISTART, IRET )
		WRITE (6,*) 'IRET, ISTART: ',IRET,ISTART
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_FSPC' ) THEN
		WRITE (6,*) 'ENTER IFLNO, NWORD, ISTART'
		READ  (5,*) IFLNO, NWORD, ISTART
		CALL DM_FSPC ( IFLNO, NWORD, ISTART, IRET )
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C-------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_EFRE' ) THEN
		WRITE (6,*) 'IFLNO, NBLK'
		READ  (5,*) IFLNO, NBLK
		CALL DM_EFRE ( IFLNO, NBLK, IRET )
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_AFRE' ) THEN
		WRITE (6,*) 'ENTER IFLNO, NUMBLK, LOCBLK'
		READ  (5,*) IFLNO, NUMBLK, LOCBLK
		CALL DM_AFRE (IFLNO, NUMBLK, LOCBLK,IRET)
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'ST_CTOI' ) THEN
		WRITE (6,*) 'ENTER NVAL'
		READ  (5,*) NVAL
		WRITE (6,*) 'ENTER CHARRY: '
		READ  (5,2) (CHARRY(I),I=1,NVAL)
		CALL ST_CTOI (CHARRY, NVAL, IARRAY, IRET )
		WRITE (6,*) 'IRET,IARRAY = ', IRET,(IARRAY(I),I=1,NVAL)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'ST_ITOC' ) THEN
		WRITE (6,*) 'ENTER NVAL'
		READ  (5,*) NVAL
		WRITE (6,*) 'ENTER IARRAY: '
		READ  (5,*) (IARRAY(I),I=1,NVAL)
		CALL ST_ITOC (IARRAY,NVAL,CHARRY,IRET )
		WRITE (6,*) 'IRET,CHARRY',IRET, (' ',CHARRY(I),I=1,NVAL)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_RINT' ) THEN
		WRITE (6,*) 'ENTER IFLNO,ISWORD,NWORD'
		READ  (5,*) IFLNO,ISWORD,NWORD
		CALL DM_RINT (IFLNO,ISWORD,NWORD,IDATA,IRET)
		WRITE (6,*) 'IRET,IDATA: ',IRET,(IDATA(I),I=1,NWORD)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WINT' ) THEN
		WRITE (6,*) 'ENTER IFLNO,ISWORD,NWORD'
		READ  (5,*) IFLNO,ISWORD,NWORD
		WRITE (6,*) 'ENTER ISTART,INC'
		READ  (5,*) ISTART,INC
		DO I = 1, NWORD
		    IDATA (I) = ISTART
		    ISTART = ISTART + INC
		END DO
		CALL DM_WINT (IFLNO,ISWORD,NWORD,IDATA,IRET )
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_RFLT' ) THEN
		WRITE (6,*) 'ENTER IFLNO,ISWORD,NWORD'
		READ  (5,*) IFLNO,ISWORD,NWORD
		CALL DM_RFLT (IFLNO,ISWORD,NWORD,DATA,IRET)
		WRITE (6,*) 'IRET,DATA: ',IRET,(DATA(I),I=1,NWORD)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WFLT' ) THEN
		WRITE (6,*) 'ENTER IFLNO,ISWORD,NWORD'
		READ  (5,*) IFLNO,ISWORD,NWORD
		WRITE (6,*) 'ENTER START,RINC'
		READ  (5,*) START,RINC
		DO I = 1, NWORD
		    DATA (I) = START
		    START = START + RINC
		END DO
		CALL DM_WFLT (IFLNO,ISWORD,NWORD,DATA,IRET )
		WRITE (6,*) 'IRET = ', IRET,(DATA(I),I=1,NWORD)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_RCH4' ) THEN
		WRITE (6,*) 'ENTER IFLNO,ISWORD,NWORD'
		READ  (5,*) IFLNO,ISWORD,NWORD
		CALL DM_RCH4 (IFLNO,ISWORD,NWORD,CHARRY,IRET)
		WRITE (6,*) 'IRET,IDATA: ',IRET,(CHARRY(I),' ',I=1,NWORD)
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WCH4' ) THEN
		WRITE (6,*) 'ENTER IFLNO,ISWORD,NWORD'
		READ  (5,*) IFLNO,ISWORD,NWORD
		WRITE (6,*) 'ENTER CHARRY ON SEPARATE LINES'
		DO I = 1, NWORD
		    READ (5,2) CHARRY (I)
		END DO
		CALL DM_WCH4 (IFLNO,ISWORD,NWORD,CHARRY,IRET )
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_RSTR' ) THEN
		WRITE (6,*) 'ENTER IFLNO,ISWORD,NCHAR'
		READ  (5,*) IFLNO,ISWORD,NCHAR
		CALL DM_RSTR (IFLNO,ISWORD,NCHAR,STRING,IRET)
		WRITE (6,*) 'IRET,STRING: ',IRET,' ',STRING
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_WSTR' ) THEN
		WRITE (6,*) 'ENTER IFLNO,ISWORD,NCHAR'
		READ  (5,*) IFLNO,ISWORD,NCHAR
		WRITE (6,*) 'ENTER STRING'
		READ  (5,2) STRING
		CALL DM_WSTR (IFLNO,ISWORD,NCHAR,STRING,IRET)
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_FWRT' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		CALL DM_FWRT (IFLNO, IRET )
		WRITE (6,*) 'IRET = ', IRET
		IF (IRET .NE. 0) CALL ER_WMSG ('DM',IRET,' ',IER)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_DRWH' ) THEN
		WRITE (6,*) 'ENTER IFLNO, IPOS'
		READ  (5,*) IFLNO, IPOS
	        CALL DM_DRWH (IFLNO, IPOS, IRET)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DM_DCLH' ) THEN
		WRITE (6,*) 'ENTER IFLNO, IPOS'
		READ  (5,*) IFLNO, IPOS
	        CALL DM_DCLH (IFLNO, IPOS, IRET)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DUMP' ) THEN
		IF  ( (COMMN .NE. 'DMLABL').AND.(COMMN.NE.'DMDMGT').AND.
     +			(COMMN.NE.'DMKEYS').AND.(COMMN.NE.'DMHDRS').AND.
     +			(COMMN.NE.'DMCACH').AND.(COMMN.NE.'DMFILE').AND.
     +			(COMMN.NE.'DMPART').AND.(COMMN.NE.'DMFHDR'))THEN
		  WRITE (6,1010)
1010		  FORMAT ( '    DMLABL  DMDMGT  DMKEYS  DMHDRS ',/
     +                   '    DMCACH  DMFILE  DMPART  DMFHDR ' /
     +                   ' ENTER COMMON BLOCK NAME: ', $ )
		  READ  (5,2 ) INPUT
		  CALL ST_LCUC ( INPUT, INPUT, IER ) 
	        ELSE
		  INPUT = COMMN
		END IF
C*
		IF  ( INPUT .EQ. 'DMLABL' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		WRITE (6,4) KVERSN(IFLNO),KFHDRS(IFLNO),KPFILE(IFLNO),
     +			    KROW(IFLNO),
     +			    KRKEYS(IFLNO),KPRKEY(IFLNO),KPROWH(IFLNO),
     +			    KCOL(IFLNO),KCKEYS(IFLNO),KPCKEY(IFLNO),
     +			    KPCOLH(IFLNO),KPRT(IFLNO),KPPART(IFLNO),
     +			    KPDMGT(IFLNO),KLDMGT(IFLNO),KPDATA(IFLNO),
     +			    KFTYPE(IFLNO),KFSRCE(IFLNO),KMACHN(IFLNO),
     +			    KMISSD(IFLNO),SMISSD(IFLNO),KVMST(IFLNO),
     +			    MVMST, KIEEET(IFLNO), MIEEET
4	    FORMAT (' KVERSN = ',I5,5X,'KFHDRS = ',I5,5X,'KPFILE = ',
     +          I5,5X,/,
     +          ' KROW   = ',I5,5X,'KRKEYS = ',I5,5X,'KPRKEY = ',
     +          I5,5X,'KPROWH = ',I5,/,' KCOL   = ',I5,5X,
     +          'KCKEYS = ',I5,5X,'KPCKEY = ',I5,5X,'KPCOLH = ',
     +          I5,/,' KPRT   = ',I5,5X,'KPPART = ',I5,5X,
     +          'KPDMGT = ',I5,5X,'KLDMGT = ',I5,/,' KPDATA = ',
     +          I5,5X,'KFTYPE = ',I5,5X,'KFSRCE = ',I5 ,/,
     +          ' KMACHN = ',I5,5X,'KMISSD = ',I6,4X,
     +          ' SMISSD = ',F8.1,/,
     +          ' KVMST,MVMST,KIEEET,MIEEET = ', 4L2 )
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DMDMGT' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		WRITE (6,*) 'KPNEXT,KNFREE,KMFREE',KPNEXT (IFLNO),
     +				KNFREE(IFLNO),KMFREE(IFLNO)
		WRITE (6,*) 'KLSTWD: ',KLSTWD (IFLNO)
		DO  I = 1, KNFREE (IFLNO)
		  WRITE (6,*) I, (KFREEW (J,I,IFLNO),J=1,2)
		END DO
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DMKEYS' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
3		FORMAT (10 (2X,A4) )
		WRITE (6,*) 'ROW KEYS'
		WRITE (6,3) (KKROW(I,IFLNO),I=1,KRKEYS(IFLNO))
		WRITE (6,*) 'COL KEYS'
		WRITE (6,3) (KKCOL(I,IFLNO),I=1,KCKEYS(IFLNO))
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DMHDRS' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		WRITE (6,*) 'KLSTRW = ',KLSTRW(IFLNO),'  KLSTCL = ',
     +				KLSTCL(IFLNO)
		WRITE (6,*) 'ROW HEADERS-------------'
		IHD = 0
		DO I = 1, KROW (IFLNO)
		  IHD = IHD + 1
		  WRITE (6,*) IHD,
     +				(KHEADR (J,IHD,IFLNO),J=0,KRKEYS(IFLNO))
		END DO
		WRITE (6,*) 'COL HEADERS---------------'
		DO I = 1, KCOL (IFLNO)
		  IHD = IHD + 1
		  WRITE (6,*) IHD,
     +				(KHEADR (J,IHD,IFLNO),J=0,KCKEYS(IFLNO))
		END DO
C------------------------------------------------------------------------
	      ELSE IF (INPUT .EQ. 'DMCACH' ) THEN
		WRITE (6,*) 'KCLAST = ', KCLAST
		WRITE (6,*) 'KCFLNO = ', (KCFLNO (I),I=1,MCACHE)
		WRITE (6,*) 'KCRECN = ', (KCRECN (I),I=1,MCACHE)
		WRITE (6,*) 'KWRITE = ', (KWRITE (I),I=1,MCACHE)
		WRITE (6,*) 'ENTER CACHE NUMBER'
		READ  (5,*) MCNUM
		IF ( MCNUM .GT. 0 ) WRITE (6,*) 'CACHE: ',
     +				(KCDATA (I,MCNUM),I=1,128)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DMFILE' ) THEN
		WRITE (6,*) 'LUNDM  = ',( LUNDM (IFLNO),IFLNO=1,MMFILE)
		WRITE (6,*) 'WFLAG  = ', ( WFLAG (IFLNO),IFLNO=1,MMFILE)
		WRITE (6,*) 'KSHARE = ', (KSHARE (IFLNO),IFLNO=1,MMFILE)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DMPART' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		NPRT = KPRT (IFLNO)
		WRITE (6,*) ' PART NAMES'
		WRITE (6,3) (KPRTNM(I,IFLNO),I=1,NPRT)
		WRITE (6,*) ' HEADER LENGTHS'
		WRITE (6,*) (KLNHDR(I,IFLNO),I=1,NPRT)
		WRITE (6,*) ' PART DATA TYPES'
		WRITE (6,*) (KTYPRT (I,IFLNO),I=1,NPRT)
		WRITE (6,*) ' # PARAMETERS'
		WRITE (6,*) (KPARMS(I,IFLNO),I=1,NPRT)
		WRITE (6,*) ' PACKING NOS'
		WRITE (6,*) (KPKNO(I,IFLNO),I=1,NPRT)
		WRITE (6,*) ' PACKED WORDS'
		WRITE (6,*) (KWORDP(I,IFLNO),I=1,NPRT)
		DO  J = 1, NPRT
		    WRITE (6,*) ' PARAMETER NAMES: ', KPRTNM(J,IFLNO)
		    WRITE (6,3) (KPRMNM(I,J,IFLNO),I=1,KPARMS(J,IFLNO))
		    WRITE (6,*) (KSCALE(I,J,IFLNO),I=1,KPARMS(J,IFLNO))
		    WRITE (6,*) (KOFFST(I,J,IFLNO),I=1,KPARMS(J,IFLNO))
		    WRITE (6,*) (KBITS(I,J,IFLNO),I=1,KPARMS(J,IFLNO))
		END DO
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DMFHDR' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		IFH = KFHDRS (IFLNO)
		WRITE (6,*) 'FILE HEADER NAMES'
		WRITE (6,3) (KFHNAM(I,IFLNO),I=1,IFH)
		WRITE (6,*) 'FILE HEADER LENGTHS'
		WRITE (6,*) (KFHLEN(I,IFLNO),I=1,IFH)
		WRITE (6,*) 'FILE HEADER TYPES'
		WRITE (6,*) (KFHTYP(I,IFLNO),I=1,IFH)
C------------------------------------------------------------------------
	      ELSE IF ( INPUT .EQ. 'DMSRCH' ) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		WRITE (6,*) 'KSROW,KSCOL: ',KSROW(IFLNO),KSCOL(IFLNO)
		WRITE (6,*) 'NSRCH,SRCFLG: ',NSRCH(IFLNO),SRCFLG(IFLNO)
		DO  I = 0, NSRCH (IFLNO)
		    WRITE (6,*) 'SEARCH # ', I, ' ADDFLG = ', 
     +					KADDSR(I,IFLNO)
		    WRITE (6,*) 'KSNROW = ', KSNROW (I,IFLNO)
		    DO  J = 1, KSNROW (I,IFLNO)
			WRITE (6,*) 'J, KSLROW,KSRLOV,KSRHIV: ', J,
     +				KSLROW(J,I,IFLNO),KSRLOV(J,I,IFLNO),
     +				KSRHIV(J,I,IFLNO)
		    END DO
		    WRITE (6,*) 'KSNCOL = ', KSNCOL (I,IFLNO)
		    DO  J = 1, KSNCOL (I,IFLNO)
			WRITE (6,*) 'J, KSLCOL,KSCLOV,KSCHIV: ', J,
     +				KSLCOL(J,I,IFLNO),KSCLOV(J,I,IFLNO),
     +				KSCHIV(J,I,IFLNO)
		    END DO
		END DO
C------------------------------------------------------------------------
	      END IF
	    END IF
C*
	END DO
	END
