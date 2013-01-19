	SUBROUTINE SF_CRCN  ( filnam, maxcny, maxtim, isffln, iret)
C************************************************************************
C* SF_CRCN								*
C*									*
C* This subroutine creates a new watch-by-county file.  The file	*
C* will store times as rows of a DM file and counties as columns.  	*
C*									*
C* If the file cannot be created, error messages will be written.	*
C*									*
C* SF_CRCN  ( FILNAM, MAXCNY, MAXTIM, ISFFLN, IRET )			*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Surface file name		*
C*	MAXCNY		INTEGER		Maximum number of counties 	*
C*	MAXTIM		INTEGER  	Maximum number of times 	*
C*									*
C* Output parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	IRET		INTEGER		Return code			*
C*				    	  0 = normal return		*
C*				   	 -1 = file not created		*
C**									*
C* Log:									*
C* T. Lee/GSC		10/97	From SF_CREF				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
 	INCLUDE 	'sfcmn.cmn'
C*
	CHARACTER*(*) 	filnam
C*
	CHARACTER	kcolsn (10)*4, fhdnam*4, prtnam (2)*4,
     +			krowtm (2)*4, prmnam (MMPARM, 2)*4
	INTEGER		lenhdr (2), ityprt (2), numprm (2), 
     +			isc (MMPARM, 2), ioff (MMPARM, 2), 
     +			ibit (MMPARM, 2)
C*
	DATA		krowtm / 'DATE', 'TIME' /
	DATA		kcolsn / 'STID', 'STNM', 'SLAT', 'SLON', 'SELV',
     +                           'STAT', 'STD2', 'SPRI', 'SWFO', 'WFO2'/
C------------------------------------------------------------------------
	iret = 0
C
C*	Set variables necessary for DM package.
C
	iftype = MFSF
	iflsrc = MFCOUN
	nfhdrs = 0
	fhdnam = ' '
	ifhlen = 0
	ifhtyp = 0
	nrow   = maxtim
	nrkeys = 2
	ncol   = maxcny
	nckeys = 10
	nprt   = 2
	lenhdr(1) = 0
	lenhdr(2) = 0
C
	DO  i = 1, 2
	    isc (i,1) = 0
	    ioff(i,1) = 0
	    ibit(i,1) = 0
	END DO
	DO  j = 1, 12
	    isc  (j,2)  = 0
	    ioff (j,2)  = 0
	    ibit (j,2)  = 0    
	END DO
C
C*	Save the data.
C
	numprm(1)    = 3
	prtnam(1)    = 'SFDT'
	prmnam(1,1)  = 'ONF1'
	prmnam(2,1)  = 'ONF2'
	prmnam(3,1)  = 'IPTR'
	ityprt(1)    = MDREAL
C
	numprm(2)    = 12
	prtnam(2)    = 'CNBX'
	prmnam(1,2)  = 'LAT1'
	prmnam(2,2)  = 'LON1'
	prmnam(3,2)  = 'LAT2'
	prmnam(4,2)  = 'LON2'
	prmnam(5,2)  = 'LAT3'
	prmnam(6,2)  = 'LON3'
	prmnam(7,2)  = 'LAT4'
	prmnam(8,2)  = 'LON4'
	prmnam(9,2)  = 'LAT5'
	prmnam(10,2) = 'LON5'
	prmnam(11,2) = 'LAT6'
	prmnam(12,2) = 'LON6'
	ityprt(2)    = MDREAL 
	maxprm       = MMPARM
C
C*	Create the file.
C
	CALL DM_CRET (filnam, iftype, iflsrc, nfhdrs, fhdnam, ifhlen,
     +                ifhtyp, nrow, nrkeys, krowtm, ncol, nckeys, 
     +	              kcolsn, nprt, prtnam, lenhdr, ityprt, numprm, 
     +	              maxprm, prmnam, isc, ioff, ibit, isffln,
     +                iret)
C
C*	Check for errors.
C
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DM', iret, filnam, ier )
	    iret = -1
	    CALL ER_WMSG  ( 'SF', iret, filnam, ier )
	    RETURN
	  ELSE
C
C*	    Load common areas.
C
	    isfcfn (isffln) = isffln
	    dttype (isffln) = 'ROW'
	    sttype (isffln) = 'COL'
	    kdate  (isffln) = 1
	    ktime  (isffln) = 2
	    kstid  (isffln) = 1
	    kstnm  (isffln) = 2
	    kslat  (isffln) = 3
	    kslon  (isffln) = 4
	    kselv  (isffln) = 5
	    kstat  (isffln) = 6
	    kstd2  (isffln) = 7
	    kspri  (isffln) = 8
	    kswfo  (isffln) = 9
	    kwfo2  (isffln) = 10
	    kparm  (isffln) = 3
	    krow   (isffln) = 0
	    kcol   (isffln) = 0
	    curtim (isffln) = ' '
	    timset (isffln) = .false.
	    stnset (isffln) = .false.
	    ftmset (isffln) = .false.
	    curstn (isffln) = ' '
	END IF
C*
	RETURN
	END
