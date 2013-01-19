	SUBROUTINE SN_CREF ( filnam, iflsrc, nparm, parms, maxstn, 
     +                       maxtim, pkflg, iscale, iofset, ibits, 
     +                       stmflg, isnfln, iret)
C************************************************************************
C* SN_CREF								*
C*									*
C* This subroutine creates a new standard sounding data file.  The	*
C* file will store times as rows of a DM file and stations as		*
C* columns.  If the packing flag, PKFLG, is set, data will be packed	*
C* using values in ISCALE, IOFSET and IBITS.  Note that SN_CRFP gets	*
C* packing information from a file.  If the station time flag is set, a *
C* single word is allocated with each data report to store the report	*
C* time (HHMM).  This time should be sent to SN_WDAT.			*
C*									*
C* The data source values are parameters in GEMPRM.PRM.		*
C*									*
C* SN_CREF  ( FILNAM, IFLSRC, NPARM, PARMS,  MAXSTN, MAXTIM, PKFLG, 	*
C*            ISCALE, IOFSET, IBITS, STMFLG, ISNFLN, IRET)		*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Surface file name		*
C*	IFLSRC		INTEGER		Data source 			*
C*	NPARM		INTEGER		Number of parameters 		*
C*	PARMS  (NPARM)	CHAR*4		Parameter names 		*
C*	MAXSTN		INTEGER		Maximum number of stations 	*
C*	MAXTIM		INTEGER  	Maximum number of times 	*
C*	PKFLG		LOGICAL		Packing flag			*
C*	ISCALE (NPARM)	INTEGER	 	Scaling factor			*
C*	IOFSET (NPARM)	INTEGER		Offset term			*
C*	IBITS  (NPARM)	INTEGER	 	Number of bits			*
C*	STMFLG		LOGICAL		Station time flag		*
C*									*
C* Output parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	IRET		INTEGER		Return code			*
C*				    	  0 = normal return		*
C*				   	 -1 = file not created		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 4/87	From SF_CREF				*
C* M. desJardins/GSFC	 8/87	Redone					*
C* K. Brill/NMC		 8/93	Changes for 8-char stn ID		*
C* m.gamazaychikov/SAIC	03/08	Changed to iflsrc in call to DM_CRET    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'sncmn.cmn'
C*
	CHARACTER*(*) 	filnam, parms (*)
	INTEGER		iscale (*), iofset (*), ibits (*)
	LOGICAL 	pkflg, stmflg
C*
	CHARACTER	kcolsn (8)*4, fhdnam*4, prtnam*4, krowtm (2)*4
	DATA		krowtm /'DATE', 'TIME' /
	DATA		kcolsn /'STID', 'STNM', 'SLAT', 'SLON', 'SELV',
     +                          'STAT', 'COUN', 'STD2' /
C------------------------------------------------------------------------
	iret = 0
C
C*	Set variables necessary for DM package.
C
	iftype = MFSN
	nfhdrs = 0
	fhdnam = ' '
	ifhlen = 0
	ifhtyp = 0
	nrow = maxtim
	nrkeys = 2
	ncol = maxstn
	nckeys = 8
	nprt = 1
	prtnam = 'SNDT'
C
C*	Check to see if time is to be stored with each report.
C
	IF  (stmflg)  THEN
	    lenhdr = 1
	  ELSE
	    lenhdr = 0
	END IF
C
C*	Set up correct data type.
C
	IF  (pkflg)  THEN
	    ityprt = MDRPCK
	  ELSE
	    ityprt = MDREAL
	END IF
C
C*	Create the file.
C
	maxprm = nparm
	CALL DM_CRET (filnam, iftype, iflsrc, nfhdrs, fhdnam, ifhlen,
     +                ifhtyp, nrow, nrkeys, krowtm, ncol, nckeys, 
     +	              kcolsn, nprt, prtnam, lenhdr, ityprt, nparm, 
     +	              maxprm, parms, iscale, iofset, ibits, isnfln,
     +                iret)
C
C*	Write out errors.
C
	IF (iret .ne. 0) THEN
	    CALL ER_WMSG ('DM', iret, filnam, ier)
	    iret = -1
	    CALL ER_WMSG ('SN', iret, filnam, ier)
	    RETURN
	  ELSE
C
C*	    Load common areas.
C
	    isndfn (isnfln) = isnfln
	    dttype (isnfln) = 'ROW'
	    sttype (isnfln) = 'COL'
	    kdate  (isnfln) = 1
	    ktime  (isnfln) = 2
	    kstid  (isnfln) = 1
	    kstnm  (isnfln) = 2
	    kslat  (isnfln) = 3
	    kslon  (isnfln) = 4
	    kselv  (isnfln) = 5
	    kstat  (isnfln) = 6
	    kcoun  (isnfln) = 7
	    kstd2  (isnfln) = 8
	    kparm  (isnfln) = nparm
	    krow   (isnfln) = 0
	    kcol   (isnfln) = 0
	    curtim (isnfln) = ' '
	    curstn (isnfln) = ' '
	    icrstn (isnfln,1) = 0
	    icrstn (isnfln,2) = 0
	    timset (isnfln) = .false.
	    stnset (isnfln) = .false.
	    ftmset (isnfln) = .false.
	    mrgtyp (isnfln) = .true.
	END IF
C*
	RETURN
	END
