	SUBROUTINE SF_CREF  ( filnam, iflsrc, nparm, parms, maxstn, 
     +			      maxtim, pkflg, iscale, iofset, ibits, 
     +			      stmflg, isffln, iret)
C************************************************************************
C* SF_CREF								*
C*									*
C* This subroutine creates a new standard surface data file.  The file	*
C* will store times as rows of a DM file and stations as columns.  	*
C*									*
C* If the packing flag, PKFLG, is set, data will be packed using 	*
C* values in ISCALE, IOFSET and IBITS.  Note that SF_CRFP reads the	*
C* parameters and packing information from a GEMPAK packing file.	*
C*									*
C* If the station time flag is set, a single word is allocated with 	*
C* each data report to store the report time (HHMM).  This time 	*
C* should be sent to SF_WDAT.  						*
C*									*
C* If the file cannot be created, error messages will be written.	*
C*									*
C* The data source values are parameters in GEMPRM.PRM .		*
C* Current definitions include:						*
C*      MFUNKN        unknown						*
C*      MFAIRW        airways surface observation			*
C*      MFMETR        Metar report					*
C*      MFSHIP        ship report					*
C*      MFBUOY        buoy report					*
C*      MFSYNP        synoptic report					*
C*									*
C* SF_CREF  ( FILNAM, IFLSRC, NPARM, PARMS, MAXSTN, MAXTIM, PKFLG,	*
C*            ISCALE, IOFSET, IBITS, STMFLG, ISFFLN, IRET )		*
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
C*	ISFFLN		INTEGER		Surface file number		*
C*	IRET		INTEGER		Return code			*
C*				    	  0 = normal return		*
C*				   	 -1 = file not created		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/84						*
C* I. Graffman/RDS	 4/87	From SF_CREATE for GEMPAK4		*
C* M. desJardins/GSFC	 2/88	Fixed documentation			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		 8/93	Added STD2 and SPRI			*
C* D. Keiser/GSC	 4/96	Add another part option			*
C* S. Jacobs/NCEP	 6/96	Added part 3 for AIRW and METR specials	*
C* S. Jacobs/NCEP	 7/96	Changed check for file source = 100	*
C*				  to only save text			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'sfcmn.cmn'
C*
	CHARACTER*(*) 	filnam, parms (*)
	INTEGER		iscale (*), iofset (*), ibits (*)
	LOGICAL 	pkflg, stmflg
C*
	CHARACTER	kcolsn (9)*4, fhdnam*4, prtnam (3)*4,
     +			krowtm (2)*4, prmnam (MMPARM, 3)*4
	INTEGER		lenhdr (3), ityprt (3),
     +			numprm (3), isc (MMPARM, 3),
     +			ioff (MMPARM, 3), ibit (MMPARM, 3)
C*
	DATA		krowtm / 'DATE', 'TIME' /
	DATA		kcolsn / 'STID', 'STNM', 'SLAT', 'SLON', 'SELV',
     +                           'STAT', 'COUN', 'STD2', 'SPRI' /
C------------------------------------------------------------------------
	iret = 0
C
C*	Set variables necessary for DM package.
C
	iftype = MFSF
	nfhdrs = 0
	fhdnam = ' '
	ifhlen = 0
	ifhtyp = 0
	nrow   = maxtim
	nrkeys = 2
	ncol   = maxstn
	nckeys = 9
	nprt   = 1
	numprm(1) = nparm
	DO i = 1, numprm(1)
	    prmnam(i,1) = parms(i)
	    isc (i,1)   = iscale(i)
	    ioff(i,1)	= iofset(i)
	    ibit(i,1)	= ibits(i)
	END DO
	IF  ( stmflg )  THEN
	    lenhdr(1) = 1
	    lenhdr(2) = 1
	    lenhdr(3) = 2
	  ELSE
	    lenhdr(1) = 0
	    lenhdr(2) = 0
	    lenhdr(3) = 1
	END IF
	IF ( iflsrc .gt. 100 ) THEN
C
C*	    Save the real data and the text data.
C
	    prtnam(1) = 'SFDT'
	    IF  ( pkflg )  THEN
	    	ityprt(1) = MDRPCK
	    ELSE
	    	ityprt(1) = MDREAL
	    END IF
	    prtnam(2) = 'SFTX'
	    nprt = 2
	    ityprt(2) = MDCHAR 
	    numprm(2) = 1
	    prmnam(1,2) = 'TEXT'
	    isc (1,2) = 0
	    ioff(1,2) = 0
	    ibit(1,2) = 0
C
C*	    Add the "SPECIAL"s part for SAO and METAR reports.
C
	    IF  ( ( iflsrc .eq. 101 ) .or. ( iflsrc .eq. 102 ) )  THEN
		prtnam (3) = 'SFSP'
		nprt = 3
		ityprt(3) = MDCHAR 
		numprm(3) = 1
		prmnam(1,3) = 'SPCL'
		isc (1,3) = 0
		ioff(1,3) = 0
		ibit(1,3) = 0
	    END IF
	  ELSE IF ( iflsrc .lt. 100 ) THEN
C
C*	    Save only the real data.
C
	    prtnam (1) = 'SFDT'
	    IF  ( pkflg )  THEN
	    	ityprt(1) = MDRPCK
	    ELSE
	    	ityprt(1) = MDREAL
	    END IF
	  ELSE
C
C*	    Save only the text data.
C
	    prtnam(1) = 'SFTX'
	    ityprt(1) = MDCHAR 
	END IF		
	maxprm = MMPARM
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
	    kcoun  (isffln) = 7
	    kstd2  (isffln) = 8
	    kspri  (isffln) = 9
	    kparm  (isffln) = nparm
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
