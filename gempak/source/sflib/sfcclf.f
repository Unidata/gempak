	SUBROUTINE SF_CCLF  ( filnam, iflsrc, nparm, parms, maxstn, 
     +			      maxtim, pkflg, iscale, iofset, ibits, 
     +			      stmflg, isffln, iret)
C************************************************************************
C* SF_CCLF								*
C*									*
C* This subroutine creates a new climate surface data file.  The file	*
C* will store stations as rows of a DM file and times as columns.  	*
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
C* These are not currently used by any GEMPAK program.  Current		*
C* definitions include:							*
C*      MFUNKN        unknown						*
C*      MFAIRW        airways surface observation			*
C*      MFMETR        Metar report					*
C*      MFSHIP        ship report					*
C*      MFBUOY        buoy report					*
C*      MFSYNP        synoptic report					*
C*									*
C* SF_CCLF  ( FILNAM, IFLSRC, NPARM, PARMS, MAXSTN, MAXTIM, PKFLG,	*
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
C* M. desJardins/GSFC	 3/90						*
C* K. Brill/NMC		 8/93	Added STD2 and SPRI			*
C* S. Jacobs/NCEP	 7/96	Updated documentation			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'sfcmn.cmn'
C*
	CHARACTER*(*) 	filnam, parms (*)
	INTEGER		iscale (*), iofset (*), ibits (*)
	LOGICAL 	pkflg, stmflg
C*
	CHARACTER	krowsn (9)*4, fhdnam*4, prtnam*4, kcoltm (2)*4
C*
	DATA		kcoltm / 'DATE', 'TIME' /
	DATA		krowsn / 'STID', 'STNM', 'SLAT', 'SLON', 'SELV',
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
	nrow   = maxstn
	nrkeys = 9
	ncol   = maxtim
	nckeys = 2
	nprt   = 1
	prtnam = 'SFDT'
	IF  ( stmflg )  THEN
	    lenhdr = 1
	  ELSE
	    lenhdr = 0
	END IF
	IF  ( pkflg )  THEN
	    ityprt = MDRPCK
	  ELSE
	    ityprt = MDREAL
	END IF
	maxprm = nparm
C
C*	Create the file.
C
	CALL DM_CRET (filnam, iftype, iflsrc, nfhdrs, fhdnam, ifhlen,
     +                ifhtyp, nrow, nrkeys, krowsn, ncol, nckeys, 
     +	              kcoltm, nprt, prtnam, lenhdr, ityprt, nparm, 
     +	              maxprm, parms, iscale, iofset, ibits, isffln,
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
	    dttype (isffln) = 'COL'
	    sttype (isffln) = 'ROW'
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
