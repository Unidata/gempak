	SUBROUTINE SF_CSDF  ( filnam, iflsrc, nparm,  parms, maxrpt,
     +			      pkflg,  iscale, iofset, ibits, stmflg, 
     +			      isffln, iret)
C************************************************************************
C* SF_CSDF								*
C*									*
C* This subroutine creates a new ship surface data file.  The		*
C* file will store times and stations together as columns in row	*
C* 1.  This type of file may be used to store data, such as ship	*
C* reports, where the station locations vary in time.			*
C*									*
C* If the packing flag, PKFLG, is set, data will be packed using 	*
C* values in ISCALE, IOFSET and IBITS.  Note that SF_CSDP reads		*
C* the parameters and packing information from a GEMPAK packing		*
C* file.								*
C*									*
C* If the station time flag is set, a single word is allocated with 	*
C* each data report to store the report time (HHMM).  This time 	*
C* should be sent with the report to SF_WSDD.				*
C*									*
C* The subroutine SF_WSDD will write data to this file.  The data can	*
C* be read using SF_RDAT; all GEMPAK programs will be able to read	*
C* this file.  Text data can be written to this file using SF_WSTR.     *
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
C* SF_CSDF  ( FILNAM, IFLSRC, NPARM, PARMS,  MAXRPT, PKFLG, 		*
C*            ISCALE, IOFSET, IBITS, STMFLG, ISFFLN, IRET )		*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Surface file name		*
C*	IFLSRC		INTEGER		Data source 			*
C*	NPARM		INTEGER		Number of parameters 		*
C*	PARMS  (NPARM)	CHAR*4		Parameter names 		*
C*	MAXRPT		INTEGER		Maximum number of reports	*
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
C* I. Graffman/RDS	 4/87	SF_CREF					*
C* M. desJardins/GSFC	 2/88	Adapted for single dimension files	*
C* K. Brill/NMC		09/91	Corrected mispelled variable names	*
C*				IFLSRC and KROWHD in CALL DM_CRET	*
C* K. Brill/NMC		 8/93	Added STD2 and SPRI			*
C* S. Jacobs/NCEP	 7/96	Updated documentation			*
C* D. Kidwell/NCEP	 7/99	Added ability to save text              *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'sfcmn.cmn'
C*
	CHARACTER*(*) 	filnam, parms (*)
	INTEGER		iscale (*), iofset (*), ibits (*)
	LOGICAL 	pkflg, stmflg
C*
	CHARACTER	kcolhd(11)*4, fhdnam*4, prtnam(2)*4,
     +			krowhd (1)*4, prmnam (MMPARM, 3)*4
	INTEGER		lenhdr (2), ityprt (2),
     +			nparms (2), isc (MMPARM, 2),
     +			ioff (MMPARM, 2), ibit (MMPARM, 2)
C*
	DATA		kcolhd / 'DATE', 'TIME', 'STID', 'STNM', 
     +				 'SLAT', 'SLON', 'SELV', 'STAT', 
     +				 'COUN', 'STD2', 'SPRI' /
	DATA		krowhd / 'REPT' /
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
	nrow   = 1
	nrkeys = 1
	ncol   = maxrpt
	nckeys = 11
	nprt   = 1
	nparms(1) = nparm
	DO i = 1, nparms(1)
	    prmnam(i,1) = parms(i)
	    isc (i,1)   = iscale(i)
	    ioff(i,1)	= iofset(i)
	    ibit(i,1)	= ibits(i)
	END DO
	IF (stmflg) THEN
	    lenhdr(1) = 1
	    lenhdr(2) = 1
	  ELSE
	    lenhdr(1) = 0
	    lenhdr(2) = 0
	END IF

        IF ( iflsrc .gt. 100 ) THEN
C
C*          Save the real data and the text data.
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
	    nparms(2) = 1
	    prmnam(1,2) = 'TEXT'
	    isc (1,2) = 0
	    ioff(1,2) = 0
	    ibit(1,2) = 0
          ELSE IF ( iflsrc .lt. 100 ) THEN
C
C*	    Save only the real data.
C
	    prtnam(1) = 'SFDT'
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
     +                ifhtyp, nrow, nrkeys, krowhd, ncol, nckeys, 
     +	              kcolhd, nprt, prtnam, lenhdr, ityprt, nparms, 
     +	              maxprm, prmnam, isc, ioff, ibit, isffln,
     +                iret)
C
C*	Write error message.
C
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DM', iret, filnam, ier )
	    iret = -1
	    CALL ER_WMSG  ( 'SF', iret, filnam, ier )
	    RETURN
	  ELSE
C
C*	    Write row header so it is not empty.
C
	    irowhd = 1
	    irow   = 1
	    CALL DM_WRWH  ( isffln, irow, irowhd, ipos, ier )
C
C*	    Load common areas.
C
	    isfcfn (isffln) = isffln
	    dttype (isffln) = 'COL'
	    sttype (isffln) = 'COL'
	    kdate  (isffln) = 1
	    ktime  (isffln) = 2
	    kstid  (isffln) = 3
	    kstnm  (isffln) = 4
	    kslat  (isffln) = 5
	    kslon  (isffln) = 6
	    kselv  (isffln) = 7
	    kstat  (isffln) = 8
	    kcoun  (isffln) = 9
	    kstd2  (isffln) = 10
	    kspri  (isffln) = 11
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
