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
C* this file.								*
C*									*
C* If the file cannot be created, error messages will be written.	*
C*									*
C* The data source values are parameters in GEMPRM.PRM .		*
C* These are not currently used by any GEMPAK program.  Current		*
C* definitions include:							*
C*      MFNONE        unknown						*
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
C* P.Bruehl/Unidata	 5/94	Modified for reduced # of header vars	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'sfcmn.cmn'
C*
	CHARACTER*(*) 	filnam, parms (*)
	INTEGER		iscale (*), iofset (*), ibits (*)
C*
	CHARACTER	kcolhd(5)*4, fhdnam*4, prtnam*4, krowhd*4
	LOGICAL 	pkflg, stmflg
C*
C	DATA		kcolhd / 'DATE', 'TIME', 'STID', 'STNM', 
C     +				 'SLAT', 'SLON', 'SELV', 'STAT', 
C     +				 'STAT'/
C	DATA		kcolhd / 'DATE', 'TIME', 'SLAT', 'SLON' /
	DATA		kcolhd / 'DATE', 'TIME', 'STID', 'SLAT', 'SLON' /
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
c	nckeys = 9
	nckeys = 5
	nprt   = 1
	prtnam = 'SFDT'
	IF (stmflg) THEN
	    lenhdr = 1
	  ELSE
	    lenhdr = 0
	END IF
	IF (pkflg) THEN
	    ityprt = MDRPCK
	  ELSE
	    ityprt = MDREAL
	END IF
	maxprm = nparm
C
C*	Create the file.
C
	CALL DM_CRET (filnam, iftype, iflsrc, nfhdrs, fhdnam, ifhlen,
     +                ifhtyp, nrow, nrkeys, krowhd, ncol, nckeys, 
     +	              kcolhd, nprt, prtnam, lenhdr, ityprt, nparm, 
     +	              maxprm, parms, iscale, iofset, ibits, isffln,
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
	    CALL DM_WRWH  ( isffln, irow, irowhd, iposition, ier )
C
C*	    Load common areas.
C
	    isfcfn (isffln) = isffln
	    dttype (isffln) = 'COL'
	    sttype (isffln) = 'COL'
	    kdate  (isffln) = 1
	    ktime  (isffln) = 2
	    kstid  (isffln) = 3
c	    kstnm  (isffln) = 4
	    kslat  (isffln) = 4
	    kslon  (isffln) = 5 
c	    kselv  (isffln) = 7
c	    kstat  (isffln) = 8
c	    kcoun  (isffln) = 9
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
