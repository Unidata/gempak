        SUBROUTINE NAMRDD ( lunin, tblde, nde, degree,
     +			    sncfac, sfcfac, nprmsn, nprmsf,
     +                      parmsn, parmsf, snbprm, npbfr,
     +			    nprms2, prmsf2, sfcfc2, istbyt, dattim,
     +			    stid, istnm, slat, slon, selv, sndata,
     +			    sfdata, sfdat2, iclass, nz, ifhr, iret )
C************************************************************************
C* NAMRDD								*
C*									*
C* This subroutine reads the next record from a model profile dataset.	*
C* The first six sounding parameters on output are PRES, TMPC, DWPC,	*
C* SPED, DRCT, HGHT.  Since HGHT is computed, there is one additional	*
C* sounding parameter in the output array.				*
C*									*
C* SNBPRM contains the list of parms actually found in the BUFR data	*
C* in the order that they are present.  These are used to trigger	*
C* parameter conversions for a standardized output.  			*
C*									*
C* Note:  The number of levels returned is the number of levels above	*
C*	  the ground, i.e., levels on which the pressure is less than	*
C*	  the surface pressure.						*
C*									*
C* If an auxiliary surface data file is used, it contains any remaining *
C* primary output data followed by the diagnosed surface values.  Any	*
C* diagnosed parameter destined for the primary file must also be in	*
C* the auxiliary file.							*
C*									*
C* NAMRDD  ( LUNIN, TBLDE, NDE, DEGREE, SNCFAC, SFCFAC, NPRMSN, NPRMSF,	*
C*	     PARMSN, PARMSF, SNBPRM, NPBFR, NPRMS2, PRMSF2, SFCFC2,	*
C*	     ISTBYT, DATTIM, STID, ISTNM, SLAT, SLON, SELV,		*
C*	     SNDATA, SFDATA, SFDAT2, ICLASS, NZ, IFHR, IRET )		*
C*									*
C* Input parameters:							*
C*	LUNIN		INTEGER		Model input file unit number	*
C*	TBLDE(*)	CHAR*		Table D entry list		*
C*	NDE		INTEGER		# of Table D entries		*
C*	DEGREE		LOGICAL		Flag for stn lat/lon in degrees *
C*	SNCFAC (MMPARM) REAL		Profile parm conversion factors *
C*	SFCFAC (MMPARM) REAL		Surface parm conversion factors *
C*	NPRMSN 		INTEGER		Number of profile parms		*
C*	NPRMSF		INTEGER		Number of surface parms		*
C*	PARMSN(*)	CHAR*4		Sounding parameter list		*
C*	PARMSF(*)	CHAR*4		Surface parameter list		*
C*	SNBPRM (NPBFR)	CHAR*4		BUFR profile parm list		*
C*	NPBFR		INTEGER		Number of BUFR profile parms	*
C*	NPRMS2		INTEGER		Number of auxiliary sfc parms	*
C*	PRMSF2 (NPRMS2) CHAR*4		Auxiliary sfc parameter list	*
C*	SFCFC2 (MMPARM) REAL		Overflow sfc parm conv factors	*
C*									*
C* Input and Output parameter:						*
C*	ISTBYT		INTEGER		Starting byte in input file	*
C*					  Set ISTBYT = 1 on first call	*
C*	DATTIM		CHAR*		Date/time			*
C*									*
C* Output parameters:							*
C*	STID		CHAR*		Station ID			*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	SNDATA (*)	REAL		Array of sounding data		*
C*	SFDATA (*)	REAL		Array of surface data		*
C*	SFDAT2 (*)	REAL		Array of auxiliary sfc data	*
C*	ICLASS		INTEGER		Class of profile data		*
C*	NZ		INTEGER		Number of levels in sounding	*
C*	IFHR		INTEGER		Forecast hour of profile	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 +1 = End of input file		*
C*					-12 = Error reading input file	*
C*					-14 = Incorrect snd parm count	*
C*					-15 = Incorrect sfc parm count	*
C*					-27 = No data at station	*
C**									*
C* Log:									*
C* K. Brill/NMC		12/93						*
C* K. Brill/NMC		 3/94	Change WNUM to WSYM numbers		*
C* K. Brill/NMC		 3/94   Added first lvl T,Td,S,D as sfc data	*
C* K. Brill/NMC		 5/94	Change for reading BUFR file		*
C* K. Brill/NMC		 7/94	Added SNBPRM				*
C* K. Brill/NMC          8/94   Save deltat; Fix SWBL; missing sfc data	*
C* K. Brill/NMC		 8/94	Convert sfc temps to C			*
C* K. Brill/NMC		 9/94	Soil parms missing over water; fix SWBL *
C* K. Brill/NMC		 9/94	Set CFRL and CWTR to missing for now	*
C* K. Brill/NMC		10/94	Set BFGR to missing			*
C* K. Brill/NMC		10/94	Convert TMPC & T2MS to C		*
C* K. Brill/NMC		10/94	Set WSYM=999 for no precip		*
C* K. Brill/NMC		10/94	Changes for auxiliary sfc file		*
C* K. Brill/NMC		10/94	Added TD2M				*
C* K. Brill/NMC		 1/95	IRET = 2 for no data at station		*
C* K. Brill/NMC		 1/95   Set CFRL and CWTR to NOT missing	*
C* K. Brill/NMC		 4/96	Let AUX file contain primary parms	*
C* K. Brill/EMC		 7/96	Make sure wtrpnt is never true.		*
C* K. Brill/EMC		 7/98	Changes for using J. Woollen's decoder	*
C* D. Kidwell/NCEP	12/98	SNM... -> NAM..., SNMODL -> NAMSND      *
C* K. Brill/HPC		 5/99   Changes for STID; assign missing data	*
C*				in SFDAT2 for aux file parms		*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	PARAMETER	( MAXSND = LLMXDT, MXPROF = 8 )
	PARAMETER	( MAXSFC = 4*MMPARM )
C*
	CHARACTER*(*)	parmsn (*), parmsf (*), snbprm (*), prmsf2 (*),
     +			tblde (*), stid, dattim
	LOGICAL		degree
	REAL		sndata (*), sfdata (*), sfdat2 (*)
	REAL		sncfac (*), sfcfac (*), sfcfc2 (*)
C*
	REAL		snbufr ( MAXSND ), sfbufr ( MAXSFC )
	INTEGER		iday (3), npsnx (MXPROF), nzx (MXPROF)
C*
	LOGICAL		equal, wtrpnt, srch
	CHARACTER*32	errstr
	CHARACTER*4	prmtmp (MMPARM)
	REAL		slmlst (1024), swmlst (1024)
	REAL		scrtch (MAXSND, MXPROF)
	SAVE		deltat, slmlst, swmlst, ifctlt, istndx
	INCLUDE		'ERMISS.FNC'
	DATA		ifctlt /-9/
C*
C----------------------------------------------------------------------
	iret = 0
C
C*	Read in the next profile data set.
C
	CALL NAMRBF ( lunin, MAXSND, MXPROF, MAXSFC, tblde, nde,
     +		      degree, ihr, iday, ifct, istnm, stid, slat,
     +                slon, selv, npsnx, nzx, nprofs,  npsf, scrtch,
     +		      sfbufr, iclass, ier )
	IF ( ier .eq. 1 .or. ier .eq. -12 ) THEN
	    iret = ier
	    RETURN
	END IF
	IF ( ier .ne. 0 ) THEN
	    iret = ier
	    errstr = ' ERROR in NAMRBF'
	    CALL ER_WMSG ( 'NAMSND', iret, errstr, ier ) 
	    RETURN
	END IF
	IF ( npsf .eq. 0 ) THEN
	    iret = -27
	    RETURN
	END IF
C
C*	Merge the sounding data into snbufr or select the
C*	sounding set to use and load snbufr.
C
C*	Note:  If two profile parts should have the same number
C*	       of parameters, only the first can be selected.
C*	       Specify all the parameters to get the second part.
C
	kpart = 0
	ksum = 0
	equal = .true.
	DO kprf = 1, nprofs
	    IF ( npsnx (kprf) .eq. npbfr ) kpart = kprf
	    ksum = ksum + npsnx (kprf)
	    IF ( kprf .gt. 1 ) THEN
		equal = equal .and. nzx (kprf) .eq. nzx (kprf-1)
	    END IF
	END DO
	npsn = ksum
	IF ( kpart .eq. 0 .and. ( .not. equal .or. ksum .ne. npbfr )
     +     ) THEN
	    iret = -35
	    RETURN
	END IF
	IF ( kpart .eq. 0 ) THEN
	    ip1 = 1
	    ip2 = nprofs
	    nz = nzx (1)
	ELSE
	    ip1 = kpart
	    ip2 = kpart
	    nz = nzx (kpart)
	END IF
	ii = 0
	DO k = 1, nz
	    DO ip = ip1, ip2
		npar1 = npsnx (ip) * ( k - 1 ) + 1
		npar2 = npar1 + npsnx (ip) - 1
		DO ipar = npar1, npar2
		    ii = ii + 1
		    snbufr (ii) = scrtch (ipar,ip)
		END DO
	    END DO
	END DO
C
C*	Check for new forecast time.  Set station counter index.
C
	IF ( ifctlt .ne. ifct ) THEN
	    CALL NAMCDT ( ihr, iday, ifct, dattim, ifhr, deltat, ier )
	    ifctlt = ifct
	    istndx = 1
	ELSE
	    istndx = istndx + 1
	END IF
C
C*	Check for correct sounding parameter count.
C
	IF ( npsn .gt. nprmsn .and. nprmsn .ne. 0 ) THEN
	    iret = -14
	    RETURN
	END IF
C
C*	Apply conversion factor to sounding data.
C
	CALL NAMLSD ( snbufr, nz, npsn, sncfac, sndata, ier )
C
C*	Load the surface data. 
C
	itp = 0
	icp = 0
	ifxlp = 0
	ifxlh = 0
	ifxsh = 0
	ifxss = 0
	ifxsn = 0
	iswrd = 0
	iswru = 0
	ilwrd = 0
	ilwru = 0
	islmm = 0
	iswem = 0
	in01m = 0
	ir01m = 0
	iq2ms = 0
	wsym =  999
	ipcnt = 0
	wtrpnt = .false.
	IF ( npsf .gt. 40 ) THEN
	    npstop = 40
	ELSE
	    npstop = npsf
	END IF
	DO i = 1, npstop
	    ipcnt = ipcnt + 1
C
C*	    Check for location of 2m specific humidity.
C
	    IF ( parmsf (ipcnt) .eq. 'Q2MS' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) iq2ms = ipcnt
C
C*	    Check for location of convective and total precip.
C
	    IF ( parmsf (ipcnt) .eq. 'P01M' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) itp = ipcnt
	    IF ( parmsf (ipcnt) .eq. 'C01M' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) icp = ipcnt
C
C*	    Check for location of surface fluxes.
C
	    IF ( parmsf (ipcnt) .eq. 'FXLP'.and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ifxlp = ipcnt
	    IF ( parmsf (ipcnt) .eq. 'FXLH' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ifxlh = ipcnt
	    IF ( parmsf (ipcnt) .eq. 'FXSH' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ifxsh = ipcnt
	    IF ( parmsf (ipcnt) .eq. 'FXSS' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ifxss = ipcnt
	    IF ( parmsf (ipcnt) .eq. 'FXSN' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ifxsn = ipcnt
	    IF ( parmsf (ipcnt) .eq. 'SWRD' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) iswrd = ipcnt
	    IF ( parmsf (ipcnt) .eq. 'SWRU' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) iswru = ipcnt
	    IF ( parmsf (ipcnt) .eq. 'LWRD' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ilwrd = ipcnt
	    IF ( parmsf (ipcnt) .eq. 'LWRU' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ilwru = ipcnt
C
C*	    Check for location of moisture budget terms.
C
	    IF ( parmsf (ipcnt) .eq. 'SLMM' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) islmm = ipcnt
	    IF ( parmsf (ipcnt) .eq. 'SWEM' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) iswem = ipcnt
	    IF ( parmsf (ipcnt) .eq. 'N01M' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) in01m = ipcnt
	    IF ( parmsf (ipcnt) .eq. 'R01M' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ir01m = ipcnt
	    ip01m = itp
C
C*	    Check all categorical precip types to assign a
C*	    nonzero value to WSYM.
C
	    IF ( parmsf (ipcnt ) .eq. 'WXTS' .and.
     +		 sfbufr (i) .eq. 1 ) wsym = 70
	    IF ( parmsf (ipcnt ) .eq. 'WXTP' .and.
     +		 sfbufr (i) .eq. 1 ) wsym = 79
	    IF ( parmsf (ipcnt ) .eq. 'WXTZ' .and.
     +		 sfbufr (i) .eq. 1 ) wsym = 66
	    IF ( parmsf (ipcnt ) .eq. 'WXTR' .and.
     +		 sfbufr (i) .eq. 1 ) wsym = 60
C
C*	    Assign value of surface parm to SFDATA.
C
     	    IF ( .not. ERMISS ( sfbufr (i) ) ) THEN
     		sfdata (ipcnt) = sfbufr (i) * sfcfac (ipcnt)
C
C*		Do parameter conversions.
C
		IF ( parmsf (ipcnt) .eq. 'SKTC' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
		IF ( parmsf (ipcnt) .eq. 'SLTC' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
		IF ( parmsf (ipcnt) .eq. 'SBTC' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
		IF ( parmsf (ipcnt) .eq. 'TMIN' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
		IF ( parmsf (ipcnt) .eq. 'TMAX' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
		IF ( parmsf (ipcnt) .eq. 'T2MS' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
		IF ( parmsf (ipcnt) .eq. 'TMPC' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
	    ELSE
		sfdata (ipcnt) = sfbufr (i)
	    END IF
	    IF ( parmsf (ipcnt) .eq. 'PRES' ) THEN
     		psfc = sfdata (ipcnt)
	    END IF
C--7/24/96	    IF ( parmsf (ipcnt) .eq. 'SLMM' .and.
C--7/24/96    +		 ERMISS ( sfbufr (i) ) ) wtrpnt = .true.
	END DO
C
C*	If the point is over water set soil parameters to 
C*	missing.
C
	IF ( wtrpnt ) THEN
	    DO i = 1, npstop
		IF ( parmsf (i) .eq. 'SLTC' ) sfdata (i) = RMISSD
		IF ( parmsf (i) .eq. 'SBTC' ) sfdata (i) = RMISSD
		IF ( parmsf (i) .eq. 'SWEM' ) THEN
		    sfdata (i) = RMISSD
		    iswem = 0
		END IF
		IF ( parmsf (i) .eq. 'FXSS' ) THEN
		    sfdata (i) = RMISSD
		    ifxss = 0
		END IF
		IF ( parmsf (i) .eq. 'FXSN' ) THEN
		    sfdata (i) = RMISSD
		    ifxsn = 0
		END IF
		IF ( parmsf (i) .eq. 'N01M' ) THEN
		    sfdata (i) = RMISSD
		    in01m = 0
		END IF
		IF ( parmsf (i) .eq. 'R01M' ) THEN
		    sfdata (i) = RMISSD
		    ir01m = 0
		END IF
		IF ( parmsf (i) .eq. 'BFGR' ) sfdata (i) = RMISSD
	    END DO
	END IF
C
C*	Parameters in excess of 40 are destined for the AUX file.
C
	npsf2 = 0
	DO i = npstop+1, npsf
	    npsf2 = npsf2 + 1
	    ipcnt = ipcnt + 1
C
C*	    Check for location of 2m specific humidity.
C
	    IF ( prmsf2 (npsf2) .eq. 'Q2MS' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) iq2ms = ipcnt
C
C*	    Check for location of convective and total precip.
C
	    IF ( prmsf2 (npsf2) .eq. 'P01M' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) itp = ipcnt
	    IF ( prmsf2 (npsf2) .eq. 'C01M' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) icp = ipcnt
C
C*	    Check for location of surface fluxes.
C
	    IF ( prmsf2 (npsf2) .eq. 'FXLP'.and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ifxlp = ipcnt
	    IF ( prmsf2 (npsf2) .eq. 'FXLH' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ifxlh = ipcnt
	    IF ( prmsf2 (npsf2) .eq. 'FXSH' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ifxsh = ipcnt
	    IF ( prmsf2 (npsf2) .eq. 'FXSS' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ifxss = ipcnt
	    IF ( prmsf2 (npsf2) .eq. 'FXSN' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ifxsn = ipcnt
	    IF ( prmsf2 (npsf2) .eq. 'SWRD' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) iswrd = ipcnt
	    IF ( prmsf2 (npsf2) .eq. 'SWRU' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) iswru = ipcnt
	    IF ( prmsf2 (npsf2) .eq. 'LWRD' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ilwrd = ipcnt
	    IF ( prmsf2 (npsf2) .eq. 'LWRU' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ilwru = ipcnt
C
C*	    Check for location of moisture budget terms.
C
	    IF ( prmsf2 (npsf2) .eq. 'SLMM' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) islmm = ipcnt
	    IF ( prmsf2 (npsf2) .eq. 'SWEM' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) iswem = ipcnt
	    IF ( prmsf2 (npsf2) .eq. 'N01M' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) in01m = ipcnt
	    IF ( prmsf2 (npsf2) .eq. 'R01M' .and.
     +		 .not. ERMISS ( sfbufr (i) ) ) ir01m = ipcnt
	    ip01m = itp
C
C*	    Check all categorical precip types to assign a
C*	    nonzero value to WSYM.
C
	    IF ( prmsf2 (npsf2 ) .eq. 'WXTS' .and.
     +		 sfbufr (i) .eq. 1 ) wsym = 70
	    IF ( prmsf2 (npsf2 ) .eq. 'WXTP' .and.
     +		 sfbufr (i) .eq. 1 ) wsym = 79
	    IF ( prmsf2 (npsf2 ) .eq. 'WXTZ' .and.
     +		 sfbufr (i) .eq. 1 ) wsym = 66
	    IF ( prmsf2 (npsf2 ) .eq. 'WXTR' .and.
     +		 sfbufr (i) .eq. 1 ) wsym = 60
C
C*	    Assign value of surface parm to SFDATA.
C
     	    IF ( .not. ERMISS ( sfbufr (i) ) ) THEN
     		sfdata (ipcnt) = sfbufr (i) * sfcfc2 (npsf2)
		sfdat2 (npsf2) = sfdata (ipcnt)
C
C*		Do parameter conversions.
C
		IF ( prmsf2 (npsf2) .eq. 'SKTC' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
		IF ( prmsf2 (npsf2) .eq. 'SLTC' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
		IF ( prmsf2 (npsf2) .eq. 'SBTC' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
		IF ( prmsf2 (npsf2) .eq. 'TMIN' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
		IF ( prmsf2 (npsf2) .eq. 'TMAX' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
		IF ( prmsf2 (npsf2) .eq. 'T2MS' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
		IF ( prmsf2 (npsf2) .eq. 'TMPC' )
     +		    sfdata (ipcnt) = sfdata (ipcnt) - 273.16
	    ELSE
		sfdata (ipcnt) = sfbufr (i)
		sfdat2 (npsf2) = sfdata (ipcnt)
	    END IF
	    IF ( prmsf2 (npsf2) .eq. 'PRES' ) THEN
     		psfc = sfdata (ipcnt)
	    END IF
C--7/24/96	    IF ( prmsf2 (npsf2) .eq. 'SLMM' .and.
C--7/24/96     +	 ERMISS ( sfbufr (i) ) ) wtrpnt = .true.
	END DO
C
C*	If the point is over water set soil parameters to 
C*	missing.
C
	IF ( wtrpnt ) THEN
	    DO j = npstop+1, npsf
		i = j - npstop
		IF ( prmsf2 (i) .eq. 'SLTC' ) sfdata (j) = RMISSD
		IF ( prmsf2 (i) .eq. 'SBTC' ) sfdata (j) = RMISSD
		IF ( prmsf2 (i) .eq. 'SWEM' ) THEN
		    sfdata (j) = RMISSD
		    iswem = 0
		END IF
		IF ( prmsf2 (i) .eq. 'FXSS' ) THEN
		    sfdata (j) = RMISSD
		    ifxss = 0
		END IF
		IF ( prmsf2 (i) .eq. 'FXSN' ) THEN
		    sfdata (j) = RMISSD
		    ifxsn = 0
		END IF
		IF ( prmsf2 (i) .eq. 'N01M' ) THEN
		    sfdata (j) = RMISSD
		    in01m = 0
		END IF
		IF ( prmsf2 (i) .eq. 'R01M' ) THEN
		    sfdata (j) = RMISSD
		    ir01m = 0
		END IF
		IF ( prmsf2 (i) .eq. 'BFGR' ) sfdata (j) = RMISSD
	    END DO
	END IF
C
C*	Use the now exhausted SNBUFR array as temporary storage
C*	to compute requested GEMPAK sounding parameters for the
C*	output parameters.  First level data is saved for
C*	"surface" data.
C
	nnn = nz * npsn
	DO i = 1, nnn
	    snbufr (i) = sndata (i)
	END DO
	io = 0
	tmpcsf = RMISSD
	DO k = 1, nz
	    prs = RMISSD
	    tmpc = RMISSD
	    r = RMISSD
	    u = RMISSD
	    v = RMISSD
	    rsw = RMISSD
	    rlw = RMISSD
	    DO ip = 1, npsn
		in = ip + ( k - 1 ) * npsn
		IF ( snbprm (ip) .eq. 'PRES' ) THEN
		    prs = snbufr (in)
		ELSE IF ( snbprm (ip) .eq. 'TMPK' ) THEN
		    IF ( .not. ERMISS ( snbufr (in) ) ) THEN
     			tmpc = snbufr (in) - 273.16
		    ELSE
			tmpc = RMISSD
		    END IF
		ELSE IF ( snbprm (ip) .eq. 'SPFH' ) THEN
		    IF ( .not. ERMISS ( snbufr (in) ) ) THEN
		        r = 1000. * snbufr (in) / ( 1. - snbufr (in) )
		    ELSE
			r = RMISSD
		    END IF
		ELSE IF ( snbprm (ip) .eq. 'UWND' ) THEN
		    u = snbufr (in)
		ELSE IF ( snbprm (ip) .eq. 'VWND' ) THEN
		    v = snbufr (in)
		ELSE IF ( snbprm (ip) .eq. 'DTSW' ) THEN
		    rsw = snbufr (in)
		ELSE IF ( snbprm (ip) .eq. 'DTLW' ) THEN
		    rlw = snbufr (in)
		END IF
	    END DO
	    dwpt = PR_DWPT ( r, prs )
	    dir = PR_DRCT ( u, v )
	    spd = PR_SPED ( u, v )
	    IF ( .not. ERMISS ( rsw ) .and.
     +		 .not. ERMISS ( rlw ) ) THEN
		dtar = rsw + rlw
	    ELSE
		dtar = RMISSD
	    END IF
C
C*	    Build height.
C
	    IF ( k .eq. 1 ) THEN
		dwptsf = dwpt
		drctsf = dir
		spedsf = spd
		tmpcsf = tmpc
C*
		tb = tmpc
		tt = tmpc
		tdb = dwpt
		tdt = dwpt
		pb = psfc
		pt = prs
		scl = PR_SCLH ( tb, tt, tdb, tdt, pb, pt )
		zz = PR_MHGT ( selv, pb, pt, scl )
		tb = tmpc
		tdb = dwpt
		pb = pt
	    ELSE
		tt = tmpc
		tdt = dwpt
		pt = prs
		scl = PR_SCLH ( tb, tt, tdb, tdt, pb, pt )
		zz = PR_MHGT ( zz, pb, pt, scl )
		tb = tt
		tdb = tdt
		pb = pt
	    END IF			
C
C*	    Now loop over the output parameters.
C
	    DO ip = 1, nprmsn
		io = io + 1
		sndata (io) = RMISSD
		IF ( parmsn (ip) .eq. 'PRES' ) THEN
		    sndata (io) = prs
		ELSE IF ( parmsn (ip) .eq. 'TMPC' ) THEN
		    sndata (io) = tmpc
		ELSE IF ( parmsn (ip) .eq. 'DWPC' ) THEN
		    sndata (io) = dwpt
		ELSE IF ( parmsn (ip) .eq. 'SPED' ) THEN
		    sndata (io) = spd
		ELSE IF ( parmsn (ip) .eq. 'DRCT' ) THEN
		    sndata (io) = dir
		ELSE IF ( parmsn (ip) .eq. 'HGHT' ) THEN
		    sndata (io) = zz
		ELSE IF ( parmsn (ip) .eq. 'DTAR' ) THEN
		    sndata (io) = dtar
C
C******		ELSE IF ( parmsn (ip) .eq. 'CFRL' ) THEN
C******		    sndata (io) = RMISSD
C******		ELSE IF ( parmsn (ip) .eq. 'CWTR' ) THEN
C******		    sndata (io) = RMISSD
C
		ELSE
		    DO iq = 1, npsn
			IF ( parmsn (ip) .eq. snbprm (iq) ) THEN
			    in = iq + ( k - 1 ) * npsn
			    sndata (io) = snbufr (in)
			END IF
		    END DO
		END IF
	    END DO
	END DO
C
C*	Compute derived sfc parameters.  These parameter names
C*	trail the dataset parameters in the surface parm list.
C
	IF ( npsf .lt. nprmsf .and. nprms2 .eq. 0 ) THEN
C
C*	    All diagnosed parms are destined for primary 
C*	    output file; so, load PRMSF2 with remaining names.
C
	    indx = 0
	    DO i = npsf+1, nprmsf
		indx = indx + 1
		prmtmp (indx) = parmsf (i)
	    END DO
	    istop = indx
C*
	ELSE IF ( nprms2 .ne. 0 ) THEN
C
C*	    Diagnosed parms are destined for auxiliary file.
C
	    istop = nprms2
	    DO i = 1, istop
		prmtmp (i) = prmsf2 (i)
	    END DO
	ELSE
	    istop = 0
	END IF
C*
	IF ( istop .ne. 0 ) THEN
	  DO i = npsf2+1, istop
	    sfdat2 (i) = RMISSD
C
C*	    Add first level sounding data as surface free
C*	    atmospheric values.
C
	    IF ( prmtmp (i) .eq. 'TMPC' ) THEN
		sfdat2 (i) = tmpcsf
	    ELSE IF ( prmtmp (i) .eq. 'DWPC' .or.
     +		      prmtmp (i) .eq. 'DWPT' ) THEN
		sfdat2 (i) = dwptsf
	    ELSE IF ( prmtmp (i) .eq. 'SPED' ) THEN
		sfdat2 (i) = spedsf
	    ELSE IF ( prmtmp (i) .eq. 'DRCT' ) THEN
		sfdat2 (i) = drctsf
	    END IF
C
C*	    ESTIMATE 2 m dewpoint temperature.
C
	    IF ( prmtmp (i) .eq. 'TD2M' .and.
     +		 iq2ms .ne. 0 ) THEN
		sfdat2 (i) = PR_DWPT ( sfdata (iq2ms), psfc )
	    ELSE IF ( prmtmp (i) .eq. 'TD2M' ) THEN
		sfdat2 (i) = RMISSD
	    END IF
C
C*	    If the next parameter is stable precipitation, it
C*	    must be computed from two preceding values.
C
	    IF ( prmtmp (i) .eq. 'S01M' .and.
     +	   	 itp * icp .ne. 0 ) THEN
                sfdat2 (i) = sfdata (itp) - sfdata (icp)
	    ELSE IF ( prmtmp (i) .eq. 'S01M' ) THEN
		sfdat2 (i) = RMISSD
	    END IF
C
C*	    If WSYM is in the parameter list, assign it.
C
	    IF ( prmtmp (i) .eq. 'WSYM' ) THEN
		sfdat2 (i) = wsym
	    END IF
C
C*	    IF FXTT is in the parameter list, compute the total
C*	    residual energy flux at the surface.
C
	    IF ( prmtmp (i) .eq. 'FXTT' .and. ifxlh .ne. 0 .and.
     +		 ifxsh .ne. 0 .and. ifxss .ne. 0 .and.
     +		 ifxsn .ne. 0 .and. iswrd .ne. 0 .and.
     +		 iswru .ne. 0 .and. ilwrd .ne. 0 .and.
     +		 ilwru .ne. 0 )  THEN
		sfdat2 (i) = sfdata (ifxlh) + sfdata (ifxsh) +
     +				 sfdata (ifxss) + sfdata (ifxsn) +
     +				 sfdata (iswrd) + sfdata (iswru) +
     +				 sfdata (ilwrd) + sfdata (ilwru)
	    ELSE IF ( prmtmp (i) .eq. 'FXTT' ) THEN
		sfdat2 (i) = RMISSD
	    END IF
C
C*	    IF RNET is in the parameter list, compute the net
C*	    radiative energy flux at the surface.
C
	    IF ( prmtmp (i) .eq. 'RNET' .and. iswrd .ne. 0 .and.
     +		 iswru .ne. 0 .and. ilwrd .ne. 0 .and.
     +		 ilwru .ne. 0 )  THEN
		sfdat2 (i) = sfdata (iswrd) + sfdata (iswru) +
     +				 sfdata (ilwrd) + sfdata (ilwru)
	    ELSE IF ( prmtmp (i) .eq. 'RNET' ) THEN
		sfdat2 (i) = RMISSD
	    END IF
C
C*	    IF NDRF is in the parameter list, compute the net
C*	    downward radiative energy flux at the surface.
C
	    IF ( prmtmp (i) .eq. 'NDRF' .and. iswrd .ne. 0 .and.
     +		 iswru .ne. 0 .and. ilwrd .ne. 0 ) THEN
		sfdat2 (i) = sfdata (iswrd) + sfdata (iswru) +
     +				 sfdata (ilwrd)
	    ELSE IF ( prmtmp (i) .eq. 'NDRF' ) THEN
		sfdat2 (i) = RMISSD
	    END IF
C
C*	    IF SLLP is in the parameter list, compute it from FXLP.
C
	    IF ( prmtmp (i) .eq. 'SLLP' .and. ifxlp .ne. 0 .and.
     +		 iswem .ne. 0 ) THEN
		IF ( sfdata (iswem) .gt. 0. ) THEN
		    heatvp = 2.834E06
		ELSE
		    heatvp = 2.500E06
		END IF
		sfdat2 (i) = deltat * sfdata (ifxlp) / heatvp
	    ELSE IF ( prmtmp (i) .eq. 'SLLP' ) THEN
		sfdat2 (i) = RMISSD
	    END IF
C
C*	    IF SLLH is in the parameter list, compute it from FXLH.
C
	    IF ( prmtmp (i) .eq. 'SLLH' .and. ifxlh .ne. 0 .and.
     +		 iswem .ne. 0 ) THEN
		IF ( sfdata (iswem) .gt. 0. ) THEN
		    heatvp = 2.834E06
		ELSE
		    heatvp = 2.500E06
		END IF
		sfdat2 (i) = deltat * sfdata (ifxlh) / heatvp
	    ELSE IF ( prmtmp (i) .eq. 'SLLH' ) THEN
		sfdat2 (i) = RMISSD
	    END IF
C
C*	    IF  SWBL is in the parameter list, compute the total
C*	    water budget.
C
	    IF ( prmtmp (i) .eq. 'SWBL' .and. islmm .ne. 0 .and.
     +		 ip01m .ne. 0 .and. iswem .ne. 0 .and.
     +		 ir01m .ne. 0 .and. ifxlh .ne. 0 ) THEN
		IF ( ifct .eq. 0 ) THEN
		    term1 = 0.
		    term2 = 0.
		    slmlst (istndx) = sfdata (islmm)
		    swmlst (istndx) = sfdata (iswem)
		ELSE
		    term1 = sfdata (islmm) - slmlst (istndx)
		    slmlst (istndx) = sfdata (islmm)
		    term2 = sfdata (iswem) - swmlst (istndx)
		    swmlst (istndx) = sfdata (iswem)
		END IF
		IF ( sfdata (iswem) .gt. 0. ) THEN
		    heatvp = 2.834E06
		ELSE
		    heatvp = 2.500E06
		END IF
		sllhx = deltat * sfdata (ifxlh) / heatvp
		sfdat2 (i) = -term1 - term2 + sfdata (ip01m) -
     +				 sfdata (ir01m) + sllhx
	    ELSE IF ( prmtmp (i) .eq. 'SWBL' ) THEN
		sfdat2 (i) = RMISSD
	    END IF
	  END DO
	END IF
C
C*	Load parameters that are in primary file.
C
	ipcnt = npstop
	IF ( npsf .lt. nprmsf ) THEN
	    DO i = npsf+1, nprmsf
		jp = 0
		srch = .true.
		DO WHILE ( jp .lt. istop .and. srch )
		    jp = jp + 1
		    IF ( prmtmp (jp) .eq. parmsf (i) ) THEN
			ipcnt = ipcnt + 1
			sfdata (ipcnt) = sfdat2 (jp)
			srch = .false.
		    END IF
		END DO
		IF ( srch ) THEN
		    ipcnt = ipcnt + 1
		    sfdata (ipcnt) = RMISSD
		END IF
	    END DO
	END IF
C
C*	Check for correct count of surface parameters.
C
	IF ( ipcnt .ne. nprmsf .and. nprmsf .ne. 0 ) THEN
	    iret = -15
	    RETURN
	END IF
C*
	RETURN
	END
