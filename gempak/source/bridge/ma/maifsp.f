	SUBROUTINE MA_IFSP  ( rimnem, cimnem, iret )
C************************************************************************
C* MA_IFSP								*
C*									*
C* This subroutine initializes the interface mnemonic arrays and sets   *
C* the pointers within COMMON / RINTFP / and COMMON / CINTFP /.		*
C*									*
C* MA_IFSP  ( RIMNEM, CIMNEM, IRET )					*
C*									*
C* Output parameters:							*
C*	RIMNEM (*)	CHAR*		Interface mnemonics for reals   *
C*	CIMNEM (*)	CHAR*		Interface mnemonics for chars   *
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = One or more pointers	*
C*					      could not be set 		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/97	Adapted from AF_IFSP			*
C* R. Hollern/NCEP	12/97	Replaced MA_IFFP, MA_IFMP calls with DC_*
C* D. Kidwell/NCEP	10/98	Added init of interface mnemonics;      *
C*				added intf mnemonics to calling sequence*
C* R. Hollern/NCEP	 7/99	Added 24 more mnemonics                 *
C* R. Hollern/NCEP	 9/99	Added drifting buoy section 4 mnemonics *
C* C. Caruso Magee/NCEP	 4/01	Modified for Coast Guard reports	*
C* D. Kidwell/NCEP	 3/02	Corrected PRMN to PSMN                  *
C* D. Kidwell/NCEP	 4/05	Added PKWD, PKWS, PKWT, PMN1, PMNT      *
C************************************************************************
	INCLUDE		'macmn.cmn'
C*
	CHARACTER*(*)	rimnem (*), cimnem (*)
C*
	LOGICAL		allok
	INTEGER		iploc (NRSLMN)
	CHARACTER	rifmn ( NRIMN )*8, cifmn ( NCIMN )*8
C*
C*	Establish array equivalence to COMMON / RINTFP /
C*
	EQUIVALENCE	( iploc (1), iryear )
C*
	DATA	( rifmn (i), i = 1, 58 ) /
     +		'YEAR'  , 'MNTH'  , 'DAYS'  , 'HOUR'  , 'MINU'  ,
     +		'SLAT'  , 'SLON'  , 'SELV'  , 
     +		'DRCT'  , 'SPED'  , 'GUM0'  , 'GUM2'  , 'GUM1'  ,
     +		'XS10'  , 'XS20'  , 'PRES'  , 'PMSL'  , 'P03D'  , 
     +		'CHPT'  , '3HPC'  , 'RELH'  , 'MSST'  , 'STMWC' ,
     +		'TMPC'  , 'DWPC'  , 'SSTC'  , 'TMWC'  , 'DTFV1' ,
     +		'MXTM'  , 'DTFV2' , 'MITM'  , 'QOPM'  , 'QCBH'  ,
     +		'QWTM'  , 'QATM'  , 'QBST'  , 'QCIL'  , 'MSDM'  ,
     +		'PXXM'  , 'P06M'  , 'P12M'  , 'P18M'  , 'P24M'  ,
     +		'P01M'  , 'P02M'  , 'P03M'  , 'P09M'  , 'P15M'  ,
     +		'INPC'  , 'ITSO'  , 'TOST'  , 'CBAS'  , 'VSBK'  ,
     +		'CFRT'  , 'WWMO'  , 'WWMA'  , 'PWWM'  , 'PWWA'  /
C*
	DATA	( rifmn (i), i = 59, NRSLMN ) /
     +		'PSW2'  , 'CFRL'  , 'CTYL'  , 'CLHL'  , 'CTYM'  ,
     +		'CLHM'  , 'CTYH'  , 'CLHH'  , 'TDMP'  , 'ASMP'  ,
     +		'WPER'  , 'POWW'  , 'HOWW'  , 'COIA'  , 'IDTH'  ,
     + 		'ROIA'  , '24PC'  , 'PWYR'  , 'PWMO'  , 'PWDY'  ,
     +		'PWHR'  , 'PWMN'  , 'PWDR'  , 'PWSP'  , 'DTFVM' ,
     +		'GUMS'  , 'WHGT'  , 'CORN'  , 'BPID'  , 'SUWS'  ,
     +		'QPOS'  , 'QTIM'  ,
     +		'QCLS'  , 'QDS1'  , 'QXS1'  , 'QDS2'  , 'QXS2'  ,
     +		'Q3D1'  , 'Q3D2'  , 'Q4CL'  , 'PWA2'  , 'VRTM'  ,
     +		'PSYR'  , 'PSMN'  , 'PSDY'  , 'PSHR'  , 'PSMI'  ,
     +		'DBVV'  , 'DBDD'  , 'BENG'  , 'DROT'  , 'CALN'  , 
     +		'DLATH' , 'DLONH' , 'SKNT'  , 'ALTI'  , 'TMPF'  ,
     +		'SSTF'  , 'VSBY'  , 'MXWH'  , 'TERC'  , 'GUST'  , 
     +		'PKWD'  , 'PKWS'  , 'PKWT'  , 'PMN1'  , 'PMNT'  /
C*
	DATA	( rifmn (i), i = NRSLMN + 1, NRIMN ) /
     +		'NDTS'  , 'DBSS'  , 'STMP'  , 'SALN'  , 
     +                    MDM1T3 * ' ',
     +          'NCLO'  , 'VSSO'  , 'CLAM'  , 'CLTP'  , 'HOCB'  ,
     +		          MCM1T4 * ' ' ,
     +		'NCL3'  , 'VSSO3' , 'CLAM3' , 'CLTP3' , 'HOCB3' , 
     +                    M3M1T4 * ' ',
     +          'NCWD'  , 'TPMI'  , 'WDRC'  , 'WDSC'  ,
     +		          MWM1T3 * ' ',
     +          'NSWV'  , 'DOSW'  , 'POSW'  , 'HOSW'  ,
     +		          MSM1T3 * ' '                          /
C*					Real interface mnemonics 
C*
	DATA	( cifmn (i), i = 1, NCIMN ) 	/ 'STID' /
C*					Character interface mnemonics 
C-----------------------------------------------------------------------
	iret = 0
C
C*	Initialize the interface mnemonics.
C
	DO i = 1, NRIMN
	    rimnem ( i ) = rifmn ( i )
	END DO
	DO i = 1, NCIMN
	    cimnem ( i ) = cifmn ( i ) 
	END DO
C
C*	The logical variable "allok" is initially set to .true. but will
C*	be reset to .false. if any of the pointers cannot be set.
C
	allok = .true.
C
C*	Set the pointers for the single-level interface mnemonics.
C
	DO i = 1, NRSLMN
	    CALL DC_IFFP ( rimnem ( i ), rimnem, NRIMN, allok, 
     +                     iploc ( i ), ier )
	END DO
C
	CALL DC_IFFP  ( 'STID', cimnem, NCIMN, allok, icstid, ier )
C
C*	Set the pointers for depth/temperature/salinity data.
C
	CALL DC_IFFP  ( 'NDTS', rimnem, NRIMN, allok, irndts, ier )
	CALL DC_IFFP  ( 'DBSS', rimnem, NRIMN, allok, irdbss (1), ier )
	CALL DC_IFMP  ( 3, MXDLYR, irdbss, ier )
	CALL DC_IFFP  ( 'STMP', rimnem, NRIMN, allok, irstmp (1), ier )
	CALL DC_IFMP  ( 3, MXDLYR, irstmp, ier )
	CALL DC_IFFP  ( 'SALN', rimnem, NRIMN, allok, irsaln (1), ier )
	CALL DC_IFMP  ( 3, MXDLYR, irsaln, ier )
C
C*	Set the pointers for cloud data.
C
	CALL DC_IFFP  ( 'NCLO', rimnem, NRIMN, allok, irnclo, ier )
	CALL DC_IFFP  ( 'VSSO', rimnem, NRIMN, allok, irvsso (1), ier )
	CALL DC_IFMP  ( 4, MXCLYR, irvsso, ier )
	CALL DC_IFFP  ( 'CLAM', rimnem, NRIMN, allok, irclam (1), ier )
	CALL DC_IFMP  ( 4, MXCLYR, irclam, ier )
	CALL DC_IFFP  ( 'CLTP', rimnem, NRIMN, allok, ircltp (1), ier )
	CALL DC_IFMP  ( 4, MXCLYR, ircltp, ier )
	CALL DC_IFFP  ( 'HOCB', rimnem, NRIMN, allok, irhocb (1), ier )
	CALL DC_IFMP  ( 4, MXCLYR, irhocb, ier )
C
C*	Set the pointers for cloud3 data.
C
	CALL DC_IFFP  ( 'NCL3', rimnem, NRIMN, allok, irncl3, ier )
	CALL DC_IFFP  ( 'VSSO3', rimnem, NRIMN, allok, irvss3(1), ier )
	CALL DC_IFMP  ( 4, MX3LYR, irvss3, ier )
	CALL DC_IFFP  ( 'CLAM3', rimnem, NRIMN, allok, ircla3(1), ier )
	CALL DC_IFMP  ( 4, MX3LYR, ircla3, ier )
	CALL DC_IFFP  ( 'CLTP3', rimnem, NRIMN, allok, irclt3(1), ier )
	CALL DC_IFMP  ( 4, MX3LYR, irclt3, ier )
	CALL DC_IFFP  ( 'HOCB3', rimnem, NRIMN, allok, irhcb3(1), ier )
	CALL DC_IFMP  ( 4, MX3LYR, irhcb3, ier )
C
C*	Set the pointers for continuous wind data.
C
	CALL DC_IFFP  ( 'NCWD', rimnem, NRIMN, allok, irncwd, ier )
	CALL DC_IFFP  ( 'TPMI', rimnem, NRIMN, allok, irtpmi (1), ier )
	CALL DC_IFMP  ( 3, MXWLYR, irtpmi, ier )
	CALL DC_IFFP  ( 'WDRC', rimnem, NRIMN, allok, irwdrc (1), ier )
	CALL DC_IFMP  ( 3, MXWLYR, irwdrc, ier )
	CALL DC_IFFP  ( 'WDSC', rimnem, NRIMN, allok, irwdsc (1), ier )
	CALL DC_IFMP  ( 3, MXWLYR, irwdsc, ier )
C
C*	Set the pointers for wave data.
C
	CALL DC_IFFP  ( 'NSWV', rimnem, NRIMN, allok, irnswv, ier )
	CALL DC_IFFP  ( 'DOSW', rimnem, NRIMN, allok, irdosw (1), ier )
	CALL DC_IFMP  ( 3, MXSLYR, irdosw, ier )
	CALL DC_IFFP  ( 'POSW', rimnem, NRIMN, allok, irposw (1), ier )
	CALL DC_IFMP  ( 3, MXSLYR, irposw, ier )
	CALL DC_IFFP  ( 'HOSW', rimnem, NRIMN, allok, irhosw (1), ier )
	CALL DC_IFMP  ( 3, MXSLYR, irhosw, ier )
C
C*	Were all of the pointers properly set?
C
	IF  ( .not. allok ) THEN
	    iret = -1
	END IF
C*
	RETURN
	END
