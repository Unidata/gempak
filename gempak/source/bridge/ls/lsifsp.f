        SUBROUTINE LS_IFSP ( rimnem, cimnem, iret )
C************************************************************************
C* LS_IFSP								*
C*									*
C* This subroutine initializes the interface mnemonic arrays and sets	*
C* the pointers within COMMON / RINTFP / and COMMON / CINTFP /.		*
C*									*
C* LS_IFSP  ( RIMNEM, CIMNEM, IRET )                                    *
C*									*
C* Output parameters:							*
C*      RIMNEM (*)      CHAR*		Interface mnemonics for reals	*
C*      CIMNEM (*)      CHAR*		Interface mnemonics for chars	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = Normal return 		*
C*					 -1 = One or more pointers	*
C*					      could not be set 		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/97	Adapted from AF_IFSP			*
C* R. Hollern/NCEP	 1/98   Changes based on MA_IFSP                *
C* A. Hardy/GSC          1/98   Added GEMINC                            *
C* D. Kidwell/NCEP	10/98   Added init of interface mnemonics;	*
C*                              added intf mnemonics to calling sequence*
C* R. Hollern/NCEP	 8/99   Added more mnemonics to rifmn array     *
C************************************************************************
	INCLUDE	 	'GEMPRM.PRM'
	INCLUDE		'lscmn.cmn'
C*
	CHARACTER*(*)   rimnem (*), cimnem (*)
C*
	LOGICAL		allok
	INTEGER		iploc (NRSLMN)
        CHARACTER       rifmn ( NRIMN )*8, cifmn ( NCIMN )*8
C*
C*	Establish array equivalence to COMMON / RINTFP /
C*
	EQUIVALENCE	( iploc (1), irwmob )
C*
C*      Real interface mnemonics.
C*
	DATA	( rifmn (i), i = 1, 65 ) /
     +		'WMOB'  , 'WMOS'  , 'SLAT'  , 'SLON'  , 'SELV'  ,
     +		'YEAR'  , 'MNTH'  , 'DAYS'  , 'HOUR'  , 'MINU'  ,
     +		'DRCT'  , 'SPED'  , 'GUM0'  , 'GUM2'  , 'GUM1'  ,
     +		'XS10'  , 'XS20'  , 'PRES'  , 'PMSL'  , 'P03D'  , 
     +		'CHPT'  , '3HPC'  , 'RELH'  , 'MSST'  , 'STMWC' ,
     +		'TMPC'  , 'DWPC'  , 'SSTC'  , 'TMWC'  , 'DTFV1' ,
     +		'MXTM'  , 'DTFV2' , 'MITM'  , 'HGTM'  , 'PRLC'  ,
     +		'PXXM'  , 'P06M'  , 'P12M'  , 'P18M'  , 'P24M'  ,
     +		'P01M'  , 'P02M'  , 'P03M'  , 'P09M'  , 'P15M'  ,
     +		'INPC'  , 'ITSO'  , 'TOST'  , 'CBAS'  , 'VSBK'  ,
     +		'CFRT'  , 'WWMO'  , 'WWMA'  , 'PWWM'  , 'PWWA'  ,
     +		'PWA2'  , 'PSW2'  , 'DOFS'  , 'DHFS' ,  'SNOWCM',
     +		'CTTP'  , 'CTMX'  , 'CTMN'  , 'CTP24' , 'SOGR'  /
	DATA	( rifmn (i), i = 66, 94 ) /
     +		'CFRL'  , 'CTYL'  , 'CLHL'  , 'CTYM'  ,  'CLHM' ,
     +		'CTYH'  , 'CLHH'  ,
     +		'WPER'  , 'POWW'  , 'HOWW'  , 'COIA'  , 'IDTH'  ,
     + 		'ROIA'  , '24PC'  , 'PWYR'  , 'PWMO'  , 'PWDY'  ,
     +		'PWHR'  , 'PWMN'  , 'PWDR'  , 'PWSP'  , 'DTFVM' ,
     +		'GUMS'  , 'WHGT'  , 'CORN'  , 'SUWS'  , 'VRTM'  ,
     +          'T12X'  , 'T12N'  				/
	DATA	( rifmn (i), i = 95, NRIMN ) /
     +          'NCLO'  , 'VSSO'  , 'CLAM'  , 'CLTP'  , 'HOCB'  ,
     +		          MCM1T4 * ' ' ,
     +		'NCL3'  , 'VSSO3' , 'CLAM3' , 'CLTP3' , 'HOCB3' , 
     +                    M3M1T4 * ' ',
     +		'NCL4'  , 'VSSO4' , 'CLAM4' , 'CLTP4' , 'HOCT4' , 
     +                    'CLDT4' , M4M1T4 * ' ',
     +          'NSWV'  , 'DOSW'  , 'POSW'  , 'HOSW'  ,
     +		          MSM1T3 * ' '                          /
C*
C*      Character interface mnemonics.
C*
        DATA    ( cifmn (i), i = 1, NCIMN )   / 'STID' /
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
C*	Set the pointers for cloud4 data.
C
	CALL DC_IFFP  ( 'NCL4', rimnem, NRIMN, allok, irncl4, ier )
	CALL DC_IFFP  ( 'VSSO4', rimnem, NRIMN, allok, irvss4(1), ier )
	CALL DC_IFMP  ( 5, MX4LYR, irvss4, ier )
	CALL DC_IFFP  ( 'CLAM4', rimnem, NRIMN, allok, ircla4(1), ier )
	CALL DC_IFMP  ( 5, MX4LYR, ircla4, ier )
	CALL DC_IFFP  ( 'CLTP4', rimnem, NRIMN, allok, irclt4(1), ier )
	CALL DC_IFMP  ( 5, MX4LYR, irclt4, ier )
	CALL DC_IFFP  ( 'HOCT4', rimnem, NRIMN, allok, irhct4(1), ier )
	CALL DC_IFMP  ( 5, MX4LYR, irhct4, ier )
	CALL DC_IFFP  ( 'CLDT4', rimnem, NRIMN, allok, ircldt(1), ier )
	CALL DC_IFMP  ( 5, MX4LYR, ircldt, ier )
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
