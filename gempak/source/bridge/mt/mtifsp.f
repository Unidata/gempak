	SUBROUTINE MT_IFSP  ( rimnem, cimnem, iret )
C************************************************************************
C* MT_IFSP								*
C*									*
C* This subroutine initializes the interface mnemonic arrays and sets   *
C* the pointers within COMMON / RINTFP / and COMMON / CINTFP /.		*
C*									*
C* MT_IFSP  ( RIMNEM, CIMNEM, IRET )					*
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
C* D. Kidwell/NCEP	4/98    Adapted from LS_IFSP - new interface    *
C* D. Kidwell/NCEP     10/98    Added init of interface mnemonics;      *
C*				added intf mnemonics to calling sequence*
C************************************************************************
	INCLUDE	 	'GEMPRM.PRM'
	INCLUDE		'mtcmn.cmn'
C*
	CHARACTER*(*)	rimnem (*), cimnem (*)
C*
	LOGICAL		allok
	INTEGER		iploc (NRSLMN)
	CHARACTER	rifmn ( NRIMN )*8, cifmn ( NCIMN )*8
C*
C*	Establish array equivalence to COMMON / RINTFP /
C*
	EQUIVALENCE	( iploc (1), irthrp )
C*
	DATA	( rifmn (i), i = 1, NRSLMN ) /
     +		'THRPT' , 'AUTO'  , 'DRCT'  , 'SPED'  , 'SKNT'  ,
     +		'GUMS'  , 'GUST'  , 'DRC1'  , 'DRC2'  , 'VRTV'  ,
     +		'TMPC'  , 'DWPC'  , 'ALTI'  , 'ALTM'  , 'P03I'  ,
     +		'P06I'  , 'P24I'  , 'P01I'  , 'SNOW'  , 'SNEW'  ,
     +		'WEQS'  , 'CTYL'  , 'CTYM'  , 'CTYH'  , 'MSUN'  ,
     +		'PMSL'  , 'TDXC'  , 'TDNC'  , 'T6XC'  , 'T6NC'  ,
     +		'P03D'  , 'CTMX'  , 'CTMN'  , 'CTTP'  , 'CORN'  ,
     +		'PWDR'  , 'PWSP'  , 'PWHR'  , 'PWMN'  , 'WSHH'  ,
     +		'WSHM'  , 'SLAT'  , 'SLON'  , 'SELV'  /
	DATA	( rifmn (i), i = NRSLP1, NRIMN ) /
     +		'NVSB'  , 'VSBY'  , 'VSBK'  , 'VSFL'  ,
     +			  MVM1T3 * ' ',
     +		'NRWY'  , 'V1RIM' , 'V2RIM' , 'V1RF'  , 'V2RF'  ,
     +			  'RWYT'  , MRM1T5 * ' ',
     +		'NPWX'  ,
     +		'NSKY'  , 'CMTN'  , MSM1 * ' ' /
C*					Real interface mnemonics
C*
	DATA	( cifmn (i), i = 1, NCIMN ) /
     +	  	'STID'  , 
     +		'RWID'  , MRM1 * ' ' ,
     +		'WCOD'  , MWM1 * ' ' /
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
	CALL DC_IFFP ( 'STID', cimnem, NCIMN, allok, icstid, ier )
C
C*	Set the pointers for the multi-level interface mnemonics.
C
C*	Set the pointers for horizontal visibility data.
C
	CALL DC_IFFP ( 'NVSB', rimnem, NRIMN, allok, irnvsb, ier )
	CALL DC_IFFP ( 'VSBY', rimnem, NRIMN, allok, irvsby (1), ier )
	CALL DC_IFMP ( 3, MXVISB, irvsby, ier )
	CALL DC_IFFP ( 'VSBK', rimnem, NRIMN, allok, irvsbk (1), ier )
	CALL DC_IFMP ( 3, MXVISB, irvsbk, ier )
	CALL DC_IFFP ( 'VSFL', rimnem, NRIMN, allok, irvsfl (1), ier )
	CALL DC_IFMP ( 3, MXVISB, irvsfl, ier )
C
C*	Set the pointers for runway visibility data.
C
	CALL DC_IFFP ( 'NRWY', rimnem, NRIMN, allok, irnrwy, ier )
	CALL DC_IFFP ( 'RWID', cimnem, NCIMN, allok, icrwid(1), ier )
	CALL DC_IFMP ( 1, MXRWVR, icrwid, ier )
	CALL DC_IFFP ( 'V1RIM', rimnem, NRIMN, allok, irv1ri(1), ier )
	CALL DC_IFMP ( 5, MXRWVR, irv1ri, ier )
	CALL DC_IFFP ( 'V2RIM', rimnem, NRIMN, allok, irv2ri(1), ier )
	CALL DC_IFMP ( 5, MXRWVR, irv2ri, ier )
	CALL DC_IFFP ( 'V1RF', rimnem, NRIMN, allok, irv1rf(1), ier )
	CALL DC_IFMP ( 5, MXRWVR, irv1rf, ier )
	CALL DC_IFFP ( 'V2RF', rimnem, NRIMN, allok, irv2rf(1), ier )
	CALL DC_IFMP ( 5, MXRWVR, irv2rf, ier )
	CALL DC_IFFP ( 'RWYT', rimnem, NRIMN, allok, irrwyt(1), ier )
	CALL DC_IFMP ( 5, MXRWVR, irrwyt, ier )
C 
C*	Set the pointers for present weather data. 
C
	CALL DC_IFFP ( 'NPWX', rimnem, NRIMN, allok, irnpwx, ier )
	CALL DC_IFFP ( 'WCOD', cimnem, NCIMN, allok, icwcod(1), ier )
	CALL DC_IFMP ( 1, MXWTHR, icwcod, ier )
C 
C*	Set the pointers for sky condition data. 
C
	CALL DC_IFFP ( 'NSKY', rimnem, NRIMN, allok, irnsky, ier )
	CALL DC_IFFP ( 'CMTN', rimnem, NRIMN, allok, ircmtn(1), ier )
	CALL DC_IFMP ( 1, MXSKYC, ircmtn, ier )
C
C*	Were all of the pointers properly set?
C
	IF  ( .not. allok ) THEN
	    iret = -1
	END IF
C*
	RETURN
	END
