	SUBROUTINE TA_IFSP  ( rimnem, cimnem, iret )
C************************************************************************
C* TA_IFSP								*
C*									*
C* This subroutine initializes the interface mnemonic arrays and sets	*
C* the pointers within COMMON / RINTFP / and COMMON / CINTFP /.         *
C*									*
C* TA_IFSP  ( RIMNEM, CIMNEM, IRET )					*
C*									*
C* Output parameters:							*
C*	RIMNEM (*)	CHAR*		Interface mnemonics for reals	*
C*	CIMNEM (*)	CHAR*		Interface mnemonics for chars	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = one or more pointers	*
C*					      could not be set 		*
C*									*
C**									*
C* Log:									*
C* C. Caruso Magee/NCEP 08/06                                           *
C* C. Caruso Magee/NCEP 10/07	Add pressure (PRLC)			*
C* J. Ator/NCEP		10/08	Add ACTP and OBSVR			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'tacmn.cmn'
C*
	CHARACTER*(*)	rimnem (*), cimnem (*)    
C*
	LOGICAL		allok
	INTEGER		iploc ( NRIMN )
	CHARACTER	rifmn ( NRIMN )*8, cifmn ( NCIMN )*8 
C*
C*	Establish equivalance between iploc ( ) and COMMON / RINTFP /
C*
	EQUIVALENCE	( iploc (1), iryear )
C*
C*	Real interface mnemonics.
C*
	DATA	( rifmn (i), i = 1, NRIMN )
     +	/  'YEAR'  , 'MNTH'  , 'DAYS'  , 'HOUR'  , 'MINU'  , 'SECO'  ,
     +     'SLAT'  , 'SLON'  , 'HMSL'  , 'PRLC'  , 'FLVL'  , 'POAF'  ,
     +	   'IALR'  , 'SMMO'  , 'TMDB'  , 'WDIR'  , 'WSPD'  , 'TRBX'  ,
     +	   'TEDR'  , 'AFIC'  , 'PCCF'  , 'REHU'  , 
     +     'NTQC'  , 'TPQC'  ,
     +                  MLM1QC * ' ' /
C*
C*      Character interface mnemonics.
C
	DATA	( cifmn (i), i = 1, NCIMN )
     +	/  'ACRN'  , 'ACTP'  , 'OBSVR'  / 
C*
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
C*	Set the pointers for the character interface mnemonics.
C
        CALL DC_IFFP  ( 'ACRN', cimnem, NCIMN, allok, icacrn, ier )
        CALL DC_IFFP  ( 'ACTP', cimnem, NCIMN, allok, icactp, ier )
        CALL DC_IFFP  ( 'OBSVR', cimnem, NCIMN, allok, icobsv, ier )
C
C*	Set the pointers for the real single-level interface mnemonics.
C
	DO ii = 1, NRSLMN
	    CALL DC_IFFP  ( rimnem (ii), rimnem, NRIMN, allok,
     +			    iploc (ii), ier ) 
	END DO
C
C*	Set the pointers for the multi-level interface mnemonics.
C
        CALL DC_IFFP  ( 'NTQC', rimnem, NRIMN, allok, irntqc, ier )
        CALL DC_IFFP  ( 'TPQC', rimnem, NRIMN, allok, irtpqc (1), ier )
        CALL DC_IFMP  ( 1, MXTPQC, irtpqc, ier )
C
C*	Were all of the pointers properly set?
C
	IF  ( .not. allok ) THEN
	    iret = -1
	END IF
C*
	RETURN
	END
