	SUBROUTINE IG_IFSP  ( rimnem, iret )
C************************************************************************
C* IG_IFSP								*
C*									*
C* This subroutine initializes the interface mnemonic arrays and sets	*
C* the pointers within COMMON / RINTFP /.                      		*
C*									*
C* IG_IFSP  ( RIMNEM, IRET )						*
C*									*
C* Output parameters:							*
C*	RIMNEM (*)	CHAR*		Interface mnemonics for reals	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = one or more pointers	*
C*					      could not be set 		*
C*									*
C**									*
C* Log:									*
C* C. Caruso Magee/NCEP 10/05                                           *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'igcmn.cmn'
C*
	CHARACTER*(*)	rimnem (*)                
C*
	LOGICAL		allok
	INTEGER		iploc ( NRIMN )
	CHARACTER	rifmn ( NRIMN )*8                      
C*
C*	Establish equivalance between iploc ( ) and COMMON / RINTFP /
C*
	EQUIVALENCE	( iploc (1), iryear )
C*
C*	Real interface mnemonics.
C*
	DATA	( rifmn (i), i = 1, NRIMN )
     +	/  'YEAR'  , 'MNTH'  , 'DAYS'  , 'HOUR'  , 'MINU'  , 'SECW'  ,
     +     'SLAT'  , 'SLON'  , 'SAID'  , 'SWID'  , 'OGCE'  , 'ORBN'  ,
     +	   'SELV'  , 'HINC'  , 'RSST'  , 'AETP'  , 'LSQL'  , 'ASFL'  ,
     +	   'RSFL'  , 'EENO'  , '1TQC'  , 'SGW1'  , 'FOST'  , 'SGW2'  ,
     +	   'NVPP'  , '2TQC'  , 'SONA'  , 'RWVC'  , 'RLQC'  , 'SSI1'  ,
     +     'SSI2'  , 
     +     'NBKS'  , 'TOBD'  , 'QCB1'  , 'BKS1'  , 'FOS1'  ,
     +     'BKS2'  , 'QCB2'  , 'ELB1'  , 'HINB'  , 'FOS2'  ,
     +     'ELB2'  , 'NVPB'  , 
     +                  MLM1BK * ' ' ,
     +     'NMEF'  , 'MEFR'  , 'QCTM'  , 'TMBR'  ,
     +                  MLM1FT * ' ' ,
     +     'NSWW'  , 'SWCM'  , 'WS10'  , 
     +           MLM1SW * ' ' /
C*
C-----------------------------------------------------------------------
	iret = 0
C
C*	Initialize the interface mnemonics.
C
	DO i = 1, NRIMN
	    rimnem ( i ) = rifmn ( i ) 
	END DO
C
C*	The logical variable "allok" is initially set to .true. but will
C*	be reset to .false. if any of the pointers cannot be set.
C
	allok = .true.
C
C*	Set the pointers for the real single-level interface mnemonics.
C
	DO ii = 1, NRSLMN
	    CALL DC_IFFP  ( rimnem (ii), rimnem, NRIMN, allok,
     +			    iploc (ii), ier ) 
	END DO
C
C*	Set the pointers for the multi-level interface mnemonics.
C*      Backscatter and related parameters.
C
        CALL DC_IFFP  ( 'NBKS', rimnem, NRIMN, allok, irnbks, ier )
        CALL DC_IFFP  ( 'TOBD', rimnem, NRIMN, allok, irtobd (1), ier )
        CALL DC_IFMP  ( 11, MXBKST, irtobd, ier )
        CALL DC_IFFP  ( 'QCB1', rimnem, NRIMN, allok, irqcb1 (1), ier )
        CALL DC_IFMP  ( 11, MXBKST, irqcb1, ier )
        CALL DC_IFFP  ( 'BKS1', rimnem, NRIMN, allok, irbks1 (1), ier )
        CALL DC_IFMP  ( 11, MXBKST, irbks1, ier )
        CALL DC_IFFP  ( 'FOS1', rimnem, NRIMN, allok, irfos1 (1), ier )
        CALL DC_IFMP  ( 11, MXBKST, irfos1, ier )
        CALL DC_IFFP  ( 'BKS2', rimnem, NRIMN, allok, irbks2 (1), ier )
        CALL DC_IFMP  ( 11, MXBKST, irbks2, ier )
        CALL DC_IFFP  ( 'QCB2', rimnem, NRIMN, allok, irqcb2 (1), ier )
        CALL DC_IFMP  ( 11, MXBKST, irqcb2, ier )
        CALL DC_IFFP  ( 'ELB1', rimnem, NRIMN, allok, irelb1 (1), ier )
        CALL DC_IFMP  ( 11, MXBKST, irelb1, ier )
        CALL DC_IFFP  ( 'HINB', rimnem, NRIMN, allok, irhinb (1), ier )
        CALL DC_IFMP  ( 11, MXBKST, irhinb, ier )
        CALL DC_IFFP  ( 'FOS2', rimnem, NRIMN, allok, irfos2 (1), ier )
        CALL DC_IFMP  ( 11, MXBKST, irfos2, ier )
        CALL DC_IFFP  ( 'ELB2', rimnem, NRIMN, allok, irelb2 (1), ier )
        CALL DC_IFMP  ( 11, MXBKST, irelb2, ier )
        CALL DC_IFFP  ( 'NVPB', rimnem, NRIMN, allok, irnvpb (1), ier )
        CALL DC_IFMP  ( 11, MXBKST, irnvpb, ier )
C
C*      Mean frequency, brightness temperature, and assoc. parameters.
C
        CALL DC_IFFP  ( 'NMEF', rimnem, NRIMN, allok, irnmef, ier )
        CALL DC_IFFP  ( 'MEFR', rimnem, NRIMN, allok, irmefr (1), ier )
        CALL DC_IFMP  ( 3, MXFRTM, irmefr, ier )
        CALL DC_IFFP  ( 'QCTM', rimnem, NRIMN, allok, irqctm (1), ier )
        CALL DC_IFMP  ( 3, MXFRTM, irqctm, ier )
        CALL DC_IFFP  ( 'TMBR', rimnem, NRIMN, allok, irtmbr (1), ier )
        CALL DC_IFMP  ( 3, MXFRTM, irtmbr, ier )
C
C*      Satellite derived wind calculation method and 10 m wind speed.
C
        CALL DC_IFFP  ( 'NSWW', rimnem, NRIMN, allok, irnsww, ier )
        CALL DC_IFFP  ( 'SWCM', rimnem, NRIMN, allok, irswcm (1), ier )
        CALL DC_IFMP  ( 2, MXSWWS, irswcm, ier )
        CALL DC_IFFP  ( 'WS10', rimnem, NRIMN, allok, irws10 (1), ier )
        CALL DC_IFMP  ( 2, MXSWWS, irws10, ier )
C
C*	Were all of the pointers properly set?
C
	IF  ( .not. allok ) THEN
	    iret = -1
	END IF
C*
	RETURN
	END
