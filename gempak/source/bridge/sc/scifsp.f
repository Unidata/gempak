	SUBROUTINE SC_IFSP ( rimnem, cimnem, iret )
C************************************************************************
C* SC_IFSP                                                              *
C*                                                                      *
C* This subroutine initializes the interface mnemonic arrays and sets   *
C* the pointers within COMMON / RINTFP / and COMMON / CINTFP /.         *
C*                                                                      *
C* SC_IFSP  ( RIMNEM, CIMNEM, IRET )                                    *
C*                                                                      *
C* Output parameters:                                                   *
C*	RIMNEM (*)	CHAR*		Interface mnemonics for reals   *
C*	CIMNEM (*)	CHAR*		Interface mnemonics for chars   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                       -1 = one or more pointers      *
C*                                            could not be set          *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* A. Hardy/GSC		12/97 	Based on AF_IFSP                        *
C* D. Kidwell/NCEP	 6/98	Store NPWX with multi-level vals        *
C* D. Kidwell/NCEP	10/98	Added init of interface mnemonics;      *
C*                              added intf mnemonics to calling sequence*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
	INCLUDE         'sccmn.cmn'
C*
	CHARACTER*(*)	rimnem (*), cimnem (*)
C*
	LOGICAL         allok
	INTEGER         iploc ( NRSIMN )
	CHARACTER	rifmn ( NRIMN )*8, cifmn ( NCIMN )*8
C*
C*      Establish equivalence between iploc ( ) and COMMON / RINTFP /
C*
	EQUIVALENCE     ( iploc (1), irtdxc )
C*
	DATA	( rifmn (i), i = 1, NRIMN )
     +	/   'TDXC'  , 'TDNC'  , 'P06I'  , 'P24I'  , 'SNOW'  , 'SNEW'  ,
     +	    'WEQS'  , 'MSUN'  , 'CTYL'  , 'CTYM'  , 'CTYH'  , 'CFRT'  ,
     +	    'CFRL'  , 'CBAS'  , 'SLAT'  , 'SLON'  , 'SELV'  , 'CORN'  ,
     +      'NPWX'  /
C*					Real interface mnemonics
C*
	DATA	( cifmn (i), i = 1, NCIMN )
     +	/  'STID' , 'WCOD' ,  ' ' ,  ' '  /
C*					Character interface mnemonics
C*
C-----------------------------------------------------------------------
	iret = 0
C
C*	Initialize the interface mnemonics.
C
	DO i = 1,NRIMN
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
	DO ii = 1, NRSIMN
	    CALL DC_IFFP ( rimnem (ii), rimnem, NRIMN, allok,
     +			   iploc (ii), ier )
	END DO
C
	CALL DC_IFFP  ( 'STID', cimnem, NCIMN, allok, icstid, ier )
C
C*	Set the pointers for multiple character weather groups.
C
	CALL DC_IFFP  ( 'NPWX', rimnem, NRIMN, allok, irnpwx, ier)
	CALL DC_IFFP  ( 'WCOD', cimnem, NCIMN, allok, icwcod (1), ier)
	CALL DC_IFMP  ( 1, MXWLYR, icwcod, ier )
C
C*	Check that all of the pointers were set correctly.
C
	IF ( .not. allok ) THEN
	    iret = -1
	END IF
C*
	RETURN
	END
