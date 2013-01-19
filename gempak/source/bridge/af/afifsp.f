	SUBROUTINE AF_IFSP  ( rimnem, cimnem, iret )
C************************************************************************
C* AF_IFSP								*
C*									*
C* This subroutine initializes the interface mnemonic arrays and sets   *
C* the pointers within COMMON / RINTFP / and COMMON / CINTFP /.		*
C*									*
C* AF_IFSP  ( RIMNEM, CIMNEM, IRET )					*
C*									*
C* Output parameters:							*
C*	RIMNEM (*)	CHAR*		Interface mnemonics for reals   *
C*	CIMNEM (*)	CHAR*		Interface mnemonics for chars   *
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = one or more pointers	*
C*					      could not be set 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		08/97						*
C* J. Ator/NCEP		12/97	AF_IFMP -> DC_IFMP, AF_IFFP -> DC_IFFP	*
C* D. Kidwell/NCEP	10/98	Added init of interface mnemonics;      *
C*				added intf mnemonics to calling sequence*
C* J. Ator/NCEP		12/98	Added YEAR, MNTH, DAYS to /INTF/	*
C* D. Kidwell/NCEP	 6/99	Added more icing and turbulence parms   *
C* D. Kidwell/NCEP	 8/99	(HOCB,HOCT) -> (HCBF,HCTF)              *
C* D. Kidwell/NCEP	 1/00	Added RSID (reporting station id)       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	rimnem (*), cimnem (*)
C*
	LOGICAL		allok
	INTEGER		iploc ( NRSIMN )
	CHARACTER	rifmn ( NRIMN )*8, cifmn ( NCIMN )*8
C*
C*	Establish equivalence between iploc ( ) and COMMON / RINTFP /
C*
	EQUIVALENCE	( iploc (1), irslat )
C*
	DATA	( rifmn ( i ), i = 1, NRIMN )
     +	/   'SLAT' , 'SLON' , 'DRCT' , 'SKNT' , 'TMPC' , 'DWPC' ,
     +	    'RELH' , 'SSTC' , 'PMSL' , 'FLVL' , 'PSAL' , 'POAF' ,
     +	    'ACNS' , 'TADR' , 'PCAT' , 'MDEVG', 'DAYW' , 'VSBY' ,
     +	    'YEAR' , 'MNTH' , 'DAYS' , 'HOUR' , 'MINU' ,
     +	    'WDIR1', 'WSKT1',
     +	    'VSIG' , 'PRES' , 'HGTM' ,
     +	    'NTRB' , 'DGOT' , 'HBOT' , 'HTOT' , 'FQOT' , 'TPOT' ,
     +		        MNM1T5 * ' ',
     +	    'NICG' , 'AFIC' , 'HBOI' , 'HTOI' , 'TPOI' ,
     +			MNM1T4 * ' ',
     +	    'NPWX' , 'HBWX' , 'HTWX' ,
     +			MWM1T2 * ' ',
     +	    'NCLD' , 'CLAM' , 'CLTP' , 'HCBF' , 'HCTF' ,
     +			MNM1T4 * ' '  	/
C*					Real interface mnemonics 
C*
	DATA	( cifmn ( i ), i = 1, NCIMN )
     +	/   'ACID' , 'ACTP' , 'RPID' , 'RSID' ,
     +	    'WCOD' , MWM1 * ' '  /
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
	DO ii = 1, NRSIMN
	    CALL DC_IFFP  ( rimnem (ii), rimnem, NRIMN, allok,
     +			    iploc (ii), ier ) 
	END DO
C
	CALL DC_IFFP  ( 'ACID', cimnem, NCIMN, allok, icacid, ier )
	CALL DC_IFFP  ( 'ACTP', cimnem, NCIMN, allok, icactp, ier )
	CALL DC_IFFP  ( 'RPID', cimnem, NCIMN, allok, icrpid, ier )
	CALL DC_IFFP  ( 'RSID', cimnem, NCIMN, allok, icrsid, ier )
C
C*	Set the pointers for the multi-level interface mnemonics.
C
C*	Turbulence.
C
	CALL DC_IFFP  ( 'NTRB', rimnem, NRIMN, allok, irntrb, ier )
	CALL DC_IFFP  ( 'DGOT', rimnem, NRIMN, allok, irdgot (1), ier )
	CALL DC_IFMP  ( 5, MXNLYR, irdgot, ier )
	CALL DC_IFFP  ( 'HBOT', rimnem, NRIMN, allok, irhbot (1), ier )
	CALL DC_IFMP  ( 5, MXNLYR, irhbot, ier )
	CALL DC_IFFP  ( 'HTOT', rimnem, NRIMN, allok, irhtot (1), ier )
	CALL DC_IFMP  ( 5, MXNLYR, irhtot, ier )
	CALL DC_IFFP  ( 'FQOT', rimnem, NRIMN, allok, irfqot (1), ier )
	CALL DC_IFMP  ( 5, MXNLYR, irfqot, ier )
	CALL DC_IFFP  ( 'TPOT', rimnem, NRIMN, allok, irtpot (1), ier )
	CALL DC_IFMP  ( 5, MXNLYR, irtpot, ier )
C
C*	Airframe icing.
C
	CALL DC_IFFP  ( 'NICG', rimnem, NRIMN, allok, irnicg, ier )
	CALL DC_IFFP  ( 'AFIC', rimnem, NRIMN, allok, irafic (1), ier )
	CALL DC_IFMP  ( 4, MXNLYR, irafic, ier )
	CALL DC_IFFP  ( 'HBOI', rimnem, NRIMN, allok, irhboi (1), ier )
	CALL DC_IFMP  ( 4, MXNLYR, irhboi, ier )
	CALL DC_IFFP  ( 'HTOI', rimnem, NRIMN, allok, irhtoi (1), ier )
	CALL DC_IFMP  ( 4, MXNLYR, irhtoi, ier )
	CALL DC_IFFP  ( 'TPOI', rimnem, NRIMN, allok, irtpoi (1), ier )
	CALL DC_IFMP  ( 4, MXNLYR, irtpoi, ier )
C
C*	Cloud data.
C
	CALL DC_IFFP  ( 'NCLD', rimnem, NRIMN, allok, irncld, ier )
	CALL DC_IFFP  ( 'CLAM', rimnem, NRIMN, allok, irclam (1), ier )
	CALL DC_IFMP  ( 4, MXNLYR, irclam, ier )
	CALL DC_IFFP  ( 'CLTP', rimnem, NRIMN, allok, ircltp (1), ier )
	CALL DC_IFMP  ( 4, MXNLYR, ircltp, ier )
	CALL DC_IFFP  ( 'HCBF', rimnem, NRIMN, allok, irhcbf (1), ier )
	CALL DC_IFMP  ( 4, MXNLYR, irhcbf, ier )
	CALL DC_IFFP  ( 'HCTF', rimnem, NRIMN, allok, irhctf (1), ier )
	CALL DC_IFMP  ( 4, MXNLYR, irhctf, ier )
C
C*	Present weather.
C
	CALL DC_IFFP  ( 'NPWX', rimnem, NRIMN, allok, irnpwx, ier )
	CALL DC_IFFP  ( 'WCOD', cimnem, NCIMN, allok, icwcod (1), ier )
	CALL DC_IFMP  ( 1, MXWLYR, icwcod, ier )
	CALL DC_IFFP  ( 'HBWX', rimnem, NRIMN, allok, irhbwx (1), ier )
	CALL DC_IFMP  ( 2, MXWLYR, irhbwx, ier )
	CALL DC_IFFP  ( 'HTWX', rimnem, NRIMN, allok, irhtwx (1), ier )
	CALL DC_IFMP  ( 2, MXWLYR, irhtwx, ier )
C
C*	Were all of the pointers properly set?
C
	IF  ( .not. allok ) THEN
	    iret = -1
	END IF
C*
	RETURN
	END
