        SUBROUTINE LS_INTF ( rimnem, cimnem, cprms, imnem, numprm,
     +			     iret )
C************************************************************************
C* LS_INTF                                                              *
C*                                                                      *
C* This subroutine establishes the link between the GEMPAK parameters   *
C* and the interface mnemonics.                                         *
C* 								        *
C* LS_INTF ( RIMNEM, CIMNEM, CPRMS, IMNEM, NUMPRM, IRET )		*
C*								        *
C* Input parameters:                                                    *
C*	RIMNEM (*)	CHAR*	     Interface mnemonics for reals      *
C*	CIMNEM (*)	CHAR*	     Interface mnemonics for chars      *
C*								        *
C* Output parameters:						        *
C*	CPRMS (*)	CHAR*        GEMPAK parms                       *
C*	IMNEM (*)	INTEGER      Subscript mappings, data to GEMPAK *
C*	NUMPRM		INTEGER	     Number of GEMPAK parameters        *
C*      IRET            INTEGER      Return code                        *
C*                                     0 = Normal return                *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* D. Kidwell/NCEP      1/98	Based on MA_INTF                        *
C* D. Kidwell/NCEP      2/98	Added code to pick up snow amounts      *
C* D. Kidwell/NCEP      2/98	Added 24-hour pressure change           *
C* D. Kidwell/NCEP      4/98	Removed relative humidity               *
C* D. Kidwell/NCEP      4/98	Removed TMWC                            *
C* D. Kidwell/NCEP      5/98	New interface mnemonics MXTM, MITM      *
C* D. Kidwell/NCEP     10/98	Added intf mnemonics to calling sequence*
C* D. Kidwell/NCEP     10/99	Removed WWMA, PWWA                      *
C* S. Chiswell/Unidata	8/01	Added T12X, T12N 12hr max/min		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE  	'lscmn.cmn'
C*
        CHARACTER*(*)  	cprms (*), rimnem (*), cimnem (*)
	INTEGER		imnem (*)
C*
        CHARACTER  	dprms (MMPARM)*4
C*
	PARAMETER	( NUMP = 36 )
	PARAMETER	( NUMEXT = MMPARM - NUMP )
C*
        DATA  		dprms / 'PMSL',  'PRES',  'P03D',  'TMPC',
     +                		'DWPC',  'SPED',
     +                		'DRCT',  'GUMS',  'VSBK',  'P03M',
     +                		'P06M',  'P09M',  'P12M',  'P18M',
     +               		'P24M',  'TDXC',  'TDNC',  'T12X',
     +				'T12N',  'WWMO',  'PWWM',  'CFRT',
     +                		'CFRL',  'CTYL',  'CTYM',  'CTYH', 
     +				'CLHL',  'CLHM',  'CLHH',  'SSTC',
     +                		'WPER',  'WHGT',  'SNOW',  'CBAS',
     +                		'SNEW',  'P24C',  NUMEXT*' ' /
C-----------------------------------------------------------------------
        iret = 0
C
	DO i = 1, MMPARM
	    imnem ( i ) = 0
	END DO
C
C*	Loop to find all interface mnemonics which match GEMPAK parms.
C
	DO i = 1, NUMP
	    cprms ( i ) = dprms ( i )
	    k = 1
	    DO WHILE ( k .le. NRIMN )
		IF ( dprms ( i ) .eq. rimnem ( k ) )  THEN
		    imnem ( i ) = k
		    k = NRIMN + 1
		  ELSE
		    k = k + 1
		END IF
	    END DO
	END DO
C
C*	Parms SNOW and SNEW will require modifications of the decoded
C*	values, so find corresponding interface mnemonics now.
C
	CALL ST_FIND ( 'SNOW', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'SNOWCM', rimnem, NRIMN, kloc, ier )
	imnem ( iloc ) = kloc
C
	CALL ST_FIND ( 'SNEW', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'DOFS', rimnem, NRIMN, kloc, ier ) 
	imnem ( iloc ) = kloc
C
C*	Parm P24C uses interface mnemonic 24PC (a BUFR legacy),
C*	so find corresponding mnemonic now.
C
	CALL ST_FIND ( 'P24C', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( '24PC', rimnem, NRIMN, kloc, ier )
	imnem ( iloc ) = kloc
C
C*	Equate parms TDXC and TDNC to interface mnemonics MXTM and
C*	MITM.  They will only be written to the GEMPAK file if the
C*	time period is 24 hours.
C
	CALL ST_FIND ( 'TDXC', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'MXTM', rimnem, NRIMN, kloc, ier )
	imnem ( iloc ) = kloc
C
	CALL ST_FIND ( 'TDNC', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'MITM', rimnem, NRIMN, kloc, ier ) 
	imnem ( iloc ) = kloc
C
	numprm = NUMP
C*
	RETURN
	END
