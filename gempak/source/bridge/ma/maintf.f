	SUBROUTINE MA_INTF ( rimnem, cimnem, cprms, imnem, numprm,
     +			     iret )
C************************************************************************
C* MA_INTF                                                              *
C*                                                                      *
C* This subroutine establishes the link between the GEMPAK parameters   *
C* and the interface mnemonics.                                         *
C* 								        *
C* MA_INTF ( RIMNEM, CIMNEM, CPRMS, IMNEM, NUMPRM, IRET )		*
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
C* D. Kidwell/NCEP 	4/97 						*
C* D. Kidwell/NCEP 	4/97	Changed CFRA to CFRT			*
C* D. Kidwell/NCEP 	5/97	Expanded GEMPAK parameter list          *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C* D. Kidwell/NCEP     10/98	Added intf mnemonics to calling sequence*
C* D. Kidwell/NCEP     12/98	Added secondary swell wave parameters   *
C* D. Kidwell/NCEP     10/99	Removed WWMA, PWWA                      *
C* F. J. Yen/NCEP	4/01	Added GUST and ALTI for Coast Guard data*
C* D. Kidwell/NCEP      3/02	Added SHPD, SHPK; removed GUM1, P06M,   *
C*				XS10, ITSO                              *
C* D. Kidwell/NCEP      4/05	Added PKWD, PKWS, PKWT, PMN1, PMNT      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)  	cprms (*), rimnem (*), cimnem (*)
	INTEGER		imnem (*)
C*
        CHARACTER  	dprms (MMPARM)*4
C*
	PARAMETER	( NUMP = 43 )
	PARAMETER	( NUMEXT = MMPARM - NUMP )
C*
        DATA		dprms / 'DRCT', 'SPED', 'PWHR', 'PWMN', 
     +				'PWDR', 'PWSP', 'PMSL', 'P03D',
     +				'CTYL', 'CLHL', 'TMPC', 'DWPC',
     +				'SSTC', 'TOST', 'VSBK', 'WWMO',
     +				'PWWM', 'CBAS', 'CFRT', 'WPER',
     +				'WHGT', 'POWW', 'HOWW', 'DOSW',
     +				'POSW', 'HOSW', 'DOS2', 'POS2',
     +				'HOS2', 'IDTH', 'ROIA', 'PRES', 
     +				'TMWC', 'CFRL', 'GUST', 'ALTI',
     +				'SHPD', 'SHPK', 'PKWD', 'PKWS',
     +				'PKWT', 'PMN1', 'PMNT', NUMEXT*' ' /
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
C*	Find interface mnemonics which will need modifications for
C*	GEMPAK output.
C*	GEMPAK output parms in question are DOS2, POS2 and HOS2,
C*	and SHPD and SHPK.
C
	CALL ST_FIND ( 'DOS2', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'DOSW', rimnem, NRIMN, kloc, ier )
	imnem ( iloc ) = kloc + 3
C
	CALL ST_FIND ( 'POS2', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'POSW', rimnem, NRIMN, kloc, ier )
	imnem ( iloc ) = kloc + 3
C
	CALL ST_FIND ( 'HOS2', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'HOSW', rimnem, NRIMN, kloc, ier )
	imnem ( iloc ) = kloc + 3
C
	CALL ST_FIND ( 'SHPD', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'TDMP', rimnem, NRIMN, kloc, ier )
	imnem ( iloc ) = kloc
C
	CALL ST_FIND ( 'SHPK', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'ASMP', rimnem, NRIMN, kloc, ier )
	imnem ( iloc ) = kloc
C
	numprm = NUMP
C*
	RETURN
	END
