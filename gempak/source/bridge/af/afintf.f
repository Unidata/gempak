        SUBROUTINE AF_INTF ( rimnem, cimnem, cprms, imnem, numprm, 
     +			     iret )
C************************************************************************
C* AF_INTF                                                              *
C*                                                                      *
C* This subroutine establishes the link between the GEMPAK parameters   *
C* and the interface mnemonics.                                         *
C* 								        *
C* AF_INTF ( RIMNEM, CIMNEM, CPRMS, IMNEM, NUMPRM, IRET )               *
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
C* D. Kidwell/NCEP 	9/97 	Based on MA_INTF			*
C* D. Kidwell/NCEP 	4/98 	Removed RELH, added PRES                *
C* D. Kidwell/NCEP     10/98 	Added intf mnemonics to calling sequence*
C* D. Kidwell/NCEP      6/99 	Added more icing, turb, sky cover parms *
C* D. Kidwell/NCEP      7/99 	Restructured, hgt parms meters -> feet  *
C* D. Kidwell/NCEP      8/99 	(HOCB,HOCT) -> (HCBF,HCTF)              *
C* D. Kidwell/NCEP      1/00 	Added ATP1                              *
C************************************************************************
        INCLUDE  	'GEMPRM.PRM'
        INCLUDE  	'afcmn.cmn'
C*
	CHARACTER*(*)	cprms (*), rimnem (*), cimnem (*)
	INTEGER		imnem (*)
C*
        CHARACTER  	dprms (MMPARM)*4, intf (11)*4, gmpk (14)*4
C*
	PARAMETER 	( NUMP = 28 )
	PARAMETER	( NUMEXT = MMPARM - NUMP )
C*
        DATA		dprms / 'DRCT', 'SKNT', 'TMPC', 'DWPC', 
     +                		'VSBY', 'PMSL', 'PRES', 'SSTC',
     +                		'TURB', 'TBSE', 'TTOP', 'FQOT',
     +				'TPOT', 'ICNG', 'IBSE', 'ITOP',
     +                		'TPOI', 'WNUM', 'WBSE', 'WTOP',
     +                          'CLC1', 'CBS1', 'CTP1', 'CLC2',
     +				'CBS2', 'CTP2', 'ACRT', 'ATP1',
     +                		NUMEXT * ' ' /           
	DATA		intf  / 'DGOT', 'HBOT', 'HTOT', 'AFIC',
     +				'HBOI', 'HTOI', 'HBWX', 'HTWX',
     +				'CLAM', 'HCBF', 'HCTF' /
	DATA		gmpk  / 'TURB', 'TBSE', 'TTOP', 'ICNG',
     +				'IBSE', 'ITOP', 'WBSE', 'WTOP',
     +				'CLC1', 'CBS1', 'CTP1',
     +				'CLC2', 'CBS2', 'CTP2' /
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
C*	Reconcile differences between Gempak parameters and interface
C*	mnemonics.  Parms CLCn, CBSn, and CTPn (n = 1,2) will map to 
C*	the first two levels of sky cover data in the interface.
C*	Parm ACRT will be determined based on the bulletin type
C*	(aircraft report type).
C
	CALL ST_FIND ( 'WNUM', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'WCOD', cimnem, NCIMN, kloc, ier )
	imnem ( iloc ) = kloc
C
	CALL ST_FIND ( 'ATP1', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'ACTP', cimnem, NCIMN, kloc, ier )
	imnem ( iloc ) = kloc
C
	DO i = 1, 11
	    CALL ST_FIND ( gmpk ( i ), dprms, NUMP, iloc, ier )
	    CALL ST_FIND ( intf ( i ), rimnem, NRIMN, kloc, ier )
	    imnem ( iloc ) = kloc
	    IF ( i .gt. 8 ) THEN
		CALL ST_FIND ( gmpk ( i + 3 ), dprms, NUMP, iloc, ier )
		imnem ( iloc ) = kloc + 4
	    END IF
	END DO
C
	numprm = NUMP
C*
	RETURN
	END
