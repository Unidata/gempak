	SUBROUTINE MN_GLIN ( report, irh, istart, stid, iret ) 
C************************************************************************
C* MN_GLIN								*
C*									*
C* This subroutine determines the indicies of the beginning of each 	*
C* line in an NGM MOS report.						*
C*									*
C* MN_GLIN ( REPORT, IRH, ISTART, STID, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		NGM MOS Report			*
C*	IRH		INTEGER		Hour of Report			*
C*									*
C* Output parameters:							*
C*	ISTART(*)	INTEGER		Indicies of line beginnings	*
C*	STID		CHAR*		Station identifier		*
C*	IRET		INTEGER		Return Code			*
C**									*
C* Log:									*
C* D. Keiser/GSC	 2/96						*
C* F. J. Yen/NCEP	11/98	Cleaned up and restructured from AV_GLIN*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	PARAMETER	( NUMNAM = 22 )
C*
	CHARACTER*(*)	report, stid
	INTEGER		istart(*)
C
	CHARACTER	cpnam (NUMNAM)*5
	DATA		cpnam / 'DAY', 'HOUR', 'MN/MX', 'MX/MN',
     +				'TEMP', 'DEWPT', 'CLDS', 'WDIR',
     +				'WSPD', 'POP06', 'POP12', 'QPF',
     +				'TSV06', 'TSV12', 'PTYPE', 'POZP',
     +				'POSN', 'SNOW', 'CIG', 'VIS', 
     +				'OBVIS', 'UTC' /
C------------------------------------------------------------------------
	iret = 0
C
C*	Get station ID
C
	stid = report(1:4)
C
C*	Loop through the report, one line at a time.
C
	DO k = 1, NUMNAM
C
C*	    Find the index that identifies the start of the line.
C
	    CALL ST_LSTR ( cpnam(k), ilen, ier )
	    istart (k) = INDEX ( report, cpnam (k)(1:ilen) )
	    IF ( istart ( k ) .gt. 0 ) THEN
		istart ( k ) = istart ( k ) + 6
	    END IF 
	END DO
C*
	RETURN
	END
