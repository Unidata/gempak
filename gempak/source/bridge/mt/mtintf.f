        SUBROUTINE MT_INTF ( rimnem, cimnem, cprms, imnem, numprm, 
     +			     iret )
C************************************************************************
C* MT_INTF                                                              *
C*                                                                      *
C* This subroutine establishes the link between the GEMPAK parameters   *
C* and the interface mnemonics.                                         *
C* 								        *
C* MT_INTF ( RIMNEM, CIMNEM, CPRMS, IMNEM, NUMPRM, IRET )		*
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
C* D. Kidwell/NCEP	4/98	Adapted from LS_INTF - new interface    *
C* D. Kidwell/NCEP	5/98	Added parameter CEIL                    *
C* D. Kidwell/NCEP     10/98	Added intf mnemonics to calling sequence*
C* D. Kidwell/NCEP	1/99	Added parameter P01I, 1-hour precip     *
C* S. Jacobs/NCEP	8/12	Added parameter SNEW, new snowfall	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE  	'mtcmn.cmn'
C*
        CHARACTER*(*)  	cprms (*), rimnem (*), cimnem (*)
	INTEGER		imnem (*)
C*
        CHARACTER  	dprms (MMPARM)*4
C*
	PARAMETER	( NUMP = 30 )
	PARAMETER	( NUMEXT = MMPARM - NUMP )
C*
        DATA  		dprms / 'PMSL',  'ALTI',  'TMPC',  'DWPC',  
     +                          'SKNT',  'DRCT',  'GUST',  'WNUM',
     +                		'CHC1',  'CHC2',  'CHC3',  'VSBY',
     +                		'P03D',  'P03I',  'MSUN',  'SNOW',
     +               		'WEQS',  'P24I',  'TDXC',  'TDNC',  
     +                		'P03C',  'CTYL',  'CTYM',  'CTYH',
     +                		'P06I',  'T6XC',  'T6NC',  'CEIL',
     +                		'P01I',  'SNEW', NUMEXT*' ' /
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
C*	GEMPAK output parms in question are WNUM, CHC1, 2, and 3,
C*	CEIL and P03C.
C
	CALL ST_FIND ( 'WNUM', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'WCOD', cimnem, NCIMN, kloc, ier )
	imnem ( iloc ) = kloc
C
	CALL ST_FIND ( 'CHC1', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'CMTN', rimnem, NRIMN, kloc, ier )
	imnem ( iloc ) = kloc
C
	CALL ST_FIND ( 'CHC2', dprms, NUMP, iloc, ier )
	imnem ( iloc ) = kloc + 1
C
	CALL ST_FIND ( 'CHC3', dprms, NUMP, iloc, ier )
	imnem ( iloc ) = kloc + 2
C
	CALL ST_FIND ( 'CEIL', dprms, NUMP, iloc, ier )
	imnem ( iloc ) = kloc 
C
	CALL ST_FIND ( 'P03C', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'P03D', rimnem, NRIMN, kloc, ier )
	imnem ( iloc ) = kloc
C
	numprm = NUMP
C*
	RETURN
	END
