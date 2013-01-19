        SUBROUTINE SC_INTF ( rimnem, cimnem, cprms, imnem, numprm,
     +			     iret )
C************************************************************************
C* SC_INTF                                                              *
C*                                                                      *
C* This subroutine establishes the link between the GEMPAK parameters   *
C* and the interface mnemonics.                                         *
C* 								        *
C* SC_INTF ( RIMNEM, CIMNEM, CPRMS, IMNEM, NUMPRM, IRET )               *
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
C* A. Hardy/GSC		12/97	Based on AF_INTF			*
C* D. Kidwell/NCEP	10/98	Added intf mnemonics to calling sequence*
C************************************************************************
        INCLUDE  	'GEMPRM.PRM'
        INCLUDE  	'sccmn.cmn'
C*
	CHARACTER*(*)	cprms (*), rimnem (*), cimnem (*)
	INTEGER		imnem (*)
C*
        CHARACTER  	dprms (MMPARM)*4
C*
	PARAMETER 	( NUMP = 15 )
	PARAMETER	( NUMEXT = MMPARM - NUMP )
C*
C*	Maintain the order of the following parameter list.  Add new
C*	parameters at the end.
C
        DATA		dprms / 'TDXC', 'TDNC', 'P06I', 'P24I', 
     +                		'WNUM', 'SNOW', 'SNEW', 'WEQS',
     +                		'MSUN', 'CTYL', 'CTYM', 'CTYH',
     +                		'CFRT', 'CFRL', 'CBAS', 
     +                		 NUMEXT * ' ' /           
C*
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
C*	Parm WNUM will require modifications of the decoded values, 
C*	so find corresponding interface mnemonics now.
C
	CALL ST_FIND ( 'WNUM', dprms, NUMP, iloc, ier )
	CALL ST_FIND ( 'WCOD', cimnem, NCIMN, kloc, ier )
	imnem ( iloc ) = kloc
C
	numprm = NUMP
C*
	RETURN
	END
