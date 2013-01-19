	SUBROUTINE SI_GRPT  ( bultin, lenb, ibpnt, stid, report, lenr, 
     +			      iret )
C************************************************************************
C* SI_GRPT							        *
C*							 	        *
C* This subroutine gets the next report from sea ice bulletin.		*
C*								        *
C* SI_GRPT  ( BULTIN, LENB, IBPNT, STID, REPORT, LENR, IRET )	        *
C*								        *
C* Input parameters:						        *
C*	BULTIN		CHAR*		Bulletin		        *
C*	LENB		INTEGER		Bulletin length		        *
C*								        *
C* Input and Output parameters:					        *
C*	IBPNT		INTEGER		Pointer in bulletin	        *
C*								        *
C* Output parameters:						        *
C*	STID  		CHAR*		Station identifier              *
C*	REPORT		CHAR*		Report			        *
C*	LENR		INTEGER		Length of report	        *
C*	IRET		INTEGER		Return code		        *
C*					  0 = normal return	        *
C*					 -1 = no station id found       *
C*					 -2 = no more reports	        *
C**								        *
C* Log:								        *
C* T. Lee/SAIC		 8/02	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	CHARACTER*(*)	bultin, report, stid
C*
	PARAMETER	( MAXCHR = 72 )
C------------------------------------------------------------------------
        iret = 0 
	lenr = 0
C
C*	Check for the end of the bulletin.
C
        IF ( ( ibpnt + 5 ) .ge. lenb ) THEN 
	    report = ' '
            iret = -2
            RETURN
        END IF
C
C*      Find the next report.
C
	ibeg = ibpnt
	ist  = IMISSD
C
	DO WHILE  ( ist .eq. IMISSD .and. ( ( ibeg + 5 ) .le. lenb ) )
	    idx = INDEX ( bultin ( ibeg: ), CHLF )
	    CALL ST_C2I ( bultin ( ibeg: ibeg + 5 ), 1, ist, num, iret )
	    ibpnt = ibeg
	    ibeg  = ibeg + idx
	END DO
C
	iend = ibeg - 1
	report = bultin ( ibpnt: iend )
	ibpnt = iend + 1
	CALL ST_UNPR ( report, MAXCHR, report, lenr, iret )
C*
	RETURN
	END
