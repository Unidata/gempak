	SUBROUTINE SC_GRPT  ( bultin, lenb, ibpnt, stid, report, lenr, 
     +			      iret )
C************************************************************************
C* SC_GRPT							        *
C*							 	        *
C* This subroutine gets the next report from an SCD bulletin.           *
C* Reports must begin with a three or four-character station id,        *
C* followed by the group 'SCD'.                                         *
C*								        *
C* SC_GRPT  ( BULTIN, LENB, IBPNT, STID, REPORT, LENR, IRET )	        *
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
C* D. Kidwell/NCEP 	 3/97	Based on MT_GRPT                        *
C* A. Hardy/GSC         12/97   Added sccmn.cmn for interface           *
C* J. Ator/NCEP		12/01   Added check for ibeg < iend		*
C************************************************************************
        INCLUDE         'sccmn.cmn'
C
	CHARACTER*(*)	bultin, report, stid
C*
	CHARACTER	string*400
	LOGICAL 	found
C------------------------------------------------------------------------
        iret = 0 
	lenr = 0
C
C*	Check for the end of the bulletin.
C
        IF ( ibpnt .ge. lenb ) THEN 
	    report = ' '
            iret = -2
            RETURN
        END IF
C
C*      Find the next station id.
C
	iscd = INDEX ( bultin ( ibpnt: ), ' SCD ')
	IF ( iscd .ne. 0 ) THEN
	    ibeg = ibpnt + iscd - 5
	    IF ( ibeg .lt. ibpnt ) ibeg = ibpnt
	    CALL ST_LDSP ( bultin ( ibeg: ), string, lens, iret )
	    iend = INDEX ( string ( :lens ), ' ' )
	    IF ( iend .eq. 4 .or. iend .eq. 5 ) THEN
	        found = .true.
	        stid = string ( :iend - 1 )
	      ELSE
	        found = .false.
	    END IF
	  ELSE
	    found = .false.
	END IF
C
C*      Locate the end of the report, either by finding the start of
C*      the next report or by using the end of the bulletin or by
C*	finding the character '='.
C
	IF ( found ) THEN
	    iend = lenb
	    iscd2 = INDEX ( bultin ( ibpnt + iscd: ) , ' SCD ')
	    IF ( iscd2 .ne. 0 ) THEN
	        iend = ibpnt + iscd + iscd2 - 7
	        IF ( bultin ( iend:iend ) .ne. ' ' ) iend = iend + 1
	    END IF
	    ibpnt = iend + 1
	    ieql = INDEX ( bultin ( ibeg:iend ), '=' )
	    IF ( ieql .ne. 0 ) iend = ibeg + ieql - 2
	    IF  ( ibeg .lt. iend )  THEN
	        report = bultin ( ibeg:iend )
	        lenr   = iend - ibeg + 1
	      ELSE
		report = ' '
		ibpnt  = lenb
		iret   = -2
	    END IF
	  ELSE
C
C*	    No station id was found where expected.
C
	    report = ' '
	    ibpnt = lenb
	    iret = -1
	END IF
C*
	RETURN
	END
