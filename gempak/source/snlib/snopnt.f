	SUBROUTINE SN_OPNT  ( sndtbl, lun, parms, nparms, iret )
C************************************************************************
C* SN_OPNT								*
C*									*
C* This subroutine opens a standard sounding table and checks the 	*
C* parameters in that table.						*
C*									*
C* SN_OPNT  ( SNDTBL, LUN, PARMS, NPARMS, IRET )			*
C*									*
C* Input parameters:							*
C*	SNDTBL		CHAR*		Sounding table name		*
C*									*
C* Output parameters:							*
C*	LUN		INTEGER		LUN for sounding table		*
C*	PARMS (NPARMS)	CHAR*		Parameters			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -2 = could not open file	*
C*					-26 = too many parameters	*
C*					-27 = no parameter is found	*
C*					-28 = error reading parameters	*
C**									*
C* Log:									*
C* T. Lee/GSC	 	12/99	Adapted from SNEPRM			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms (*), sndtbl
C*
	CHARACTER	record*132, pname (20)*4
	LOGICAL		found, more
C------------------------------------------------------------------------
	iret   = 0
	nparms = 0
C
C*	Open the file.
C
	CALL FL_TBOP  ( sndtbl, 'config', lun, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -2
	    lun  = 0
	    RETURN
	END IF
C
C*	Loop through records looking for the record containing
C*	parameter names.
C
	ier   = 0
	found = .false.
	DO WHILE  ( ier .eq. 0 )
	    READ   ( lun, 10, IOSTAT = ier )  record
10	    FORMAT ( A )
	    CALL ST_LCUC  ( record, record, ier2 )
C
C*	    Check for "PARM".
C
	    IF  ( ier .eq. 0 )  THEN
		ip = INDEX  ( record, 'PARM' )
		IF  ( ip .gt. 0 )  THEN
		    ieq = INDEX  ( record, '=' )
		    IF  ( ieq .gt. 0 )  THEN
			found = .true.
			ier   = -1
		    END IF
		END IF
	    END IF
	END DO
C
C*	If PARM was not found, close file and return error.
C
	IF  ( .not. found )  THEN
	    CALL FL_CLOS  ( lun, ier )
	    lun  = 0
	    iret = -27
	    RETURN
	END IF
C
C*	Get parameter names from this record.
C
	record = record ( ieq+1 : )
	DO WHILE  ( record .ne. ' ')
C
C*	    Check to see if this is the last parameter record.
C
	    CALL ST_LSTR  ( record, last, ier )
	    IF  ( record ( last : last ) .eq. ';' )  THEN
		more = .true.
	      ELSE
		more = .false.
	    END IF
C
C*	    Break record into substrings.
C
	    CALL ST_C2C  ( record, 20, pname, npnam, ier )
C
C*	    Check that there was something in the record.
C
	    IF  ( npnam .eq. 0 )  THEN
		record = ' '
C
C*		Add these parameters to the current list.
C
	      ELSE
		DO  i = 1, npnam
C
C*		Check that there are not too many parameters.
C
		    IF  ( nparms .ge. MMPARM )  THEN
			CALL FL_CLOS  ( lun, ier )
			lun  = 0
			iret = -26
			RETURN
		      ELSE
			nparms = nparms + 1
			parms ( nparms ) = pname (i)
		    END IF
		END DO
C
C*		Read in next record.
C
		IF  ( more )  THEN
		    READ   ( lun, 10, IOSTAT= ier )  record
		    CALL ST_LCUC  ( record, record, ier2 )
		    IF  ( ier .ne. 0 )  THEN
			CALL FL_CLOS  ( lun, ier )
			lun  =  0
			iret = -28
			RETURN
		    END IF
		  ELSE
		    record = ' '
		END IF
	    END IF
	END DO
C*
	RETURN
	END
