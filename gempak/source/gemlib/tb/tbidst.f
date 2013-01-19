	SUBROUTINE TB_IDST ( stid, statnam, iret )
C************************************************************************
C* TB_IDST								*
C*									*
C* This subroutine reads and searches a table matching a state's two    *
C* character ID to return the full name of the state.                   *
C*									*
C* TB_IDST ( STID, STATNAM, IRET )					*
C*									*
C* Input parameters:							*
C*	STID   		CHAR*2		State ID			*
C*									*
C* Output parameters:							*
C*	STATNAM  	CHAR*		Station name			*
C*	IRET		INTEGER		Return code			*
C*				  	   0 = normal return		*
C*					  -3 = Couldn't open table	*
C*					 -15 = Id not found in table	*
C**									*
C* Log:									*
C* A. Hardy/GSC		8/99						*
C* A. Hardy/NCEP	5/03		Cleaned up routine		*
C************************************************************************
	CHARACTER*(*) 	statnam, stid 
C*
	CHARACTER  	table*10, type*4, sname*21, state*2
        LOGICAL         found
C------------------------------------------------------------------------
	iret   = 0
        found  = .false.
        statnam= ' '
C
C*	Open file for reading.
C
        table = 'state.tbl'
        type  = 'stns'

	CALL FL_TBOP ( table, type, lun, iret )
        IF ( iret .ne. 0 ) THEN
            iret = -3
            RETURN
        END IF
C
	iostat = 0
	DO WHILE   ( (iostat .eq. 0 ) .and. ( .not. found  ) )
C
C*	    Read the record into a buffer.
C
            state = ' '
            sname = ' '
	    READ ( lun, 500, IOSTAT = iostat )state, sname
500	    FORMAT ( A, 1x, A)
C
C*	    Set output variables.
C                   
            IF ( state .eq. stid ) THEN
                 CALL ST_NULL ( sname, sname, lens, ier)
		 statnam = sname
                 found = .true.
            END IF
	END DO
C
        IF ( .not. found ) THEN
	    iret = -15
        END IF
C
	CALL FL_REWD ( lun, ier )
	CALL FL_CLOS ( lun, ier )
C*
	RETURN
	END
