	SUBROUTINE RS_GCHR  ( ob, here, endob, numb, shift, iret )
C************************************************************************
C* RS_GCHR								*
C*									*
C* This subroutine finds a group of capital letters in OB, updates the	*
C* index pointer to OB, and returns the difference between the new	*
C* position and the old position.  The letters need not be surrounded	*
C* by spaces.								*
C*									*
C* RS_GCHR  ( OB, HERE, ENDOB, NUMB, SHIFT, IRET )			*
C*									*
C* Input parameters:							*
C*	OB		CHAR*		Observation or bulletin		*
C*	ENDOB		INTEGER		Length of OB			*
C*	NUMB		INTEGER		Number of consecutive capital 	*
C*					 letters wanted			*
C* Input/output parameters:						*
C*	HERE		INTEGER		Current position in OB		*
C*									*
C* Output parameters:							*
C*	SHIFT		INTEGER		Change in HERE			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal completion		*
C*					 -1 = no capitals found		*
C**									*
C* Log:									*
C* J. Nielsen/MIT	10/86						*
C* J. Nielsen/MIT	 2/89						*
C* J. Nielsen/MIT	 2/92	Gempacized				*
C* T. Lee/GSC		 9/97	Removed HERE1				*
C************************************************************************
	CHARACTER*(*)	ob
	INTEGER		endob, numb
C*
	INTEGER		here
C*
	INTEGER		shift, iret
C*
	LOGICAL		done
C-----------------------------------------------------------------------
	iret = 0
	shift = 0
	done = .false.
	IF  ( here + numb - 1 .ge. endob )  THEN
	    iret = -1
	    RETURN
	ENDIF
	DO WHILE  ( .not. done )
C
C*	    Look for a capital letter
C
	    DO WHILE  ( ( ob(here:here) .lt. 'A' ) .or. 
     +			( ob(here:here) .gt. 'Z' ) )
	        here = here + 1
		shift = shift + 1	
		IF  ( here + numb - 1 .gt. endob )  THEN
		    iret = -1
		    RETURN
		ENDIF
	    END DO
C
C*	    See if we have found a group of capital letters of the 
C*	    required length.
C
	    i = 1
	    DO WHILE  ( ( i .lt. numb ) .and.
     +		 	( ob (here+i:here+i) .ge. 'A' ) .and.
     +			( ob (here+i:here+i) .le. 'Z' ) )
		i = i + 1
	    END DO
C
C*	    Find enough?
C
	    IF  ( i .eq. numb )  THEN
		done = .true.
	    ELSE
		here = here + 1
		shift = shift + 1
	    ENDIF
	END DO
	RETURN
	END

