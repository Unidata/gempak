	SUBROUTINE RS_GGRP ( ob, here, endob, numb, shift, iret )
C************************************************************************
C* RS_GGRP								*
C*									*
C* This subroutine finds a group of numbers in OB, updates the index	*
C* pointer to OB, and returns the difference between the new position	*
C* and the old position.  A number is defined as any digit between 0 	*
C* and 9, or a slash (/).  The OB must be a continuous string of digits	*
C* and spaces.  The numbers need not be surrounded by spaces.		*
C*									*
C* RS_GGRP  ( OB, HERE, ENDOB, NUMB, SHIFT, IRET )			*
C*									*
C* Input parameters:							*
C*	OB		CHAR*		Encoded observation		*
C*	ENDOB		INTEGER		Length of OB			*
C*	NUMB		INTEGER		Number of consecutive integers	*
C*					 to search for			*
C*									*
C* Input and output parameters:						*
C*	HERE		INTEGER		Current position in OB		*
C*									*
C* Output parameters:							*
C*	SHIFT		INTEGER		Total change of HERE		*
C*	IRET		INTEGER		Return code			*
C*					 -1 = Failed to find group	*
C*					  0 = Successful		*
C*					  3 = Found '333' group instead	*
C*					  4 = Found '444' group instead *
C*					  5 = Found '555' group instead *
C**									*
C* Log:									*
C* J. Nielsen/MIT	10/86						*
C* J. Nielsen/MIT	 2/89						*
C* F. Cuq/UCLA		 8/90	Integer*2 -> Integer*4			*
C* J. Nielsen/TAMU	 2/92	Gempacized				*
C************************************************************************
	CHARACTER*(*)	ob
	INTEGER		endob, numb
C*
	INTEGER		here
C*
	INTEGER		shift, iret
C*
	INTEGER		here1
	LOGICAL		RS_NMER, done
C------------------------------------------------------------------------
	shift = 0
	iret = 0
	here1 = here
	done = .false.
C
C*	Loop to look for NUMB consectutive digits
C
	DO WHILE  ( .not. done )
C
C*	  Search for first non-blank character
C
	  DO WHILE  ( ob ( here:here ) .eq. ' ' )
C
C*	    Make sure there's enough space left for digits
C
	    IF  ( ( here + numb - 1 ) .gt. endob )  THEN
		iret = -1
		here = here1
		shift = 0
		RETURN
	    ENDIF
C
C*	    Try next character
C
	    here = here + 1
	    shift = shift + 1
	  END DO
C
C*	  Look for enough numbers and/or slashes
C
	  i = 0
	  DO WHILE  ( ( i .lt. numb ) .and. 
     +		      ( ( RS_NMER ( ob ( here+i : here+i ), 1 ) ) .or.
     +		        ( ob ( here+i : here+i ) .eq. '/' ) ) )
	    i = i + 1
	  END DO
C
C*	  Find enough?
C
	  IF  ( i .eq. numb )   THEN
	    done = .true.
	  ELSE
C
C*	    Check for special 3-digit code group
C
	    IF  ( i .eq. 3 )  THEN
	      IF  ( ob ( here:here+2 ) .eq. '333' )  THEN
		iret = 3
		RETURN
	      ELSE IF  ( ob ( here:here+2 ) .eq. '444' )  THEN
		iret = 4
		RETURN
	      ELSE IF  ( ob ( here:here+2 ) .eq. '555' )  THEN
		iret = 5
		RETURN
	      ENDIF
	    ENDIF
C
C*	    Look for next space, to start over
C
	    here = here + i
	    shift = shift + i
	    DO WHILE  ( ob ( here:here ) .ne. ' ' )
C
C*	      Make sure there's enough space left for digits
C
	      IF  ( ( here + numb ) .gt. endob )  THEN
		iret = -1
		here = here1
		shift = 0
		RETURN
	      ENDIF
C
C*	      Try next character
C
	      here = here + 1
	      shift = shift + 1
	    END DO
	  ENDIF
C
C*	Go back to the beginning, if necessary
C
	END DO
C
	RETURN
	END
