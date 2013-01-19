	SUBROUTINE IP_LCTL  ( input, iret )
C************************************************************************
C* IP_LCTL								*
C*									*
C* This subroutine processes the user input for animation control.	*
C* 									*
C* Valid loop commands are:						*
C*	;[1-5]		Set dwell rate group [1-5]			*
C*	;LOOP		Start the animation				*
C*	;HALT		Stop the animation				*
C*	;STEP		Step forward one frame				*
C*	;BACK		Step backward one frame				*
C*	;REVERSE	Start the reverse animation			*
C*	;ROCK		Start the rocking animation			*
C*									*
C* IP_LCTL  ( INPUT, IRET )						*
C*									*
C* Input parameters:							*
C*	INPUT		CHAR*		Loop control input string	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C**									*
C* Log:									*
C* S. Jacobs/EAI	 2/94		Copied from NT_DYNM		*
C* S. Jacobs/NMC	 8/94		Renumbered commands		*
C* K. Tyle/GSC		 7/96		Renamed from NT_LCTL		*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	input
C*
	CHARACTER	input2*72
	LOGICAL		numb, loop, halt, step, back, rvrs, rock
C*
	PARAMETER	( MXDWEL =   5 )
	PARAMETER	( CMDDWL = 100 )
C-----------------------------------------------------------------------
	iret  = 0
C
C*	Check to see which command this is.
C
	numb = .false.
	loop = .false.
	halt = .false.
	step = .false.
	back = .false.
	rvrs = .false.
	rock = .false.
	CALL ST_NUMB  ( input, inum, ier )
	IF  ( ier .eq. 0 )  numb = .true.
	IF  ( .not. numb )  THEN
	  CALL ST_LDSP  ( input, input, nc, ier )
	  CALL ST_LCUC  ( input, input2, ier )
	  CALL ST_ABBR  ( 'LOOP', input2, loop, ier )
	  IF  ( .not. loop )  THEN
	    CALL ST_ABBR  ( 'HALT', input2, halt, ier )
	    IF  ( .not. halt )  THEN
	      CALL ST_ABBR  ( 'STEP', input2, step, ier )
	      IF  ( .not. step )  THEN
		CALL ST_ABBR  ( 'BACK', input2, back, ier )
		IF  ( .not. back )  THEN
		  CALL ST_ABBR  ( 'REVERSE', input2, rvrs, ier )
		  IF  ( .not. rvrs )  THEN
		    CALL ST_ABBR  ( 'ROCK', input2, rock, ier )
		  END IF
		END IF
	      END IF
	    END IF
	  END IF
	END IF
C
C*	Process user input2.
C
	IF  ( numb )  THEN
	    IF  ( ( inum .ge. 1 ) .and. ( inum .le. MXDWEL ) )  THEN
	    	icomm = inum + CMDDWL
	    ELSE
		ier = -12
		CALL ER_WMSG  ( 'IP', ier, input2, ierr )
		RETURN
	    END IF
	ELSE IF  ( loop )  THEN
	    icomm = 1
	ELSE IF  ( halt )  THEN
	    icomm = 2
	ELSE IF  ( step )  THEN
	    icomm = 3
	ELSE IF  ( back )  THEN
	    icomm = 4
	ELSE IF  ( rvrs )  THEN
	    icomm = 5
	ELSE IF  ( rock )  THEN
	    icomm = 6
	ELSE
	    ier = -5
	    CALL ER_WMSG  ( 'IP', ier, input2, ierr )
	    RETURN
	END IF
C
C*	Process the animation command.
C
	CALL GLOOPC ( icomm, ier )
C*
	RETURN
	END
