	SUBROUTINE FL_INQR  ( filnam, exist, newfil, iret)
C************************************************************************
C* FL_INQR								*
C* 									*
C* This subroutine determines whether a file exists.			*
C* 									*
C* FL_INQR  ( FILNAM, EXIST, NEWFIL, IRET )				*
C* 									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name			*
C*									*
C* Output parameters:							*
C*	EXIST		LOGICAL		File exists flag		*
C*	NEWFIL		CHAR*		Expanded file name		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* S. Schotz/GSC	 4/90						*
C* J. Whistler/SSAI	 6/91	Added SS_ENVR to convert sys. variables	*
C* M. desJardins/NMC	 1/92	Add capability to ignore ST_UCLC	*
C* S. Jacobs/NMC	 2/94	Added return of expanded file name	*
C* G. Krueger/EAI        3/94   Changed length of char to 132           *
C* M. desJardins/NMC	 8/94	Check first without changing case	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filnam, newfil
	LOGICAL		exist
C*
	CHARACTER	file*132, flvar*132
	LOGICAL		case
C------------------------------------------------------------------------
	iret = 0
	newfil = ' '
C
C*	Eliminate '^' in first character and set flag to avoid changing
C*	case.
C
	IF  ( filnam (1:1) .eq. '^' )  THEN
	    file = filnam (2: )
	    case = .false.
	  ELSE 
	    file = filnam
	    case = .true.
	END IF
C
C*	Do the INQUIRE with the modified file name.  If the file is
C*	found, we are done.
C
	INQUIRE  ( FILE = file, EXIST = exist, IOSTAT = ier )
	IF  ( exist )  THEN
	    newfil = file
	    RETURN
	  ELSE IF  ( MTMACH .eq. MTVAX )  THEN
	    RETURN
	END IF
C
C*	If the INQUIRE failed, check for a logical name.
C
	CALL SS_ENVR  ( file, flvar, ivar )
	IF  ( ivar .eq. 0 )  THEN
	    INQUIRE  ( FILE = flvar, EXIST = exist, IOSTAT = ier )
	    IF  ( exist )  THEN
		newfil = flvar
		RETURN
	    END IF
	END IF
C
C*	If the INQUIRE has still failed, try converting name to
C*	lower case.
C
	IF  ( .not. case )  RETURN
C*
	CALL ST_UCLC  ( file, file, ier )
	IF  ( ivar .eq. 0 )  THEN
	    CALL SS_ENVR  ( file, flvar, ier )
	  ELSE
	    flvar = file
	END IF
C*
	INQUIRE  ( FILE = flvar, EXIST = exist, IOSTAT = ier )
	IF  ( exist )  THEN
	    newfil = flvar
	END IF
C*
	RETURN
	END
