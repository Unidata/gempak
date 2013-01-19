 	SUBROUTINE ST_FWRD  ( string, ifirst, ilast, nword, istrt, 
     +			      iend, iret )
C************************************************************************
C* ST_FWRD								*
C*									*
C* This subroutine returns pointers to the word which is NWORDs after	*
C* the IFIRST character in the string.  Words are assumed to be		*
C* delimited by blanks.  						*
C*									*
C* ST_FWRD  ( STRING, IFIRST, ILAST, NWORD, ISTRT, IEND, IRET )		*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*	IFIRST		INTEGER		First character to check	*
C*	ILAST		INTEGER		Last character to check		*
C*	NWORD		INTEGER		Word number 			*
C*									*
C* Output parameters:							*
C*	ISTRT		INTEGER		Pointer to start of word	*
C*	IEND		INTEGER		Pointer to end of word		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = word not found		*
C**									*
C* Log:									*
C* B. Doty/RDS		 9/87						*
C* M. desJardins/GSFC							*
C************************************************************************
	CHARACTER*(*)	string
C*
	LOGICAL		inword
C------------------------------------------------------------------------
	iret   = 0
	iword  = 0
	inword = .false.
	ichr   = ifirst
C
C*	Check each character until proper word is found.
C
	DO WHILE  ( ( iword .le. nword ) .and. ( ichr .le. ilast ) )
C
C*	    If inside string, check for blank indicating end of string.
C
	    IF  ( inword )  THEN
		IF  ( string ( ichr : ichr ) .eq. ' ' )  THEN
		    inword = .false.
C
C*		    Don't check past correct word.
C
		    IF  ( iword .eq. nword )  ichr = ilast + 1
		  ELSE
		    iend   = ichr
		END IF
C
C*		If not in word, check for start of next word.
C
	      ELSE
		IF  ( string ( ichr : ichr ) .ne. ' ' )  THEN
		    inword = .true.
		    iword  = iword + 1
		    istrt  = ichr
		    iend   = ichr
		END IF
	    END IF
C
C*	    Increment character counter.
C
	    ichr = ichr + 1
	END DO
C
C*	If proper word could not be found, reset pointers.
	IF  ( iword .ne. nword )  THEN
	    istrt = 0
	    iend  = 0
	    iret  = -4
	ENDIF
C*
	RETURN
	END
