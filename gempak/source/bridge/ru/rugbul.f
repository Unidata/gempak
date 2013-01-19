	SUBROUTINE RU_GBUL  ( ilun, bultin, lenb, iret ) 
C************************************************************************
C* RU_GBUL								*
C*									*
C* This subroutine returns the next bulletin from an upper-air		*
C* archive file.  The file must contain  <CNTL>C  to indicate the	*
C* end of a bulletin.  Each bulletin must start on a new line. If	*
C* the bulletin terminator is not reported, it must be added before	*
C* this subroutine is called.  The file must already be open.  All	*
C* unprintable characters are removed from the bulletin.		*
C*									*
C* RU_GBUL  ( ILUN, BULTIN, LENB, IRET )				*
C*									*
C* Input parameters:							*
C*	ILUN		INTEGER		Logical unit number 		*
C*									*
C* Output parameters:							*
C*	BULTIN		CHAR*		Bulletin 			*
C*	LENB		INTEGER		Bulletin length			*
C*	IRET 		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = no more bulletins		*
C**									*
C* Log:									*
C* B. Doty/RDS		 9/87						*
C* M. desJardins/GSFC	12/87	Rewritten				*
C* J. Whistler/SSAI	 5/91	Put Control C definition in GEMPRM.PRM	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	bultin
C*
	CHARACTER	record*80
	LOGICAL		done
C----------------------------------------------------------------------
	iret = 0 
	lens = LEN ( bultin )
C
C*	Loop through records searching for an and of bulletin.
C
	done = .false.
	ilen = 0  
	DO WHILE  ( ( .not. done ) .and. ( ilen .lt. lens - 80 ) ) 
	    READ  ( ilun, 9000, IOSTAT = iostat )  record
9000	    FORMAT  ( A )
C
C*	    Check for the end of a bulletin.
C
	    ipos = INDEX  ( record, CHCTLC )
C
C*	    If this is the end of the file and no bulletin has been
C*	    read, set return code.
C
	    IF  ( iostat .ne. 0 )  THEN
		IF  ( ilen .eq. 0 )  iret = -1
		done = .true.
C
C*		Add record to bulletin if an end of bulletin was
C*		not encountered.
C
	      ELSE IF  ( ipos .eq. 0 )  THEN
		bultin  ( ilen+1 : ilen+80 ) = record
		ilen = ilen + 80
C
C*		If at end of bulletin, set flag.
C
	      ELSE
		bultin  ( ilen+1 : ilen+ipos ) = record ( 1 : ipos )
		ilen = ilen + ipos
 		done = .true.
	    END IF
	END DO
C
C*	Remove unprintable characters from bulletin.
C
	CALL ST_UNPR  ( bultin, ilen, bultin, lenb, ier )
C*
	RETURN
	END
