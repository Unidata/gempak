	SUBROUTINE GSHASH  ( szhsh, ihwid, ilwid, iret )
C************************************************************************
C* GSHASH								*
C* 									*
C* This subroutine sets the hash mark size, the line width, and the line*
C* spacing.  If these parameters are not positive, no change is made.	*
C*									*
C* GSHASH  ( SZHSH, IHWID, ILWID, IRET )				*
C*                                                                    	*
C* Input parameters:							*
C* 	SZHSH		REAL		Hash mark size multiplier	*
C* 				   	  <=0 = no change		*
C*	IHWID		INTEGER		Hash mark line width multiplier	*
C*					  <=0 = no change		*
C*	ILWID		INTEGER		Hash mark line spacing 		*
C*					  <=0 = no change		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	03/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVSET.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C
C*	First check if this is the current requested characteristics.
C*	If so, do nothing.
C
	IF  ( szhsh .eq. rhshsz .and. ihwid .eq. khwid .and. 
     +        ilwid .eq. klwidh )  THEN
        ELSE
C
C*          Set requested parameters
C
	    IF ( szhsh .gt. 0. ) rhshsz = szhsh
            IF ( ihwid .ge. 1 ) khwid = ihwid
	    IF ( ilwid .ge. 1 ) klwidh = ilwid
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*		Only set if requested different than what is already set
C
                IF ( rhshsz .ne. shshsz .or. khwid .ne. lhwid .or.
     +               klwidh .ne. llwidh ) THEN
		   CALL DSHASH  ( szhsh, khwid, klwidh, 
     +                            shshsz, lhwid, llwidh, iret )
                END IF
	    END IF
	END IF
C*
	RETURN
	END
