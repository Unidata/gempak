	SUBROUTINE DSCLR2  ( icolr, icolr2, jcolr, jcolr2, iret )
C************************************************************************
C* DSCLR2								*
C* 									*
C* This subroutine sets two color numbers.  Color numbers larger than	*
C* the number of valid colors will be converted, via modular arithmetic,*
C* to a valid color number.  If the color number is negative or zero, 	*
C* no change will be made. 						*
C* 									*
C* DSCLR2  ( ICOLR, ICOLR2, JCOLR, JCOLR2, IRET )			*
C*									*
C* Input parameters:							*
C*	ICOLR		INTEGER 	Color number 1			*
C*					  <0 = no change		*
C*	ICOLR2		INTEGER 	Color number 2			*
C*					  <0 = no change		*
C* 									*
C* Output parameters:							*
C*      JCOLR           INTEGER         Color number set 1              *
C*      JCOLR2          INTEGER         Color number set 2              *
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	 3/98						*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER 	isend (4), ircv (3)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 4
	isend (2) = CSCLR2
	isend (3) = icolr
	isend (4) = icolr2
C
	CALL GPUT ( isend, 4, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( ircv, 3, ier )
	IF ( ier .ne. NORMAL ) THEN
	    iret = ier
	  ELSE
	    iret   = ircv (1)
	    jcolr  = ircv (2)
	    jcolr2 = ircv (3)
C
C*	    Save ACTIVE color common block variable.
C
	    mcolr  = jcolr
	    mcolr2 = jcolr2
	END IF
C*
	RETURN
	END
