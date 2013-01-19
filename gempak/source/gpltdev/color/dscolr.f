	SUBROUTINE DSCOLR  ( icolr, jcolr, iret )
C************************************************************************
C* DSCOLR								*
C* 									*
C* This subroutine sets the color number.  Color numbers larger than	*
C* the number of valid colors will be converted, via modular arithmetic,*
C* to a valid color number.  If the color number is negative or zero, 	*
C* no change will be made.						*
C* 									*
C* DSCOLR  ( ICOLR, JCOLR, IRET )					*
C*									*
C* Input parameters:							*
C*	ICOLR		INTEGER 	Color number			*
C*					  <0 = no change		*
C* 									*
C* Output parameters:							*
C*      JCOLR           INTEGER         Color number set                *
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* I. Durham/GSC	 3/98	Added settings for mcolr2, and jcolr2 	*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER 	isend (3), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 3
	isend (2) = CSCOLR
	isend (3) = icolr
C
	CALL GPUT ( isend, 3, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( ircv, 2, ier )
	IF ( ier .ne. NORMAL ) THEN
	    iret = ier
	  ELSE
	    iret   = ircv (1)
	    jcolr  = ircv (2)
	    jcolr2 = ircv (2)
C
C*	    Save ACTIVE color common block variable.
C
	    mcolr  = jcolr
	    mcolr2 = jcolr2
	END IF
C*
	RETURN
	END
