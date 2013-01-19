	SUBROUTINE GGETB  ( iret )
C************************************************************************
C* GGETB								*
C*									*
C* This subroutine gets an entire message for the GPLT task.		*
C*									*
C* GGETB  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85	GEMPLT 3.1				*
C* M. desJardins/NMC	 7/91	UNIX version; synchronize large buffers	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE 	'GBUFF.CMN'
C*
	PARAMETER	( IWAIT = 0 )
	INTEGER		jdata (128)
	DATA		jdata (1) / -888 /
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Read in the first buffer.
C*	The first word is the length of the buffer to be received.
C
	CALL GRECV  ( jrtype, IWAIT, mbchna, igbuff, iret )
C
C*	Proceed if there was no error.
C
	IF  ( iret .eq. 0 )  THEN
	    nwords = igbuff (1)
	    nrecv  = 128
	  ELSE
	    nwords = 0
	    igbuff (1) = 0
	    nrecv  = nwords
	END IF
C
C*	Loop receiving the rest of the data.
C
	DO WHILE  ( nrecv .lt. nwords )
	    CALL GRECV (jrtype, IWAIT, mbchna, igbuff (nrecv+1), iret)
	    IF  ( iret .ne. 0 ) THEN
		nrecv = nwords
	      ELSE
		nrecv = nrecv + 128
		CALL GSEND  ( jwtype, mbchna, jdata, 1, ier )
	    END IF
	END DO
C*
	RETURN
	END
