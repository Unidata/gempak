	SUBROUTINE GPUTC ( idata, iret )
C************************************************************************
C* GPUTC								*
C*									*
C* This subroutine sends a message to the GPLT subprocess.  The length	*
C* of the message is specified in IDATA (1).				*
C*									*
C* GPUTC ( IDATA, IRET )						*
C*									*
C* Input parameters:							*
C*	IDATA (*)	INTEGER		Message				*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85	GEMPLT 3.1				*
C* M. desJardins/NMC	 7/91	UNIX version; synchronize large buffers	*
C* M. Linda/GSC		 3/96	Created GPUTC based on GPUTB		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GBUFFD.CMN'
C*
	INTEGER		idata (*)
C
	PARAMETER	( IWAIT = 0 )
C
	INTEGER		jdata (128)
	DATA		jdata (1) / -888 /
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	The first word is the length of the buffer to be sent.
C
	nwords = idata (1)
	nsent  = 0
C
C*	Send data in 128 word blocks.
C
	DO WHILE ( nsent .lt. nwords )
	    nw = MIN ( 128, ( nwords - nsent ) )
	    CALL GSEND ( jwtypd, mbchnd, idata (nsent + 1), nw, iret )
	    IF ( iret .ne. 0 ) THEN
		nsent = nwords
	    ELSE
		IF ( nsent .gt. 0 ) THEN
		    CALL GRECV ( jrtypd, IWAIT, mbchnd, jdata, ier )
		END IF
		nsent = nsent + 128
	    END IF
	END DO
C*
	RETURN
	END
