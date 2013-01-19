	SUBROUTINE GGETC ( iwait, iret )
C************************************************************************
C* GGETC								*
C*									*
C* This subroutine gets a message from the message queue into /GBUFFD/.	*
C*									*
C* GGETC ( IWAIT, IRET )						*
C*									*
C* Input parameters:							*
C*	IWAIT		INTEGER		Wait/no-wait flag		*
C*					  0 = wait for read		*
C*					<>0 = no wait			*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85	GEMPLT 3.1				*
C* M. desJardins/NMC	 7/91	UNIX version; synchronize large buffers	*
C* M. Linda/GSC		 3/96	Created GGETC based on GGETB		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE 	'GBUFFD.CMN'
C*
	INTEGER		jdata (128)
	DATA		jdata (1) / -888 /
C------------------------------------------------------------------------
	iret = NORMAL
C
	iwat = iwait
	IF ( iwat .ne. 0 ) iwat = -1
C
C*	Read in first buffer, wait if specified.
C
	CALL GRECV ( jrtypd, iwat, mbchnd, igbufd, iret )
C
	IF ( iret .eq. 0 ) THEN
	    nwords = igbufd (1)
	    nrecv  = 128
	ELSE
	    igbufd (1) = 0
	    nwords = 0
	    nrecv  = nwords
	END IF
C
C*	Loop receiving remainig data, if any.
C
	DO WHILE ( nrecv .lt. nwords )
	    CALL GRECV (jrtypd, 0, mbchnd, igbufd (nrecv+1), iret)
	    IF ( iret .ne. 0 ) THEN
		nrecv = nwords
	    ELSE
		nrecv = nrecv + 128
		CALL GSEND ( jwtypd, mbchnd, jdata, 1, ier )
	    END IF
	END DO
C*
	RETURN
	END
