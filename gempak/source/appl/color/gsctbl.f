	SUBROUTINE GSCTBL  ( ctblnm, iret )
C************************************************************************
C* GSCTBL								*
C* 									*
C* This subroutine defines the color table.             		*
C* 									*
C* GSCTBL  ( CTBLNM, IRET )						*
C* 									*
C* Input parameters:							*
C*	CTBLNM		CHAR*		Color table name		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/EAI	11/92						*
C* A. Chang/EAI		12/93						*
C* L. Williams/EAi       3/94   Removed blank comments from header and  *
C*                              changed file description, length of the *
C*                              input string in characters and word, and*
C*                              the values of mmm and imesg.            *
C************************************************************************
        INCLUDE         'FUNCCODE.PRM'
        INCLUDE         'ERROR.PRM'
        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER*(*)   ctblnm
C*
        CHARACTER       mmm*72
        INTEGER         imesg (20), isend (2)
C------------------------------------------------------------------------
C*      Find the length of the input string in characters and word.
C
        mmm = ctblnm
        lenc = 72
        lenw = 18
C
C*      Load input parameters into the buffers then write them to
C*      the mailbox.
C
        isend (1) = lenw + 2
        isend (2) = FSCTBL
C
        CALL GPUT  ( isend, 2, iret )
        IF  ( iret .ne. NORMAL )  RETURN
C
C*      Convert the character string to an integer array and send 
C
        CALL ST_STOI  ( mmm, lenc, nv, imesg, iret )
        CALL GPUT  ( imesg, lenw, iret )
        IF  ( iret .ne. NORMAL )  RETURN
C
        CALL GGET  ( iret , 1, ierr )
        IF  ( ierr .ne. NORMAL )  iret = ierr
C*
        RETURN
        END

