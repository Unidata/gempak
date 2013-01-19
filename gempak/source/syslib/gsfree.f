      SUBROUTINE GSFREE  ( mproc, semid, grab, iret )
C************************************************************************
C* GSFREE								*
C*									*
C* This subroutine is called by subprocesses to free a semaphore lock   *
C* on the IPC message buffers.                                          *
C*                                                                      *
C* GSFREE  ( MBCHAN, GRAB, IRET )                                       *
C*                                                                      *
C* Input paramters:                                                     *
C*      MPROC           INTEGER         Subprocess type                 *
C*                                        0 = gplt                      *
C*                                        1 = device driver             *
C*      SEMID           INTEGER         Semaphore ID to free            *
C*                                                                      *
C* Output parameters:                                                   *
C*      GRAB            INTEGER         Semaphore lock release status   *
C*                                        0 = release unsuccessful      *
C*                                        1 = release successful        *
C*      IRET            INTEGER         Return code                     *
C*					  0 = normal return		*
C*					 -1 = system error		*
C**                                                                     *
C* Log:                                                                 *
C* R. McTaggart-Cowan/SUNY      01/05                                   *
C************************************************************************
        INTEGER         mproc, grab, semid, iret

        INTEGER         csfree
        INTEGER         ier
C------------------------------------------------------------------------
C*      Call the C subroutine.
C
        ier = csfree  ( mproc, grab, semid, ier )
C*
        RETURN
        END
