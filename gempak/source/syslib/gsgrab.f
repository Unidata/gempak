      SUBROUTINE GSGRAB  ( mproc, grab, semid, iret )
C************************************************************************
C* GSGRAB								*
C*									*
C* This subroutine is called by subprocesses to obtain a semaphore lock *
C* on the IPC message buffers.                                          *
C*                                                                      *
C* GSGRAB  ( MBCHAN, GRAB, IRET )                                       *
C*                                                                      *
C* Input paramters:                                                     *
C*      MPROC           INTEGER         Subprocess type                 *
C*                                        0 = gplt                      *
C*                                        1 = device driver             *
C*                                                                      *
C* Output parameters:                                                   *
C*      GRAB            INTEGER         Semaphore lock grab status      *
C*                                        0 = grab unsuccessful         *
C*                                        1 = grab successful           *
C*      SEMID           INTEGER         Semaphore ID obtained           *
C*      IRET            INTEGER         Return code                     *
C*					  0 = normal return		*
C*					 -1 = system error		*
C**                                                                     *
C* Log:                                                                 *
C* R. McTaggart-Cowan/SUNY      01/05                                   *
C************************************************************************
        INTEGER         mbchan, grab, semid, iret

        INTEGER         csgrab
        INTEGER         ier
C------------------------------------------------------------------------
C*      Call the C subroutine.
C
        ier = csgrab  ( mproc, grab, semid, ier )
C*
        RETURN
        END
