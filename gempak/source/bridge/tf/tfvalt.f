        SUBROUTINE TF_VALT ( irtarr, jrtarr, maxtim, vtimflag)
C**********************************************************************
C* TF_VALT
C* This routine is used to validate the TAF time.  It evaluates whether
C* the TAF time (jrtarr) is between the system time hour (irtarr) and
C* the time (maxtim - 1) hours from irtarr.
C*
C* TF_VALT ( irtarr, jrtarr, maxtim, vtimflag )
C*
C* Input parameters:
C*   IRTARR (5)     INTEGER  Integer report time array
C*   JRTARR (5)     INTEGER  Integer forecast time array
C*   maxtim         INTEGER  The maximum number of times contained in
C*                           the file.
C* Output parameters:
C*   validtim       LOGICAL  The flag used to determine if this is a
C*                           valid time.
C**
C* Log:
C* L. Hinson        06/08    Created
C*
C**********************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'BRIDGE.PRM'
C*
        INTEGER    irtarr (*), jrtarr (*)
        INTEGER    irtarr1 (5), etarr (5)
        INTEGER    maxtim, ier
        LOGICAL    vtimflag
        vtimflag = .true.
        CALL TI_SUBM(irtarr, irtarr(5), irtarr1, ier)
        CALL TI_MDIF(jrtarr, irtarr1, imdiff, ier)
        IF (ier .eq. 0 .and. imdiff .ge. 0) THEN
           CALL TI_ADDM(irtarr1, 60*(maxtim-1), etarr, ier)
           CALL TI_MDIF(jrtarr, etarr, imdiff, ier3)
           IF (ier .eq. 0 .and. imdiff .gt. 0) THEN
             vtimflag = .false.
           ENDIF
        ELSE
           vtimflag = .false.
        ENDIF
C*
        RETURN
        END       
        
