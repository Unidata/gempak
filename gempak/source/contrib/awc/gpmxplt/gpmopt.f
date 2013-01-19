        SUBROUTINE GPMOPT ( device, garea, proj, satfil, radfil, 
     +                      panel, text, iret )
C************************************************************************
C* GPMOPT                                                               *
C*                                                                      *
C* This subroutine displays a summary of the users options.             *
C*                                                                      *
C* GPMOPT ( DEVICE, GAREA, PROJ, SATFIL, RADFIL, PANEL, IRET )          *
C*                                                                      *
C* Input Parameters:                                                    *
C*      DEVICE          CHAR*           Device                          *
C*      GAREA           CHAR*           Graphics area name              *
C*      PROJ            CHAR*           Projection                      *
C*      SATFIL          CHAR*           Map options                     *
C*      RADFIL          CHAR*           Title input                     *
C*      PANEL           CHAR*           Panel input                     *
C*      TEXT            CHAR*           Text attribute input            *
C*                                                                      *
C* Output Parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                       -1 = exit                      *
C**                                                                     *
C* Log:                                                                 *
C* S. Schotz/GSC         8/90   GEMPAK5                                 *
C* M. desJardins/NMC     8/94   Fixed spelling error                    *
C************************************************************************
        CHARACTER*(*)   device, proj, garea, panel, satfil, radfil, text
C*
        LOGICAL         respnd
        CHARACTER       clr*3
C*
C------------------------------------------------------------------------
C*
        iret = 0
        WRITE ( 6, 5000) device, proj, garea, satfil, radfil, 
     +                   panel, text
C*
        CALL IP_RESP ( respnd, ier )
        IF  ( respnd ) THEN
            CALL TM_ACCP ( ier )
            IF ( ier .eq. 2 ) iret = -1
        END IF
C*
5000    FORMAT ( ' GPMXPLT PARAMETERS:',//
     +           ' Device:              ', A,/
     +           ' Projection:          ', A,/
     +           ' Graphics area name:  ', A,/
     +           ' Satellite file:      ', A,/
     +           ' Radar file:          ', A,/
     +           ' Panel:               ', A,/
     +           ' Text:                ', A)
C*
        RETURN
        END
