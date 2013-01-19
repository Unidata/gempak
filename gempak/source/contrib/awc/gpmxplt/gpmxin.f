        SUBROUTINE GPMXIN ( device, garea, proj, satfil, radfil, 
     +                      panel, text, iret )
C************************************************************************
C* GPMXIN                                                               *
C*                                                                      *
C* This subroutine gets the input for GPMXPLT.                          *
C*                                                                      *
C* GPMXIN  ( DEVICE, GAREA, PROJ, SATFIL, RADFIL, PANEL, TEXT, IRET )   *
C**                                                                     *
C* Log:                                                                 *
C************************************************************************
        CHARACTER*(*)   device, garea, proj, panel,
     +                  satfil, radfil, text
C-----------------------------------------------------------------------
        CALL IP_STR  ( 'DEVICE', device, ier1 )
        CALL IP_STR  ( 'GAREA',  garea,  ier2 )
        CALL IP_STR  ( 'PROJ',   proj,   ier3 )
        CALL IP_STR  ( 'PANEL',  panel,  ier4 )
        CALL IP_STR  ( 'SATFIL', satfil, ier5 )
        CALL IP_STR  ( 'RADFIL', radfil, ier6 )
        CALL IP_STR  ( 'TEXT',   text,   ier7 )
C
        iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7

        IF  ( iret .ne. 0 )  iret = -2
C*
        RETURN
        END
