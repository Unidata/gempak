	SUBROUTINE HROAM  ( ityp, x, y, iret )
C************************************************************************
C* HROAM - XWP								*
C* 									*
C* This subroutine roams the window at the specified position on        *
C* current driver.  							*
C* 									*
C* HROAM  ( ITYP, X, Y, IRET )						*
C*									*
C* Input parameters:                                                    *
C*      ITYP      INTEGER         The base point of roam          	*
C*                                        0 = upper left screen corner  *
C*                                        1 = center of the screen      *
C*                                        2 = delta_x, delta_y      	*
C*      X          REAL         Upper left window x coordinates         *
C*      Y          REAL         Upper left window y coordinates         *
C*                                                                      *
C* Output parameters:							*
C*	IRET	   INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI	 	 6/97						*
C* S. Maxwell/GSC	 6/97	Documentation changes			*
C************************************************************************
        INCLUDE         'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*      Check the requested device.
C*      Do roam in XW only.
C
        IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
C
            CALL XROAM ( ityp, x, y, ix, iy, iret )
C
C*          Set the offsets in DEVCHR.CMN
C
            IF ( iret .eq. NORMAL ) THEN
                isxoff = ix
                isyoff = iy
            END IF
C
        END IF
C*
	RETURN
	END
