	SUBROUTINE GD_CPF ( parmin, parmout, iret )
C************************************************************************
C* GD_CPF                                                               *
C*                                                                      *
C* This subroutine checks if a file containing a latitude/longitude 	*
C*	pair(s) was input.     						*
C*                                                                      *
C* GD_CPF  ( PARMIN, PARMOUT, IRET )                                    *
C*                                                                      *
C* Input parameters:                                                    *
C*      PARMIN		CHAR*		GPOINT or CXSTNS parameter	*
C*                                                                      *
C* Output parameters:                                                   *
C*	PARMOUT		CHAR*		latitude/longitude pair(s)	*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*					 -6 = open/read error		*
C**                                                                     *
C* Log:                                                                 *
C* T. Piper/SAIC	06/04	Created                                 *
C************************************************************************
	CHARACTER*(*) 	parmin, parmout
	CHARACTER	clat*8, clon*10, endp1*24, endp2*28
	REAL		xlat(2), xlon(2)
C------------------------------------------------------------------------
	iret = 0
C
C*	If a ';' is included, it is not a file name.
C*	This will eliminate grid point pairs and lat/lon pairs.
C
	isemic = INDEX ( parmin, ';' ) 
	idot = INDEX ( parmin, '.' )
	IF ( isemic .gt. 0 ) THEN
	    parmout = parmin
	    return
	ELSE IF ( idot .gt. 0 ) THEN
C
C*	This is a file because the only other valid inputs that are NOT
C*	grid points or lat/lon points are station numeric or character
C*	identifiers, which should not have a '.' in them.
C
	    ndec = 4
	    CALL ST_NULL ( parmin, parmin, lenin, ier )
	    CALL CTB_RDCPF ( parmin, npnts, xlat, xlon, iret )
	    IF ( iret .eq. 0 ) THEN
	        CALL ST_RLCH ( xlat(1), ndec, clat, ier )
	        CALL ST_RLCH ( xlon(1), ndec, clon, ier )
		endp1 = clat // ' ; ' // clon
		IF ( npnts .eq. 1 ) THEN
		    endp2 = ' '
		ELSE
		    CALL ST_RLCH ( xlat(2), ndec, clat, ier )
                    CALL ST_RLCH ( xlon(2), ndec, clon, ier )
		    endp2 = ' > ' // clat // ' ; ' // clon
		END IF
		parmout = endp1 // endp2
	    ELSE
		iret = -6
		parmout = ' '
	    END IF
	END IF
C
	RETURN
	END
