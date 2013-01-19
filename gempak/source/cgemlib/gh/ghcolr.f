	SUBROUTINE GH_COLR ( tag, idef, iret )
C************************************************************************
C* GH_COLR								*
C*									*
C* This subroutine sets the color for GPTPC subroutines.		*
C*									*
C* GH_COLR ( TAG, IDEF, IRET )  					*
C*									*
C* Input parameters:							*
C*	TAG		CHAR*		Tag for color number		*
C*	IDEF		INTEGER		Default color number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 6/01   					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	tag
C
        CHARACTER	string*30, tag1*33
C-----------------------------------------------------------------------
	iret = 0
C
           CALL ST_NULL ( tag, tag1, lent, ier1)
C
C*	    Retrieve the color from the table.
C
	    CALL GH_GCLR ( tag1, idef, icolr, ired, igreen, 
     +                     iblue, ier )
C
        IF ( ( ( icolr .eq. 1 ) .or. (icolr .eq. 31) .or.
     +       ( icolr .eq. 32 ) .or. (icolr .eq. 101 ) ) .and.
     +       ( ier .eq. 0 ) ) THEN
            icolr =  idef  
            ier = 0
            CALL ST_LSTR ( tag, lent, ier1)
            string =  '<' // tag(:lent) // '>'
            CALL ST_LSTR ( string, lens, ier1)
            CALL ER_WMSG  ( 'GPTPC', 2, string(:lens), ier1 )
        END IF
C
	CALL GSCOLR ( icolr, ier1 )
        IF ( ier .eq. 2 ) THEN
	    CALL GSCRGB ( icolr, ired, igreen, iblue, ier )
        END IF
C*
	RETURN
	END
