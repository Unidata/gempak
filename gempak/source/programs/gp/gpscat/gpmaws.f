	SUBROUTINE GPMAWS ( awpsfl, icolor, itype, iwidth, dbug, iret )
C************************************************************************
C* GPMAWS								*
C*									*
C* This subroutine plots the contents of an AWIPS graphics file to the	*
C* current device.							*
C*									*
C* GPMAWS ( AWPSFL, ICOLOR, ITYPE, IWIDTH, DBUG, IRET )			*
C*									*
C* Input parameters:							*
C*	AWPSFL		CHAR*		AWIPS file name			*
C*	ICOLOR		INTEGER		Color for the plot		*
C*	ITYPE		INTEGER		Line type for the plot		*
C*	IWIDTH		INTEGER		Line width for the plot		*
C*      DBUG            CHAR*	        Debug flag                      *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy              8/98	        Created	from gpmafs		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	awpsfl
	CHARACTER*(*)   dbug
C*
	LOGICAL		cont
C-----------------------------------------------------------------------
C*	Save the current line attributes.
C
	CALL GQLINE ( jtyp, jlhw, jwid, jwhw, ier )
C
C*	If the color is not 0, set the color and continue processing.
C
	IF  ( icolor .gt. 0 )  THEN
	    cont = .true.
	    CALL GSCOLR ( icolor, ier )
	    CALL GSLINE ( itype, 0, iwidth, 0, ier )
	  ELSE
	    cont = .false.
	END IF
	IF  ( cont )  THEN
C           
C*	    Plot the graphics.
C
	    CALL AW_READ ( awpsfl, dbug, iret )
	END IF
C
C*	Reset the line attributes.
C
	CALL GSLINE ( jtyp, jlhw, jwid, jwhw, ier )
C*
	RETURN
	END
