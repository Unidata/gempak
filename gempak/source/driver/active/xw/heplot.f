	SUBROUTINE HEPLOT  ( iret )
C************************************************************************
C* HEPLOT - XW								*
C* 									*
C* This subroutine ends plotting on the device.				*
C* 									*
C* HEPLOT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	 2/92	Move flushing from HFLUSH to HEPLOT	*
C* P. Bruehl/Unidata	 9/93	Added driver.cmn include file		*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* C. Lin/EAI		 8/94	Changed call to XXFLSH			*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
C*	Flush the graphics to the window.
C
        CALL XXFLSH ( .false., iret )
C*
	RETURN
	END
