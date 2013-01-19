	SUBROUTINE HCLOSP  ( ncurwn, iret )
C************************************************************************
C* HCLOSP - GIF								*
C* 									*
C* This subroutine closes the plot file.  Before closing the file, all	*
C* buffered data must be flushed.					*
C*									*
C* HCLOSP  ( NCURWN, IRET )						*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* M. desJardins/GSFC	12/90						*
C* J. Nielsen-G/TAMU	12/96						*
C* T. Lee/GSC		 7/00	Added ncurwn to calling seq.; Renamed	*
C*				gdr_closp to wclosp			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'driver.cmn'
C------------------------------------------------------------------------
	iret = NORMAL
	ncurwn = 0
C
C*	Close the GIF file.
C
	IF  ( opnfil )  THEN
	    CALL WCLOSP ( filnam, gfplot, iret )
	    opnfil = .false.
	END IF	
C*
	RETURN
	END
