	SUBROUTINE DSATIM ( filnam, ixout0, iyout0, ixout1, iyout1,
     +			    ixspc0, iyspc0, ixspc1, iyspc1,
     +			    iret )
C************************************************************************
C* DSATIM								*
C*									*
C* This subroutine displays a satellite image from a satellite image	*
C* file.								*
C*									*
C* DSATIM ( FILNAM, IXOUT0, IYOUT0, IXOUT1, IYOUT1,			*
C*	    IXSPC0, IYSPC0, IXSPC1, IYSPC1,				*
C*	    IRET )							*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Satellite file name		*
C*      IXOUT0          INTEGER         Image space                     *
C*      IYOUT0          INTEGER         Image space                     *
C*      IXOUT1          INTEGER         Image space                     *
C*      IYOUT1          INTEGER         Image space                     *
C*      IXSPC0          INTEGER         View space                      *
C*      IYSPC0          INTEGER         View space                      *
C*      IXSPC1          INTEGER         View space                      *
C*      IYSPC1          INTEGER         View space                      *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Krueger/EAI	 2/94						*
C* S. Jacobs/NMC	 3/94		Clean up			*
C* J. Cowie/COMET	 1/95		Image subsetting		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C									
	CHARACTER 	filnam*(*)
C------------------------------------------------------------------------
C
	iret = 0
     	CALL HSATIM ( filnam, ixout0, iyout0, ixout1, iyout1,
     +		      ixspc0, iyspc0, ixspc1, iyspc1,
     +		      iret )
C
	RETURN
	END
