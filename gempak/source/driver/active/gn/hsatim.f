	SUBROUTINE HSATIM ( fil, ixout0, iyout0, ixout1, iyout1,
     +			    ixspc0, iyspc0, ixspc1, iyspc1,
     +			    iret )
C************************************************************************
C* HSATIM - GN								*
C*									*
C* This subroutine displays a satellite image from a file.		*
C*									*
C* HSATIM ( FIL, IXOUT0, IYOUT0, IXOUT1, IYOUT1,			*
C*	    IXSPC0, IYSPC0, IXSPC1, IYSPC1,				*
C*	    IRET )							*
C*									*
C* Input parameters:							*
C*	FIL		CHAR*		File name			*
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
C* J. Cowie/COMET	 1/95	Added image subset args			*
C* J. Cowie/COMET	 5/95	Removed them				*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*) 	fil
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
