	SUBROUTINE HSATIM ( fil, ixout0, iyout0, ixout1, iyout1,
     +			    ixspc0, iyspc0, ixspc1, iyspc1,  iret )
C************************************************************************
C* HSATIM - PS								*
C*									*
C* This subroutine displays a satellite image from a file.		*
C*									*
C* HSATIM ( FIL, IXOUT0, IYOUT0, IXOUT1, IYOUT1,			*
C*	    IXSPC0, IYSPC0, IXSPC1, IYSPC1, IRET )			*
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
C* S. Jacobs/NCEP	 1/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	CHARACTER*(*) 	fil
C*
	CHARACTER	filnam*132
C------------------------------------------------------------------------
C
C*	Drop the satellite image.
C
	CALL ST_NULL ( fil, filnam, ilen, ier )
	CALL PSATIM  ( filnam, ixout0, iyout0, ixout1, iyout1, iret )
C*
	RETURN
	END
