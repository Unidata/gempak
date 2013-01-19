	SUBROUTINE HSATIM ( fil, ixout0, iyout0, ixout1, iyout1,
     +			    ixspc0, iyspc0, ixspc1, iyspc1,  iret )
C************************************************************************
C* HSATIM - XWP								*
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
C* J. Whistler/SSAI	12/91						*
C* G. Krueger/EAI	12/93	Modified hrest -> hsatim		*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* J. Cowie/COMET	 1/95	Added image subsetting arguments	*
C* J. Cowie/COMET	 5/95	Removed them				*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Jacobs/NCEP	 1/97	Added PSATIM				*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* T. Piper/SAIC	09/06	CSC on xsatim; removed ilen		*
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*) 	fil
C*
	CHARACTER	imgnam*133
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C
	CALL ST_NULL ( fil, imgnam, ilen, ier )
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
C
C*	    Drop the satellite image.
C
	    CALL XSATIM ( imgnam, ixout0, iyout0, ixout1, iyout1,
     +			  iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    CALL PSATIM ( imgnam, ixout0, iyout0, ixout1, iyout1,
     +			  iret )
	END IF
C*
	RETURN
	END
