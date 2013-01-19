	SUBROUTINE GSVIEW  ( xllf, yllf, xurf, yurf, iret )
C************************************************************************
C* GSVIEW								*
C* 									*
C* This subroutine sets the boundaries of the view region to be used 	*
C* to display the plot.  The view region is specified using fractions	*
C* of the available area on the plot device.  The point (0., 0.) is	*
C* the lower left corner of the device; (1., 1.) is the upper right 	*
C* corner of the device.  For example:					*
C*									*
C*     call gsview  ( .5, .5, 1., 1., iret ) 				*
C*									*
C* will display plots in the upper right quadrant of the device.  	*
C*									*
C* Note that the fractions describing the view region are not equal 	*
C* to coordinate values, except for a square device.			*
C*								 	*
C* GSVIEW  ( XLLF, YLLF, XURF, YURF, IRET )				*
C*									*
C* Input parameters:							*
C*	XLLF		REAL		Lower left x fraction		*
C*	YLLF		REAL		Lower left y fraction		*
C*	XURF		REAL		Upper right x fraction		*
C*	YURF		REAL		Upper right y fraction		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	10/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 4/90	Added GFLUSH				*
C* M. Linda/GSC		 2/97	Removed GFLUSH				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C-----------------------------------------------------------------------
C*	Check to see if this is a new view region.
C
	IF  ( ( xbndlf .eq. xllf ) .and. ( ybndbf .eq. yllf ) .and.
     +	      ( xbndrf .eq. xurf ) .and. ( ybndtf .eq. yurf ) )  THEN
	    iret = NORMAL
	    RETURN
	END IF
C
C*	Check that values are between 0 and 1.
C*	Store values in common and update boundary tables.
C
	IF  ( 0.0 .le. xllf  .and.  xllf .le. 1.0   .and.
     +	      0.0 .le. yllf  .and.  yllf .le. 1.0   .and.
     +	      0.0 .le. xurf  .and.  xurf .le. 1.0   .and.
     +	      0.0 .le. yurf  .and.  yurf .le. 1.0 )   THEN
	    xbndlf = xllf
	    ybndbf = yllf
	    xbndrf = xurf
	    ybndtf = yurf
C
C*	    Update the common areas.
C
	    CALL UPDVXY
	    iret = NORMAL
	 ELSE 
	    iret = NIVIEW
	END IF
C*
	RETURN
	END
