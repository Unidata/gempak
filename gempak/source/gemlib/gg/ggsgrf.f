	SUBROUTINE GG_SGRF  ( proj, garea, iret )
C************************************************************************
C* GG_SGRF								*
C*									*
C* This subroutine defines an output graph coordinate system.  PROJ	*
C* must be:								*
C*             POL    polar coordinates					*
C*             LIN    linear x and y					*
C*             LOG    linear x, logarithmic y				*
C*             KAP    linear x, y ** KAPPA  ( KAPPA = 2 / 7 )		*
C*									*
C* The graphics area GAREA is specified by five numbers corresponding	*
C* to the lower left x, lower left y, upper right x, upper right y and 	*
C* the height-to-width ratio of the plotting area.  If the plot ratio 	*
C* is unspecified or 0, the entire area inside the margins will be used.*
C*									*
C* GG_SGRF  ( PROJ, GAREA, IRET )					*
C*									*
C* Input parameters:							*
C*	PROJ		CHAR*	 	Projection type			*
C*	GAREA		CHAR*	 	Graphics area			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*					  0 = normal return		*
C*					 -4 = error specifying graph 	*
C*					 -5 = invalid projection	*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/86						*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C************************************************************************
	CHARACTER*(*) 	proj, garea
C*
	REAL 		rarr (5)
C------------------------------------------------------------------------
C*	Check for valid projection and set types.
C
	iret   = 0
	ixtype = 1
	IF  ( proj .eq. 'LIN' )  THEN
	    iytype = 1
	  ELSE IF  ( proj .eq. 'LOG' )  THEN
	    iytype = 2
	  ELSE IF  ( proj .eq. 'KAP' )  THEN
	    iytype = 3
	  ELSE IF  ( proj .eq. 'POL' )  THEN
	    ixtype = 5
	    iytype = 5
	  ELSE
	    iret = -5
	    CALL ER_WMSG  ( 'GG', iret, proj, ier )
	    RETURN
	END IF
C
C*	Valid projection found, decode GAREA and set graph mode.
C
	CALL ST_RLST  ( garea, ';', 0.0, 5, rarr, num, ier )
	CALL GSGRAF   ( ixtype, iytype, rarr (5), rarr (1), rarr (2), 
     +			rarr (3), rarr (4), iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
	    iret = -4
	END IF
C*
	RETURN
	END
