      SUBROUTINE GCLEAR  ( iret )
C************************************************************************
C* GCLEAR								*
C* 									*
C* This subroutine clears the current device.  On a direct-access 	*
C* device, GCLEAR erases the screen.  On a continuous-paper plotter, 	*
C* GCLEAR will advance to the next page.  On a single-page plotter, 	*
C* GCLEAR will unload the paper so another sheet can be loaded. 	*
C* 									*
C* GCLEAR  ( IRET )							*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	 6/84	GEMPLT Version 3.0			*
C* M. Goodman/RDS	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* M. desJardins/NMC	 1/92	Allow window size change		*
C* M. Linda/GSC		 2/97	Removed GFLUSH				*
C* C. Lin/EAI		 6/97	Add 'S' coordinates			*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'DEVCHR.CMN'
	INCLUDE		'XYDEF.CMN'
C*
C------------------------------------------------------------------------
	iret = NORMAL
C
	IF  ( ddev .ne. ' ' )  THEN
C
C*	    Clear the device.
C
	    CALL DCLEAR  ( iret )
C
C*	    If the return code from DCLEAR indicates that the size has
C*	    changed, set the new size.
C
	    IF  ( iret .eq. NWSIZE )  THEN
		iret = NORMAL
		CALL DQDCHR  ( nncolr, ier )
C
C*		Move the bounds into the XYDEF common area.
C*		Then calculate all the coordinate information.
C
C*		screen (S)  coordinates
C
		IF ( ileft .le. iright ) THEN
		    ibndls = ileft + isxoff 
		    ibndrs = ileft + isxoff + iswdth
		  ELSE
		    ibndls = iright + isxoff + iswdth
		    ibndrs = iright + isxoff 
		END IF
C
		IF ( itop .le. ibot ) THEN
		    ibndts = itop + isyoff
		    ibndbs = itop + isyoff + ishght
		  ELSE
		    ibndts = ibot + isyoff + ishght
		    ibndbs = ibot + isyoff
		END IF
C
C*		device coordinates
C
		ixbndl = ileft
		iybndb = ibot
		ixbndr = iright
		iybndt = itop
C
C*		Update transformation information.
C
		CALL UPDSXY
	    END IF
	END IF
C*
	RETURN
	END
