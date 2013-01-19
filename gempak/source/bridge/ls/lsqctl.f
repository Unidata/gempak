        SUBROUTINE  LS_QCTL  ( iret )
C************************************************************************
C* LS_QCTL                                                              *
C*                                                                      *
C* This subroutine checks the quality of the surface pressure reports	*
C* before writes to the GEMPAK output file.				* 
C*                                                                      *
C* LS_QCTL  ( IRET )                               			*
C*                                                                      *
C* Output parameters:                                                   *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                                                      *
C**								        *
C* Log:							                *
C* T. Lee/GSC		2/99	Creation				*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'lscmn.cmn'
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
        iret = 0
C
C*	Quality control.  The station pressure needs to fall into the
C*	pressure range set in the packing table. Allow 20 mb difference
C*	between the mean sea level pressure and the altimeter setting.
C
	pres = rivals ( irpres )
	selv = rivals ( irselv )
	pmsl = rivals ( irpmsl )
C
	IF  ( .not. ERMISS ( pres ) .and. .not. ERMISS ( pmsl ) 
     +	      .and. ( pres .ge. 700. .and. pres .le. 1100. ) )  THEN
		alti = PR_ALTP ( pres, selv )
		altm = PR_ALTM ( alti )
		IF  ( ABS ( pmsl - altm ) .gt. 20. )  THEN
		    rivals ( irpmsl ) = RMISSD
		    rivals ( irpres ) = RMISSD
		END IF
        END IF
C*
        RETURN
        END
