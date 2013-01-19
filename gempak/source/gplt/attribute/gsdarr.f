	SUBROUTINE GSDARR  ( szdarw, szdarh, idarwd, idartp, iret )
C************************************************************************
C* GSDARR								*
C* 									*
C* This subroutine sets the directional arrow size, arrow head size, 	*
C* line width, and arrow type.  If these parameters are not positive, 	*
C* no changes are made.							*
C*									*
C* GSDARR  ( SZDARW, SZDARH, IDARWD, IDARTP, IRET )			*
C*                                                                    	*
C* Input parameters:							*
C* 	SZDARW		REAL		Arrow size multiplier		*
C* 				   	  <=0 = no change		*
C*	SZDARH		REAL		Arrow head size multiplier	*
C*					  <=0 = no change		*
C*	IDARWD		INTEGER		Arrow width multiplier		*
C*					  <=0 = no change		*
C*	IDARTP		INTEGER		Arrow type			*
C*					  <=0 = no change		*
C*					  1 = plot wind for calm wind	*
C*					  2 = don't plot for calm wind	*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	03/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVSET.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C
C*	First check if this is the current requested characteristics.
C*	If so, do nothing.
C
	IF  ( szdarw .eq. rwdasz .and. idarwd .eq. kdarwd .and. 
     +        idartp .eq. kdartp .and. szdarh .eq. rdahsz )  THEN
        ELSE
C
C*          Set requested parameters
C
	    IF ( szdarw .gt. 0. ) rwdasz = szdarw
            IF ( szdarh .gt. 0. ) rdahsz = szdarh
            IF ( idarwd .ge. 1 ) kdarwd = idarwd
	    IF ( idartp .ge. 1 ) kdartp = idartp
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*		Only set if requested different than what is already set
C
                IF ( rwdasz .ne. swdasz .or. kdarwd .ne. ldarwd .or.
     +               kdartp .ne. ldartp .or. rdahsz .ne. sdahsz ) THEN
		   CALL DSDARR  ( szdarw, szdarh, kdarwd, kdartp, 
     +                            swdasz, sdahsz, ldarwd, ldartp, iret )
                END IF
	    END IF
	END IF
C*
	RETURN
	END
