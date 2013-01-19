	SUBROUTINE GSARRW  ( szarrw, szarrh, iarwid, iartyp, iret )
C************************************************************************
C* GSARRW								*
C* 									*
C* This subroutine sets the wind arrow size, arrow head size, 		*
C* line width, and arrow type.  If these parameters are not positive, 	*
C* no changes are made.							*
C*									*
C* NOTE THAT THE CALLING SEQUENCE OF THIS SUBROUTINE HAS CHANGED FROM	*
C* PREVIOUS VERSIONS OF GEMPAK.						*
C* 									*
C* GSARRW  ( SZARRW, SZARRH, IARWID, IARTYP, IRET )			*
C*                                                                    	*
C* Input parameters:							*
C* 	SZARRW		REAL		Arrow size multiplier		*
C* 				   	  <=0 = no change		*
C*	SZARRH		REAL		Arrow head size multiplier	*
C*					  <=0 = no change		*
C*	IARWID		INTEGER		Arrow width multiplier		*
C*					  <=0 = no change		*
C*	IARTYP		INTEGER		Arrow type			*
C*					  <=0 = no change		*
C*					  1 = plot wind for calm wind	*
C*					  2 = don't plot for calm wind	*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* S. Schotz/GSC	 1/90	Added arrow width and type		*
C* S. Schotz/GSC	 3/90	Fixed arrow size to allow < 1		*
C* S. Schotz/GSC	 8/90	Added arrow head size			*
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
	IF  ( szarrw .eq. rwasz .and. iarwid .eq. karwid .and. 
     +        iartyp .eq. kartyp .and. szarrh .eq. rwahsz )  THEN
        ELSE
C
C*          Set requested parameters
C
	    IF ( szarrw .gt. 0. ) rwasz = szarrw
            IF ( szarrh .gt. 0. ) rwahsz = szarrh
            IF ( iarwid .ge. 1 ) karwid = iarwid
	    IF ( iartyp .ge. 1 ) kartyp = iartyp
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*		Only set if requested different than what is already set
C
                IF ( rwasz .ne. swasz .or. karwid .ne. larwid .or.
     +               kartyp .ne. lartyp .or. rwahsz .ne. swahsz ) THEN
		   CALL DSARRW  ( szarrw, szarrh, karwid, kartyp, 
     +                            swasz, swahsz, larwid, lartyp, iret )
                END IF
	    END IF
	END IF
C*
	RETURN
	END
