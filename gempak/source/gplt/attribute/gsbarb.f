	SUBROUTINE GSBARB  ( szbarb, ibrwid, ibrtyp, iret )
C************************************************************************
C* GSBARB								*
C* 									*
C* This subroutine sets the wind barb size, line width, and barb type.	*
C* If these parameters are not positive, no changes are made.		*
C*									*
C* NOTE THAT THE CALLING SEQUENCE FOR THIS SUBOUTINE HAS CHANGED FROM	*
C* PREVIOUS VERSIONS OF GEMPAK.						*
C* 									*
C* GSBARB  ( SZBARB, IBRWID, IBRTYP, IRET )				*
C*                                                                    	*
C* Input parameters:							*
C*	SZBARB		REAL		Barb size multiplier		*
C* 					  <=0 = no change		*
C*	IBRWID		INTEGER         Barb width multipier		*
C* 					  <=0 = no change		*
C*	IBRTYP		INTEGER		Barb type 			*
C*					  <=0 = no change		*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* S. Schotz/GSC	 1/90   Added barb width and type parameters 	*
C* S. Schotz/GSC	 3/90   Fixed size to allow between 0 and 1	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVSET.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C
C*	First check if thes are the current requested characteristics.
C*	If so, do nothing.
C
	IF  ( szbarb .eq. rwbsz .and. ibrwid .eq. kbrwid .and.
     +        ibrtyp .eq. kbrtyp ) THEN
	ELSE 
C
C*	    Set requested parameters
C
            IF  ( szbarb .gt. 0 ) rwbsz = szbarb
            IF  ( ibrwid .ge. 1 ) kbrwid = ibrwid
            IF  ( ibrtyp .ge. 1 ) kbrtyp = ibrtyp
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*              Only set if requested different than what is already set
C
                IF ( rwbsz .ne. swbsz .or. kbrwid .ne. lbrwid .or.
     +               kbrtyp .ne. lbrtyp) THEN
		    CALL DSBARB  ( rwbsz, kbrwid, kbrtyp, swbsz, 
     +                             lbrwid, lbrtyp, iret )
                END IF
	    END IF
	END IF
C*
	RETURN
	END
