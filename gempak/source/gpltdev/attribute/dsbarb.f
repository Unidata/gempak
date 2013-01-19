	SUBROUTINE DSBARB  ( szbarb, ibrwid, ibrtyp, size, jbrwid, 
     +                       jbrtyp, iret )
C************************************************************************
C* DSBARB								*
C* 									*
C* This subroutine sets the wind barb size and width multipliers and    *
C* the wind barb type.  If these parameters are not positive, no  	*
C* changes are made.   							*
C* 									*
C* DSBARB  ( SZBARB, IBRWID, IBRTYP, SIZE, JBRWID, JBRTYP, IRET )	*
C*                                                                    	*
C* Input parameters:							*
C* 	SZBARB		REAL		Barb size multiplier		*
C* 					  <=0 = no change		*
C*      IBRWID		INTEGER		Barb width multiplier		*
C*                                        <=0 = no change		*
C*      IBRTYP		INTEGER		Barb type			*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Barb size			*
C*	JBRWID		INTEGER		Barb width			*
C*	JBRTYP		INTEGER		Barb type			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/89	Documentation				*
C* S. Schotz/GSC	 1/90	Added barb width and type		*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER 	isend (3), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 5
	isend (2) = CSBARB
	CALL GPUT  ( isend,  2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szbarb, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
        isend (1) = ibrwid
        isend (2) = ibrtyp
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	CALL GGETR  ( size, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	  ELSE
C
C*	    Save the ACTIVE wind barb size common block variable.
C
	    twbsz = size
	END IF
C*
        CALL GGET  ( ircv, 2, ier )
        IF  ( ier .ne. NORMAL ) THEN
            iret = ier
          ELSE
C
C*        Save the ACTIVE wind barb width and type in common block and
C*        output parameters
C
          jbrwid = ircv(1)
          jbrtyp = ircv(2)
          mbrwid = jbrwid
          mbrtyp = jbrwid
        END IF
C*
	RETURN
	END
