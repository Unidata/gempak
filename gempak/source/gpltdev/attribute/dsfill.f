	SUBROUTINE DSFILL  ( szfil, iftyp, size, jtype, iret )
C************************************************************************
C* DSFILL								*
C* 									*
C* This subroutine sets the polygon fill attributes.  If these 		*
C* attributes are not positive, no changes are made.   			*
C* 									*
C* DSFILL  ( SZFIL, IFTYP, SIZE, JTYPE, IRET )				*
C*                                                                    	*
C* Input parameters:							*
C* 	SZFIL		REAL		Fill pattern size           	*
C* 					  <=0 = no change		*
C*      IFTYP		INTEGER		Fill pattern type		*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Fill pattern size		*
C*	JTYPE		INTEGER		Fill pattern type   		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 6/97						*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER 	isend (2), ircv
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 4
	isend (2) = CSFILL
	CALL GPUT  ( isend,  2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szfil, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( iftyp, 1, iret )
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
C*	    Save the ACTIVE fill pattern size in common block 
C*	    variable.
C
	    tfilsz = size
	END IF
C*
        CALL GGET  ( ircv, 1, ier )
        IF  ( ier .ne. NORMAL ) THEN
            iret = ier
        ELSE
C
C*          Save the ACTIVE type in common block and
C*          output variable
C
            jtype  = ircv
            mfltyp = jtype 
        END IF
C*
	RETURN
	END
