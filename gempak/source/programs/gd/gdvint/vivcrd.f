	SUBROUTINE VI_VCRD  ( iret )
C************************************************************************
C* VI_VCRD								*
C*									*
C* This subroutine sets the vertical coordinate names in the common	*
C* block.								*
C*									*
C* VI_VCRD ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C*					-13 = invalid GVCORD		*
C*					-10 = ptop missing		*
C**									*
C* Log:									*
C* K. Brill/NMC      	05/92						*
C* K. Brill/NMC		08/92	Check for missing ptop			*
C* K. Brill/NMC		10/92	Interpret VCOORD input			*
C************************************************************************
	INCLUDE		'vicmn.cmn'
	CHARACTER	carr(2)*32, nnpt(2)*8
	CHARACTER*8	vparm
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Split the user input into two pieces.
C
	CALL ST_CLST ( gvcord, '/', ' ', 2, carr, ns, ier )
	IF ( ns .ne. 2 .or. ier .ne. 0 ) THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Split name and top pressure, if it exists.
C
	CALL ST_CLST ( carr (1), ':', ' ', 2, nnpt, ns, ier )
	vcordi = nnpt (1)
	CALL ST_CRNM ( nnpt (2), ptopi, ier )
C*
	CALL ST_CLST ( carr (2), ':', ' ', 2, nnpt, ns, ier )
	vcordo = nnpt (1)
	CALL ST_CRNM ( nnpt (2), ptopo, ier )
	CALL ST_LCUC ( vcordi, vcordi, ier )
	CALL ST_LCUC ( vcordo, vcordo, ier )
	IF ( ( vcordi .eq. NETA .or. vcordi .eq. NSGMA ) .and.
     +	       ERMISS (ptopi) ) iret = -10
	IF ( ( vcordo .eq. NETA .or. vcordo .eq. NSGMA ) .and.
     +	       ERMISS (ptopo) ) iret = -10
C
C*	Split the user input in VCOORD into two pieces.
C
	CALL ST_CLST ( vcoord, '/', ' ', 2, carr, ns, ier )
C
C*	The first piece is the list of vertical coordinates of
C*	surface data to be transfered into the output file.
C
	CALL ST_CLST ( carr(1), ';', ' ', MXSFVC, sfvclt, nsfvc,
     +		       ier )
	IF ( ier .ne. 0 ) nsfvc = 0
C
C*	Turn these names into numbers.
C
	DO i = 1, nsfvc
	    CALL LV_CORD ( sfvclt (i), vparm, isfvc (i), ier )
	    IF( ier .ne. 0 ) THEN
		isfvc (i) = 0
	    ELSE
	    	sfvclt (i) = vparm
	    END IF
	END DO
C
C*	The second piece is the flag for the Lorenz boundary.
C
	IF ( ( carr (2) (1:1) .eq. 'L' .or. carr (2) (1:1) .eq. 'l' )
     +	       .and. vcordo .eq. NTHTA )
     +		lorenz = .true.
C*
	RETURN
	END
