	SUBROUTINE IN_CTYP  ( ctype, nflag, lflag, sflag, bflag, 
     +			      fflag, iret )
C************************************************************************
C* IN_CTYP								*
C*									*
C* This subroutine decodes the input for the type of contours to be 	*
C* drawn.								*
C*									*
C* IN_CTYP  ( CTYPE, NFLAG, LFLAG, SFLAG, BFLAG, FFLAG, IRET )		*
C*									*
C* Input parameters:							*
C*	CTYPE		CHAR*		Contour attribute input		*
C*									*
C* Output parameters:							*
C*      NFLAG           LOGICAL         GEMAPK contour without subbox   *
C*	LFLAG		LOGICAL		Lagrangian contour flag		*
C*	SFLAG		LOGICAL		Spline contour flag		*
C*	BFLAG		LOGICAL		Box contour flag		*
C*	FFLAG		LOGICAL		Fill contour flag		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/NMC	12/91						*
C* T. Piper/GSC		11/98	Updated prolog				*
C* M. Li/GSC		 1/00	Added nflag				*
C* S. Gilbert/NCEP	 4/07	set nflag to always return false. 'C'   *
C*                              'L' now specify same contour algorithm  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	ctype
	LOGICAL		lflag, sflag, bflag, fflag, nflag
C*
	CHARACTER	carr (4)*8, cc*1, cinput*24
C------------------------------------------------------------------------
	iret = 0
C
C*	Break input into elements.
C
	CALL ST_LCUC  ( ctype, cinput, ier )
	CALL ST_CLST  ( cinput, '/', ' ', 4, carr, n, ier )
C
C*	Set contour types.
C
	lflag = .false.
	sflag = .false.
	bflag = .false.
	fflag = .false.
	nflag = .false.
C
C*	If input was blank, set Lagrangian type.
C
	IF  ( n .eq. 0 )  lflag = .true.
C
C*	Check each element for contour type.
C
	DO  i = 1, n
	    cc = carr (i) (1:1)
	    IF  ( cc .eq.'C' )  THEN
		lflag = .true.
              ELSE IF  ( cc .eq. 'L' )  THEN
                lflag = .true.
	      ELSE IF  ( cc .eq. 'S' )  THEN
		sflag = .true.
	      ELSE IF  ( cc .eq. 'B' )  THEN
		bflag = .true.
	      ELSE IF  ( cc .eq. 'F' )  THEN
		fflag = .true.
	    END IF
	END DO
C*
	RETURN
	END
