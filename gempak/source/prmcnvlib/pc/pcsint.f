	SUBROUTINE PC_SINT  ( noutpm, outprm, cmpflg, idx1, idx2, iret )
C************************************************************************
C* PC_SINT								*
C*									*
C* This subroutine sets up the tables needed by the PC package to	*
C* perform interpolations.						*
C*									*
C* PC_SINT  ( NOUTPM, OUTPRM, CMPFLG, IDX1, IDX2, IRET )		*
C*									*
C* Input parameters:							*
C*	NOUTPM		INTEGER		Number of parameters		*
C*	OUTPRM (NOUTPM) CHAR*4		Output parameters		*
C*	CMPFLG (NOUTPM) LOGICAL		Computable flags		*
C*	IDX1		INTEGER		Index for intermediate table	*
C*	IDX2		INTEGER 	Index for output table		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-19 = no interp;PRES not comp	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	Cleaned up				*
C* M. desJardins/GSFC	 3/91	Added interpolation using hght		*
C* M. desJardins/NMC	 9/92	Save HGHT in interm. parms if needed for*
C*				interpolation				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	CHARACTER*(*)	outprm (*)
	LOGICAL		cmpflg (*)
C*
	CHARACTER*4	interm ( MAXPRM )
	LOGICAL		cmp ( MAXPRM ), redo, list ( MAXPRM ), needht
C------------------------------------------------------------------------
C*	Set the intermediate parameters to the basics.  Be sure
C*	that the parameter to be interpolated (PRMINT) is in position 1.
C*	Note that PRMINT is saved in the common area.
C
	iret       = 0
	interm (1) = prmint
	i          = 1
	nintr      = 1
	doint      = .true.
	DO WHILE  ( ( basics(i) .ne. ' ' ) .and. ( i .le. MAXPRM ) )
	    IF  ( basics (i) .ne. prmint )  THEN
		nintr = nintr + 1
		interm ( nintr ) = basics (i)
	    END IF
	    i = i + 1
	END DO
C
C*	Using index = idx1, set up the computation of the intermediate
C*	parameters.  Eliminate those which cannot be computed from
C*	the list.
C
	index = idx1
	CALL PC_METH  ( index, jdsprm, dsparm, nintr, interm, cmp, n, 
     +			ier )
	needht = .false.
	IF  ( .not. cmp (1) )  THEN
	    IF  ( .not. cmp (6) )  THEN
		iret  = -19
		doint = .false.
		RETURN
	      ELSE
		needht = .true.
	    END IF
	END IF
C
C*	Eliminate parameters which cannot be calculated.
C
	redo = .false.
	k    = 0
	DO  i = 1, nintr
	    IF  ( cmp (i) )  THEN
		k = k + 1
		interm (k) = interm (i)
	      ELSE
		redo = .true.
	    END IF
	END DO
	nintr = k
C
C*	Use index = idx2 to compute output parameters from 
C*	intermediates.
C
	index = idx2
	CALL PC_METH  ( index, nintr, interm, noutpm, outprm, cmp, n, 
     +			ier )
C
C*	Eliminate intermediate parameters which are not used to compute
C*	output values.
C
	DO  i = 1, kinpm (idx2)
	    list (i) = .false.
	ENDDO
	list (1) = .true.
C
	DO  i = 1, noutpm
	    k = kans ( i, idx2 )
	    IF  ( ( k .gt. 0 ) .and. ( k .le. kinpm (idx2) ) )
     +				list ( k ) = .true.
	END DO
C
	DO  i = 1, kfunc (idx2)
	    DO  j = 1, 4
		k = kposno ( j, i, idx2 )
		IF  ( ( k .gt. 0 ) .and. (k .le. kinpm (idx2) ) )
     +				list ( k ) = .true.
	    END DO
	END DO
C
	k = 0
	DO i = 1, nintr
	    IF  ( list (i) .or.
     +		  ( needht .and. ( interm (i) .eq. 'HGHT' ) ) ) THEN
		k = k + 1
		interm (k) = interm (i)
	      ELSE
		redo = .true.
	    END IF
	END DO
	nintr = k
C
C*	If there are output parameters which can be computed from dataset
C*	parameters but not from the intermediate parameters, add them
C*	to the list of intermediate parameters provided BSONLY is not set.
C
	IF  ( .not. bsonly ) THEN
	    DO  i = 1, noutpm
		IF  ( (.not. cmp(i)) .and. (cmpflg(i)) ) THEN
		    nintr = nintr + 1
		    interm ( nintr ) = outprm (i)
		    redo = .true.
		END IF
	    END DO
	END IF
C
C*	Recompute tables if necessary
C
	IF  ( redo ) THEN
	    index = idx1
	    CALL PC_METH ( index, jdsprm, dsparm, nintr, interm, cmp, 
     +						n, ier )
	    index = idx2
	    CALL PC_METH ( index, nintr, interm, noutpm, outprm, cmp, 
     +						n, ier )
	END IF
C
C*	Check for hght in intermediate table and save in common.
C
	jhght = 0
	DO  i = 1, nintr
	    IF  ( interm (i) .eq. 'HGHT' )  jhght = i
	END DO
	IF  ( idx1 .eq. 2 )  THEN
	    jhght2 = jhght
	  ELSE IF  ( idx1 .eq. 6 )  THEN
	    jhght6 = jhght
	END IF
C*
	RETURN
	END
