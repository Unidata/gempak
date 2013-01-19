	SUBROUTINE SFVPRM  ( sfparm, colors, prmdst, npmdst,
     +			     parms, iploc, icolr, nparm, iret )
C************************************************************************
C* SFVPRM								*
C*									*
C* This subroutine parses the user input parameter list and checks the	*
C* parameters in the output file.					*
C*									*
C* SFVPRM ( SFPARM, COLORS, PRMDST, NPMDST, PARMS, IPLOC, ICOLR, 	*
C*	    NPARM, IRET )						*
C*									*
C* Input parameters:							*
C*	SFPARM		CHAR*		User input parameter list	*
C*	COLORS		CHAR*		User input color list		*
C*	PRMDST (NPMDST)	CHAR*		Data set parameters		*
C*	NPMDST		INTEGER		Number of parameters		*
C*									*
C* Output parameters:							*
C*	PARMS  (NPARM)	CHAR*		Parameters to output		*
C*	IPLOC  (NPARM)	INTEGER		Data location in output array	*
C*	ICOLR  (NPARM)	INTEGER		Colors to output		*
C*	NPARM		INTEGER		Number of output parameters	*
C*	IRET		INTEGER		Return code			*
C*					  -5 = STID and STNM missing	*
C*					   0 = normal			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 2/99						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sfparm, colors, prmdst (*), parms (*)
	INTEGER		iploc (*), icolr (*)
C*
	CHARACTER	pname (MMPARM)*4, tp*4
	INTEGER		icl (MMPARM)
	LOGICAL		fndid, fndnm
C------------------------------------------------------------------------
	iret  = 0
C
C*	Get parameter names and colors from the user input.
C
	CALL IN_PARM  ( MMPARM, sfparm, pname, npnam, ier )
	CALL IN_COLR  ( colors, MMPARM, icl, ier )
C
C*	Check that there are some parameters.
C
	IF  ( npnam .ne. 0 )  THEN
C
C*	    Set the extra colors to 0.
C
	    DO  j = npnam+1, MMPARM
		icl (j) = 0
	    END DO
C
C*	    Put STID and STNM at the end of the parameter and color
C*	    lists. Return with an error if both are missing from 
C*	    the parameter list.
C
	    fndid = .false.
	    fndnm = .false.
C
	    DO  j = 1, npnam
		IF  ( .not. fndid .and. pname (j) .eq. 'STID' )  THEN
		    fndid = .true.
		    tp = pname (npnam)
		    pname (npnam) = pname (j)
		    pname (j) = tp
		    jc = icl (npnam)
		    icl (npnam) = icl (j)
		    icl (j) = jc
		END IF
	    END DO
C
	    k = npnam
	    IF  ( fndid )  k = npnam - 1
	    DO  j = 1, k
		IF  ( .not. fndnm .and. pname (j) .eq. 'STNM' )  THEN
		    fndnm = .true.
		    tp = pname (k)
		    pname (k) = pname (j)
		    pname (j) = tp
		    jc = icl (k)
		    icl (k) = icl (j)
		    icl (j) = jc
		END IF
	    END DO
C
	    IF  ( .not. fndid .and. .not. fndnm )  THEN
		iret = -5
		RETURN
	    END IF
C
C*	    Add these parameters to the current list.
C
	    nparm = 0
	    DO  i = 1, npnam
C
C*		Check that parameters are in the GEMPAK file data set.
C
		CALL ST_FIND ( pname (i), prmdst, npmdst, ipos, ier )
C
		IF  ( ipos .eq. 0 )  THEN
		    IF  ( ( pname(i) .eq. 'STID' ) .or.
     +			  ( pname(i) .eq. 'STNM' ) )  THEN
C
C*			If the parameter is a station identifier,
C*			add it to the list of parms.
C
			nparm = nparm + 1
			parms ( nparm ) = pname (i)
			icolr ( nparm ) = icl (i)
			IF  ( pname(i) .eq. 'STID' ) iploc(nparm) = -1
			IF  ( pname(i) .eq. 'STNM' ) iploc(nparm) = -2
		      ELSE
			IF  ( ( pname(i) .ne. 'BLNK' ) .and.
     +			      ( pname(i) .ne. 'SPAC' ) )  THEN
C
C*			    Otherwise...
C*			    If the parameter cannot be found in the data
C*			    set, and it is not BLNK nor SPAC, write a 
C*			    warning message and do not add it to the
C*			    list of parameters.
C
			    CALL ER_WMSG ( 'SFVGSF', +3, pname (i), ier )
			END IF
		    END IF
		  ELSE
C
C*		    Otherwise, add the parameter to the list and set
C*		    the array location and the offset values.
C
		    nparm = nparm + 1
		    parms ( nparm ) = pname (i)
		    icolr ( nparm ) = icl (i)
		    iploc ( nparm ) = ipos
		END IF
	    END DO
	END IF
C*
	RETURN
	END
