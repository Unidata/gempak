	SUBROUTINE SFMCLR  ( ncol, prmlst, colors, icolor, ccvals,
     +			     icclrs, numccc, icrprm, inew, prmnew, 
     +			     endflg, iret )
C************************************************************************
C* SFMCLR								*
C*									*
C* This subroutine translates the colors.  Color coding of a parameter  *
C* based on any other parameter is allowed.  (A parameter on which      *
C* color coding is based is known as a reference parameter.)            *
C*									*
C* SFMCLR  ( NCOL, PRMLST, COLORS, ICOLOR, CCVALS, ICCLRS, NUMCCC, 	*
C*           ICRPRM, INEW, PRMNEW, ENDFLG, IRET )			*
C*									*
C* Input parameters:							*
C*	NCOL		INTEGER		Number of colors		*
C*      PRMLST(*)       CHAR*           Parameter names			*
C*	COLORS		CHAR*		COLORS input			*
C*									*
C* Output parameters:							*
C*	ICOLOR (*)	INTEGER		Colors array			*
C*	CCVALS (*)	REAL		Values for color coding         *
C*	ICCLRS (*)      INTEGER		Colors for color coding         *
C*	NUMCCC (*)	INTEGER		Number of colors for coding     *
C*	ICRPRM (*)   	INTEGER		Pointers to reference parms     *
C*	INEW		INTEGER		Total number of parameters      *
C*	PRMNEW (*)	CHAR*		Parameter names, incl. reference*
C*	ENDFLG		CHAR*		Value range end point flag	*
C*					  L = Lower data range		*
C*					  U = Upper data range		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	Reorganized				*
C* S. Schotz/GSC	 4/90   Removed parameter type added parameter	*
C* 				name instead				*
C* T. Lee/GSC            1/98   Increased ICBUFF dimension from 11->12  *
C* D. Kidwell/NCEP       2/98   Added processing of color coding info   *
C* A. Hardy/GSC          2/99   Increased icbuff 12 -> 26 for model plot*
C* S. Jacobs/NCEP	 3/99	Added endflg; Changed call to IN_CCLR	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		ccvals (*)
	CHARACTER*(*)	colors, prmlst (*), prmnew (*), endflg
	INTEGER		icolor (*), numccc (*), icclrs (*), icrprm (*)
C*
	INTEGER		icbuff (26), iclrs (LLCLEV)
	CHARACTER	ccolor (MMPARM)*72, clrprm*4
	REAL		value (LLCLEV-1)
C------------------------------------------------------------------------
	iret = 0
C
	CALL IN_CCOL  ( colors, ncol, icbuff, ccolor, ier )
	IF ( ier .ne. 0 ) CALL ER_WMSG ( 'IN', ier, ' ', ierr )
C
	ic  = 0
	ilast = 0
	jc = 0
	inew = ncol
	DO i = 1, ncol
	    prmnew (i) = prmlst (i)
	END DO
C
C*	Turn off colors for 'BLNK' and 'SPAC'.
C
	DO  i = 1, ncol
            IF  ( (prmlst (i) .eq. 'BLNK') .or. 
     +            (prmlst (i) .eq. 'SPAC') ) THEN
                icolor (i) = 0
              ELSE
		ic = ic + 1
		icolor (i) = icbuff (ic)
		IF ( icolor (i) .lt. 0 ) THEN
C
C*		    Parse color coding information.
C
		    indx = IABS ( icolor (i) ) 
		    IF ( indx .gt. ilast ) THEN
C
C*			This is a new color coding sequence.
C
		        CALL IN_CCLR ( ccolor (indx), LLCLEV, value,
     +				       iclrs, numc, clrprm, endflg,
     +				       ier )
C
		        IF ( ier .eq. 0 ) THEN
			    IF ( clrprm .eq. ' ' ) clrprm = prmlst (i)
			    CALL ST_FIND ( clrprm, prmnew, inew, ipos,
     +					   iret )
			    ilast = indx
			    numccc (indx) = numc
			    DO j = 1, numc
			        ccvals (jc + j) = value (j)
			        icclrs (jc + j) = iclrs (j)
			    END DO
			    jc = jc + numc
			    ccvals (jc) = RMISSD 
			    IF ( ipos .gt. 0 ) THEN
			        icrprm (indx) = ipos
			      ELSE
C
C*				Save new reference parm to add to list.
C
				inew = inew + 1
				prmnew (inew) = clrprm
				icrprm (indx) = inew
			    END IF
		          ELSE
  			    CALL ER_WMSG ( 'IN', ier, ' ', ierr ) 
			    icolor (i) = 1
			END IF
C
		    END IF
		END IF
	    END IF
	END DO
C*
	RETURN
	END	
