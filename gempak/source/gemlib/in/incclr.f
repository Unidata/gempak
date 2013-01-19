        SUBROUTINE IN_CCLR ( cclr, nexpc, value, iclrs, numc, clrprm,
     +                       endflg, iret )
C************************************************************************
C* IN_CCLR                                                              *
C*                                                                      *
C* This subroutine decodes the input for color coding.			*
C* The input must be in the form:                                       *
C*               (v1;v2;...vN-1/c1;c2;...cN/PARM)			*
C*	   or    (v1-vN-1-vInc/c1-cN/PARM)				*
C*	   or     a combination of a list and a range.			*
C*									*
C* Parentheses MUST enclose the color coding information.  For this     *
C* routine to  work properly, the number of values should be one less   *
C* than the number of colors.                                           *
C*                                                                      *
C* IN_CCLR  ( CCLR, NEXPC, VALUE, ICLRS, NUMC, CLRPRM, ENDFLG, IRET )	*
C*                                                                      *
C* Input parameters:                                                    *
C*      CCLR            CHAR*           Color coding input              *
C*	NEXPC 		INTEGER		Number of expected colors	*
C*                                                                      *
C* Output parameters:                                                   *
C*	VALUE (NUMC-1)	REAL		Values of the intervals		*
C*	ICLRS (NUMC)	INTEGER		Color numbers			*
C*	NUMC  		INTEGER		Number of colors	 	*
C*      CLRPRM          CHAR*           Color parameter                 *
C*	ENDFLG		CHAR*		Value range end point flag	*
C*					  L = Lower data range		*
C*					  U = Upper data range		*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                      -13 = invalid number of  	*
C*					      intervals or colors	*
C*                                      -14 = value range w/out         *
C*					      increment is invalid	*
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* S. Maxwell/GSC	 4/97						*
C* D. Kidwell/NCEP	 2/98	Removed NUMV from arg list, cleaned up  *
C* S. Jacobs/NCEP	 3/99	Added endflg parsing and return		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER*(*)   cclr, clrprm, endflg
	REAL		value(*)
	INTEGER		iclrs(*)
C*
	CHARACTER	carr (4)*72, first*4, last*4, inc*4, tstr*72
C------------------------------------------------------------------------
        iret = 0
	numc = 0
	numv = 0
C
C*	Delete leading spaces and parentheses.
C
	CALL ST_LDSP ( cclr, cclr, lens, ier )
	tstr = cclr ( 2 : lens-1)
C
C*      Parse color coding information.
C
        CALL ST_CLST ( tstr, '/',' ', 4, carr, num, ier )
C
C*	Get the end point flag. The flag indicates which end of the
C*	value range to include in the range. L is the lower value and
C*	U is the upper value.
C
	CALL ST_LCUC ( carr (4), endflg, ier )
	IF  ( endflg .eq. ' ' )  endflg = 'L'
C
C*	Get the parameter name for color coding.
C
	CALL ST_LCUC ( carr (3), clrprm, ier )
C
C*	Check for color range.
C
        CALL ST_RANG  ( carr (2), first, last, inc, ityp, ier )
C
	IF ( ityp .eq. 0 ) THEN
C
C*	    Convert strings to arrays.
C
            CALL ST_ILST ( carr (2), ';', IMISSD, nexpc, 
     +                     iclrs, numc, ier )
C
	  ELSE
            CALL ST_NUMB  ( first, ifirst, ier )
            CALL ST_NUMB  ( last,  ilast,  ier )
            CALL ST_NUMB  ( inc,   incr,   ier )
            IF  ( ityp .eq. 1 )  THEN
                incr  = 1
              ELSE
                incr  = ABS  ( incr )
            END IF
            IF  ( ifirst .gt. ilast )  incr = - incr
            DO  icol = ifirst, ilast, incr
                numc = numc + 1
                IF  ( numc .le. nexpc )  THEN
                    iclrs (numc) = icol
                END IF
            END DO
	END IF
C
C*	Check for value range.
C
	IF ( INDEX ( carr (1), ';' ) .eq. 0 ) THEN
            CALL ST_RANG  ( carr (1), first, last, inc, ityp, ier )
	  ELSE
	    ityp = 0
	END IF
C
	IF  ( ityp .eq. 0 )  THEN
C
C*	    Convert strings to arrays.
C
            CALL ST_RLST ( carr (1), ';', RMISSD, nexpc-1, value, 
     +                     numv, ier )
C
	  ELSE IF ( ityp .eq. 1 ) THEN
	    iret = -14
	    RETURN
C
	  ELSE IF ( ityp .eq. 2 ) THEN
	    CALL ST_CRND  ( first, rfirst, ndec1, ier )
	    CALL ST_CRND  ( last,  rlast,  ndec2, ier )
	    CALL ST_CRND  ( inc,   rincr,  ndec3, ier )
C
C*	    Get max value of ndec.
C
	    n = MAX ( ndec1, ndec2 ) 
	    n = MAX ( n, ndec3 ) 
	    istrt = rfirst * 10 ** n
	    iend  = rlast  * 10 ** n
	    istep = rincr  * 10 ** n
C
            incr  = ABS  ( rincr )
C
            IF  ( istrt .gt. iend )  istep = - rincr
            DO  ival = istrt, iend, istep 
                numv = numv + 1
                IF  ( numv .lt. nexpc)  THEN
                    value (numv) = FLOAT ( ival )/10 ** n
                END IF
            END DO
	END IF
C
C
C*	Check for error in the number of colors.
C
	IF ( numc .ne. numv+1 ) THEN
	    iret = -13
	    RETURN
	END IF
C*
        RETURN
        END
