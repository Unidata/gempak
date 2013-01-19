	SUBROUTINE IN_EDGE  ( string, pname, sx, sy, size, ifnt, lwid, 
     +                        ibrd, irot, ijst, iflg, iret )
C************************************************************************
C* IN_EDGE								*
C*									*
C* This subroutine decodes the specification string for the             *
C* LSTPRM parameter.  The input must be in the form:			*
C*									*
C*    PARM|x;y|size/font/width/border/rotation/justification/hw flag	*
C*									*
C* If any parameter is not specified, the active value for		*
C* parameter is used.							*
C*									*
C* IN_EDGE  ( STRING, SX, SY, SIZE, IFNT, LWID, IBRD, IROT,		*
C*            IJST, IFLG, IRET ) 			 		*
C*									*
C* Input parameters:							*
C*      STRING		CHAR*		Specification string		*
C*									*
C* Output parameters:							*
C*	PNAME		CHAR*		Parameter name			*
C*	SX		REAL		X-coord of edge plotting colums	*
C*					in normalized map coordinates.	*
C*	SY		REAL		Y-coord of edge plotting colums	*
C*					in normalized map coordinates.	*
C*      SIZE            REAL            Text size multiplier            *
C*                                       <=0 = no change                *
C*      IFNT		INTEGER         Text font number                *
C*                                        0 = no change                 *
C*      LWID            INTEGER         Text width multiplier           *
C*                                       <=0 = no change                *
C*      IBRD            INTEGER         Text border/blank fill flag     *
C*                                        <=0 = no change               *
C*      IROT            INTEGER         Text north-relative rot flag    *
C*                                        1 = screen relative           *
C*                                        2 = north relative            *
C*                                        otherwise = no change         *
C*      IJST            INTEGER         Text justification              *
C*                                         1 = left                     *
C*                                         2 = center                   *
C*                                         3 = right                    *
C*                                        otherwise = no change         *
C*      IFLG            INTEGER         Text software/hardware flag     *
C*                                        1 = software                  *
C*                                        2 = hardware                  *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* R. Jones/NCEP	05/06	Original version			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, pname
C*
	CHARACTER	carr(3)*80
	REAL		rpos(2)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Break the input string into an array of substrings
C
        CALL ST_CLST ( string, '|', ' ', 3, carr, num, ier ) 
C
C*	Parameter name.
C
	pname = carr(1)
C
C*	Plot position. Set defaults for missing values.
C
	sx = 0.95
	sy = 0.50
	IF  ( carr (2) .ne. ' ' )  THEN
	    CALL ST_RLST ( carr (2), ';', RMISSD, 2, rpos, num, ier ) 
	    IF  ( .not. ERMISS (rpos(1)) )  sx = rpos(1)
	    IF  ( .not. ERMISS (rpos(2)) )  sy = rpos(2)
        END IF
C
C*	Text attributes. Set defaults for any missing values.
C
	CALL GQTEXT ( i1, i2, r1, i3, i4, i5, i6, ier )
	CALL IN_TXTN ( carr(3), ifnt, iflg, size, lwid,
     +			ibrd, irot, ijst, ier )
     	IF  ( ifnt .le. 0 )  ifnt = 1
	IF  ( lwid .le. 0 )  lwid = 1
	IF  ( ibrd .le. 0 )  ibrd = 111
	IF  ( irot .le. 0 )  irot = 1
	IF  ( ijst .le. 0 )  ijst = 3
	IF  ( iflg .le. 0 )  iflg = 1
	IF  ( size .le. 0.0 )  size = r1
C
C*	Reset the font to Courier if HW is selected.
C
	IF  ( iflg .eq. 2 )  ifnt = 1
C*
        RETURN
	END
