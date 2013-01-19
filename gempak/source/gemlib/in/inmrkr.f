        SUBROUTINE IN_MRKR ( mrkr, condtn, iret )
C************************************************************************
C* IN_MRKR                                                              *
C*                                                                      *
C* This subroutine decodes the input for the markers.			*
C* The input must be in the form:                                       *
C*                 type : size : width : hwflag                         *
C* If the user has entered a condition, it must precede the first :     *
C*                                                                      *
C* IN_MRKR  ( MRKR, CONDTN, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C*      MRKR            CHAR*           Marker input            	*
C*                                                                      *
C* Output parameters:                                                   *
C*      CONDTN          CHAR*           Condition                       *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* S. Maxwell/GSC         3/97   	                                *
C* S. Maxwell/GSC         3/97   	Updated documentation           *
C************************************************************************
        CHARACTER*(*)   mrkr, condtn
C*
        REAL            rarr (4)
        CHARACTER       marker*24
C------------------------------------------------------------------------
        iret = 0
C
C*      Check for a colon.
C
        ipos = INDEX ( mrkr, ':' )
        IF  ( ipos .eq. 0 )  THEN
            condtn = mrkr
            RETURN
          ELSE
            condtn = mrkr ( 1:ipos-1 )
            marker = mrkr ( ipos+1: )
        END IF
C
C*      Decode size and width.
C
        CALL ST_RLST ( marker, ':', 1. , 4, rarr, num, ier )
	imkhw  = ( rarr (4) )
        size   = rarr (2)
        iwidth = NINT ( rarr (3) )
	imark  = ( rarr (1) )
        CALL GSMRKR  ( imark, imkhw, size, iwidth, ier )
C*
        RETURN
        END
