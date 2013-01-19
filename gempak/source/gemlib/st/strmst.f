	SUBROUTINE ST_RMST  ( string, substr, ipos, outstr, iret )
C************************************************************************
C* ST_RMST								*
C*									*
C* This subroutine finds a substring within a string and returns 	*
C* the position of that substring and the output string with the 	*
C* substring removed.  If the substring is not found, the position, 	*
C* IPOS, is set to zero.						*
C*									*
C* ST_RMST  ( STRING, SUBSTR, IPOS, OUTSTR, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C* 	SUBSTR		CHAR*		Substring 			*
C*									*
C* Output parameters:							*
C*	IPOS		INTEGER		Position of substring		*
C*	OUTSTR		CHAR*		Output string less substring	*
C* 	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*									*
C**									*
C* Log:									*
C* S. Schotz/GSC	 1/90						*
C* M. desJardins/NMC	 8/94	Clean up				*
C* L. Sager/NCEP         2/96   Increased size of strtmp                *
C* S. Jacobs/NCEP	10/97	Changed setting of outstr; use temp vars*
C************************************************************************
	CHARACTER*(*)	string, substr, outstr
C*
	CHARACTER	sss*160, ttt*160
C------------------------------------------------------------------------
	iret   = 0
	sss = string
	ttt = ' '
C
C*	Get lengths of strings.
C
        CALL ST_LSTR  ( substr, lensub, ier )
	IF  ( lensub .eq. 0 )  lensub = 1
	CALL ST_LSTR  ( sss, lenstr, ier )
C
C*	Search for substring in string.
C
	ipos = INDEX ( sss, substr ( : lensub ) )
	IF  ( ipos .ne. 0 )  THEN
	    ipos2 = ipos + lensub
C
C*	    First, remove substring at beginning of string.
C
            IF  ( ipos .eq. 1 )  THEN
		IF  ( ipos2 .gt. lenstr )  THEN
		    ttt = ' '
		  ELSE
		    ttt = sss (ipos2 : )
		END IF
C
C*              Remove substring at end of string.
C
	      ELSE IF  ( ipos2 .gt. lenstr )  THEN
                ttt = sss ( : ipos-1 )
C
C*              The substring is within the string.
C
	      ELSE 
                ttt = sss ( : ipos-1 ) // 
     +                   sss ( ipos2 : lenstr )
            END IF
	  ELSE
	    ttt = sss
	END IF
C
	outstr = ttt
C*
	RETURN
	END 
