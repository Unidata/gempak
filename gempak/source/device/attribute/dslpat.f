	SUBROUTINE DSLPAT  ( linpat, jltyp, iret )
C************************************************************************
C* DSLPAT								*
C* 									*
C* This subroutine sets the current line pattern to the given pattern.	*
C* The line type number will be set to zero.  The values specified in	*
C* the line pattern designate the lenght of alternating on and off 	*
C* segments.  For example, a long-dash short-dash line pattern may be	*
C* specified by LINPAT values of:					*
C*									*
C*		10 5 5 5 0 0 0 0					*
C*									*
C* This pattern designation results in a line pattern that displays	*
C* a line for 10 units, space for 5 units, a line for 5 units, and	*
C* space for 5 units and then repeats.					*
C*									*
C* DSLPAT  ( LINPAT, JLTYP, IRET )					*
C*									*
C* Input parameters:							*
C*	LINPAT (8)	INTEGER		Line pattern			*
C*									*
C* Output parameters:							*
C*	JLTYP		INTEGER		Line pattern number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/89	Documentation				*
C* S. Schotz/GSC	 8/90	Return line pattern number		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		linpat (8)
C-----------------------------------------------------------------------
	iret = NORMAL
C
C*	Set active pattern. Find last non-zero segment.
C
	DO  i = 1, 8
	    actpat (i) = linpat (i) * lpscal
	    IF  ( actpat (i) .ne. 0. ) nseg = i
	END DO
C
C*	Set pattern number to zero to show user specified pattern.
C
	jltyp = 0
C
C*	Check that there is at least one segment.  If not, make this a
C*	solid line.
C
	IF  ( nseg .eq. 0 ) THEN
	    actpat (1) = 1000000 * lpscal
	    nseg       = 1
            jltyp = 1
	END IF
        mltyp = jltyp
C*
	RETURN
	END
