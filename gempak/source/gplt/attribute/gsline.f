	SUBROUTINE GSLINE  ( iltyp, ilthw, iwidth, ilwhw, iret )
C************************************************************************
C* GSLINE								*
C* 									*
C* This subroutine sets the line attributes including the line type	*
C* number, the software/hardware line type flag, the line width,	*
C* and the software/hardware line width flag.  				*
C*									*
C* The line type is specified by a two-digit number.  The units digit	*
C* is the dash pattern; the tens digit is the scaling to be applied,	*
C* with 2 as the default.  The line dash patterns are:			*
C*									*
C*	 1	solid							*
C*	 2	short dash						*
C*	 3	medium dash						*
C*	 4	long dash, short dash					*
C*	 5	long dash						*
C*	 6	long dash, short dash, short dash, short dash		*
C*	 7	long dash, dot						*
C*	 8	medium dash, dot, dot, dot				*
C*	 9	short dash, dot						*
C*	10	dots							*
C*									*
C* GSLINE  ( ILTYP, ILTHW, IWIDTH, ILWHW, IRET )			*
C*									*
C* Input parameters:							*
C*	ILTYP		INTEGER		Line type			*
C*					  <=0 = no change		*
C*	ILTHW		INTEGER		Sw/hw line type flag		*
C*					  1 = software			*
C*					  2 = hardware			*
C*					  otherwise no change		*
C*	IWIDTH		INTEGER		Line width size multiplier	*
C*					  <=0 = no change		*
C*	ILWHW		INTEGER		Sw/hw line width flag		*
C*					  1 = software			*
C*					  2 = hardware			*
C*					  otherwise no change		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 5/89	Set size = 0 to no change		*
C* S. Schotz/GSC	 2/90	allow line type to be 1 to 99		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check if these are current requested characteristics.
C
	 IF  ( ( ( iltyp  .eq. kltyp ) .or. ( iltyp. lt.  1 )   .or.
     +					    ( iltyp .gt.  99 ) ) .and.
     +	       ( ( ilthw  .eq. klthw ) .or. ( ilthw .lt.  1 )   .or.
     +					    ( ilthw .gt.  2 ) ) .and.
     +	       ( ( iwidth .eq. klwid ) .or. ( iwidth .le. 0 ) ) .and.
     +	       ( ( ilwhw  .eq. klwhw ) .or. ( ilwhw .lt.  1 )   .or.
     +					    ( ilwhw .gt.  2 ) ) )  THEN
C
C*	    Set requested parameters.
C
	  ELSE
	    IF ( ( iltyp .ge. 1 ) .and. ( iltyp .le. 99 ) ) 
     +            kltyp = iltyp
	    IF ( ( ilthw .eq. 1 ) .or.  ( ilthw .eq. 2 ) ) klthw = ilthw
	    IF (  iwidth .gt. 0 ) klwid = iwidth
	    IF ( ( ilwhw .eq. 1 ) .or. ( ilwhw .eq. 2 ) ) klwhw = ilwhw
C
C*	    Send characteristics to device if not already set.
C
	    IF  ( ( ddev .ne. ' ' ) .and.
     +		  ( ( kltyp .ne. lltyp ) .or. ( klthw .ne. llthw ) .or.
     +		  ( klwid .ne. llwid ) .or. ( klwhw .ne. llwhw ) ) )
     +								THEN
	        CALL DSLINE  ( kltyp, klthw, klwid, klwhw, lltyp, 
     +			       llthw, llwid, llwhw, iret )
	    END IF
	END IF
C*
	RETURN
	END
