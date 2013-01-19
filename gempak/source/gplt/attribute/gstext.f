	SUBROUTINE GSTEXT  ( itxfn, itxhw, sztext, itxwid,
     +			     ibrdr, irrotn, ijust, iret )
C************************************************************************
C* GSTEXT 								*
C* 									*
C* This subroutine sets the text attributes including the font		*
C* number, the text software/hardware flag, the text size and width,	*
C* the text border and blank fill flag, the text relative rotation,	*
C* flag, and the text justification.					*
C*									*
C* The border and blank fill flag is a three digit number, ABC, with	*
C* the following definitions:						*
C*									*
C*	A	Border		1 = no,  2 = yes			*
C*	B	Blank fill	1 = no,  2 = yes			*
C*	C	Border type						*
C*									*
C* GSTEXT  ( ITXFN, ITXHW, SZTEXT, ITXWID, IBRDR, IRROTN, IJUST, IRET )	*
C*									*
C* Input parameters:							*
C*	ITXFN		INTEGER		Text font			*
C*					  <=0 = no change		*
C*	ITXHW		INTEGER		Text sw/hw flag			*
C*					  1 = software 			*
C*					  2 = hardware			*
C*					  otherwise no change		*
C*	SZTEXT		REAL		Text size			*
C*					  <=0 = no change		*
C*	ITXWID		INTEGER		Text line width			*
C*					  <=0 = no change		*
C*	IBRDR		INTEGER		Text border/blank fill flag	*
C*					  <=0 = no change		*
C*	IRROTN		INTEGER		Text north-relative rot flag	*
C*					  1 = screen relative		*
C*					  2 = north relative		*
C*					  otherwise no change		*
C*	IJUST		INTEGER		Text justification		*
C*					   1 = left			*
C*					   2 = center			*
C*					   3 = right			*
C*					  otherwise no change		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	11/84	GEMPLT Version 3.0                      *
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Schotz/GSC	 1/90	Added text width			*
C* S. Jacobs/NCEP	 9/97	Added border, rotation and just flags	*
C* S. Jacobs/NCEP	12/97	Added no clipping flag to justification	*
C* S. Jacobs/NCEP	12/97	Removed no clipping flag		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check for justification greater than 10 and reduce to a
C*	single digit.
C
	IF  ( ijust .ge. 10 )  THEN
	    jjust = MOD ( ijust, 10 )
	  ELSE
	    jjust = ijust
	END IF
C
C*	Check if these are current requested characteristics.
C
	IF ( ( ( itxfn  .eq. ktxfn  ) .or. ( itxfn  .le. 0  ) ) .and.
     +	     ( ( itxhw  .eq. ktxhw  ) .or.
     +	       ( itxhw  .lt. 1  ) .or. ( itxhw  .gt. 2 ) ) .and.
     +	     ( ( sztext .eq. rtxsz  ) .or. ( sztext .le. 0. ) ) .and.
     +       ( ( itxwid .eq. ktxwid ) .or. ( itxwid .le. 0  ) ) .and.
     +	     ( ( ibrdr  .eq. kbrdr  ) .or. ( ibrdr  .le. 0  ) ) .and.
     +	     ( ( irrotn .eq. krrotn ) .or.
     +	       ( irrotn .lt. 1  ) .or. ( irrotn .gt. 2 ) ) .and.
     +	     ( ( jjust  .eq. kjust  ) .or.
     +	       ( jjust  .lt. 1  ) .or. ( jjust  .gt. 3 ) ) )  THEN
C
C*	    Set requested characteristics.
C
	  ELSE
	    IF  ( itxfn  .gt. 0  )  ktxfn  = itxfn
	    IF  ( ( itxhw .eq. 1 ) .or. ( itxhw .eq. 2 ) ) ktxhw = itxhw
	    IF  ( sztext .gt. 0. )  rtxsz  = sztext
	    IF  ( itxwid .gt. 0  )  ktxwid = itxwid
	    IF  ( ibrdr  .gt. 0  )  kbrdr  = ibrdr
	    IF  ( ( irrotn .eq. 1 ) .or. ( irrotn .eq. 2 ) )
     +		krrotn = irrotn
	    IF  ( ( jjust .ge. 1 ) .and. ( jjust .le. 3 ) )
     +		kjust = jjust
C
C*	    Send characteristics to device if not already set.
C
	    IF  ( ( ddev .ne. ' ' ) .and.
     +		  ( ( ktxfn  .ne. ltxfn  ) .or.
     +		    ( ktxhw  .ne. ltxhw  ) .or.
     +		    ( rtxsz  .ne. stxsz  ) .or.
     +		    ( ktxwid .ne. ltxwid ) .or.
     +		    ( kbrdr  .ne. lbrdr  ) .or.
     +		    ( krrotn .ne. lrrotn ) .or.
     +		    ( kjust  .ne. ljust  ) ) ) THEN
	        CALL DSTEXT  ( ktxfn, ktxhw, rtxsz, ktxwid,
     +			       kbrdr, krrotn, kjust,
     +			       ltxfn, ltxhw, stxsz, ltxwid,
     +			       lbrdr, lrrotn, ljust, iret )
	    END IF
	END IF
C*
	RETURN
	END
