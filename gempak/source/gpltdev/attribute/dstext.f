	SUBROUTINE DSTEXT  ( itxfn, itxhw, sztext, itxwid,
     +			     ibrdr, irrotn, ijust,
     +			     jtxfn, jtxhw, size, jtxwid,
     +			     jbrdr, jrrotn, jjust, iret )
C************************************************************************
C* DSTEXT 								*
C* 									*
C* This subroutine sets the text attributes including the font		*
C* number, the text software/hardware flag, the text size and width,	*
C* the text border and blank fill flag, the text relative rotation	*
C* flag, and the text justification.					*
C*									*
C* The border and blank fill flag is a three digit number, ABC, with	*
C* the following definitions:						*
C*									*
C*	A	Border		1 = no,  2 = yes			*
C*	B	Blank fill	1 = no,  2 = yes			*
C*	C	Border type						*
C* 									*
C* DSTEXT  ( ITXFN, ITXHW, SZTEXT, ITXWID, IBRDR, IRROTN, IJUST,	*
C*	     JTXFN, JTXHW, SIZE, JTXWID, JBRDR, JRROTN, JJUST, IRET )	*
C*									*
C* Input parameters:							*
C*	ITXFN		INTEGER		Text font			*
C*					  <=0 = no change		*
C*	ITXHW		INTEGER		Text sw/hw flag			*
C*					  1 = software text		*
C*					  2 = hardware text		*
C*					  otherwise no change		*
C*	SZTEXT		REAL		Text size			*
C*					  <=0 = no change		*
C* 	ITXWID		INTEGER		Text width			*
C*					  <=0 = no change		*
C*	IBRDR           INTEGER         Text border/blank fill flag     *
C*					  <=0 = no change		*
C*	IRROTN		INTEGER		Text north-relative rot flag	*
C*					  1 = screen relative		*
C*					  2 = north relative		*
C*					  otherwise no change		*
C*	IJUST		INTEGER		Text justification		*
C*					   1 = left			*
C*					   2 = center			*
C*					   3 = right			*
C*					  11 = left no clipping		*
C*					  12 = center no clipping	*
C*					  13 = right no clipping	*
C*					  otherwise no change		*
C*									*
C* Output parameters:							*
C*	JTXFN		INTEGER		Text font			*
C*	JTXHW		INTEGER		Text sw/hw flag			*
C*	SIZE		REAL		Text size			*
C* 	JTXWID		INTEGER		Text width			*
C*	JBRDR           INTEGER         Text border/blank fill flag     *
C*	JRROTN		INTEGER		Text north-relative rot flag	*
C*	JJUST		INTEGER		Text justification		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	 9/88	Doc and returns				*
C* M. desJardins/GSFC	 5/89	Documentation				*
C* S. Schotz/GSC	 1/90	Added text width			*
C* S. Jacobs/NCEP	 9/97	Added border, rotation and just flags	*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C
	INTEGER		isend (4), ircv (2), ircva (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 9
	isend (2) = CSTEXT
	isend (3) = itxfn
	isend (4) = itxhw
C
	CALL GPUT  ( isend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( sztext, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	isend (1) = itxwid
	isend (2) = ibrdr
	isend (3) = irrotn
	isend (4) = ijust
C
	CALL GPUT  ( isend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	  ELSE
C
C*	    Get the text output variables.
C
	    CALL GGET  ( ircv, 2, iret ) 
	    IF  ( iret .ne. NORMAL )  RETURN
C
	    jtxfn = ircv (1)
	    jtxhw = ircv (2)
C
	    CALL GGETR  ( size, 1, iret ) 
	    IF  ( iret .ne. NORMAL )  RETURN
C
	    CALL GGET  ( ircva, 4, iret )
	    IF  ( iret .ne. NORMAL )  RETURN
C
	    jtxwid = ircva (1)
	    jbrdr  = ircva (2)
	    jrrotn = ircva (3)
	    jjust  = ircva (4)
C
C*	    Set the ACTIVE common block text variables.
C
	    mtxfn  = jtxfn
	    mtxhw  = jtxhw
	    ttxsz  = size
	    mtxwid = jtxwid
	    mbrdr  = jbrdr
	    mrrotn = jrrotn
	    mjust  = jjust
	END IF
C*
	RETURN
	END
