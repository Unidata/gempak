	SUBROUTINE GSTEXT  ( itxfn, itxhw, sztext, itxwid,
     +			     ibrdr, irrotn, ijust, iret )
C************************************************************************
C* GSTEXT 								*
C* 									*
C* This subroutine sets the text attributes, including the font 	*
C* number, the text software/hardware flag, the text size and width,	*
C* the text border and blank fill flag, the text relative rotation	*
C* flag, and the text justification.  If these parameters are not       *
C* positive, no change is made.						*
C*									*
C* The border and blank fill flag is a three digit number, with each of *
C* the three digits representing a separate piece of information.  The  *
C* border and blank fill flag is interpreted as follows:                *
C*                                                                      *
C*   digit 1: border     digit 2: blank fill    digit 3: border type    *
C*   ---------------     -------------------    --------------------    *
C*   1 = no              1 = no                 1 = regular box         *
C*   2 = yes             2 = yes                2 = low pressure box    *
C*                                              3 = high pressure box   *
C*                                              4 = freezing level box  *
C*                                              5 = underline           *
C*                                              6 = overline		*
C*									*
C* GSTEXT  ( ITXFN, ITXHW, SZTEXT, ITXWID, IBRDR, IRROTN, IJUST, IRET )	*
C*									*
C* Input parameters:							*
C*	ITXFN		INTEGER		Font number 			*
C*	ITXHW		INTEGER		Text software/hardware flag	*
C*					  1 = software 			*
C*					  2 = hardware 			*
C*					  otherwise no change		*
C*	SZTEXT		REAL		Text size			*
C*	ITXWID		INTEGER		Text width			*
C*	IBRDR		INTEGER		Text border/blank fill flag	*
C*	IRROTN		INTEGER		Text north-relative rot flag	*
C*					  1 = screen relative 		*
C*					  2 = north relative 		*
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
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* S. Schotz/GSC	 1/90	Added text width			*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* S. Jacobs/NCEP	 9/97	Added border, rotation and just flags	*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C* S. Jacobs/NCEP	10/05	Updated prolog to add overline info	*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C
	INTEGER 	isend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 9
	isend (2) = FSTEXT
	isend (3) = itxfn
	isend (4) = itxhw
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
	CALL GPUT  ( isend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
