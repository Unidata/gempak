	SUBROUTINE G2T_OPEN ( lunb, lunp, lunz, iret )
C************************************************************************
C* G2T_OPEN  								*
C*									*
C* This subroutine opens the G2T text message and zone table files.	*
C*									*
C* G2T_OPEN ( LUNT, LUNB, IRET )					*
C*									*
C* Input parameters:							*
C* Output parameters:							*
C*	LUNB		INTEGER		LUN for G2T_TXT.TBL		*
C*	LUNP		INTEGER		LUN for G2T_PARM.TBL		*
C*	LUNZ		INTEGER		LUN for G2T_ZONE.TBL		*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					-1 = cannot open file		*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		 9/06						*
C* T. Lee/SAIC		 9/07	Changed G2T to GOFTXT			*
C* T. Lee/SAIC		11/07	Read G2T parameters			*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER	g2txtb*12, g2tznb*12, g2tprm*12
	DATA		g2txtb /'g2t_txt.tbl' /
	DATA		g2tprm /'g2t_parm.tbl' /
	DATA		g2tznb /'g2t_zone.tbl' /
C-----------------------------------------------------------------------
	iret = 0
C
	IF ( .not. tbread )  THEN
	    CALL FL_TBOP ( g2txtb, 'goftxt', lunb, ier1 )
	    CALL FL_TBOP ( g2tznb, 'goftxt', lunz, ier2 )
	    CALL FL_TBOP ( g2tprm, 'goftxt', lunp, ier3 )
	    tbread = .true.
	END IF
C
	ier = ier1 + ier2 + ier3
	IF  ( ier .lt. 0 )  THEN
	    iret = - 1
	    RETURN
	END IF
C*
	RETURN
	END
