	SUBROUTINE SNMCLR  ( ncol, prmlst, colors, icolor, iret )
C************************************************************************
C* SNMCLR								*
C*									*
C* This subroutine translates the colors.				*
C*									*
C* SNMCLR  ( NCOL, PRMLST, COLORS, ICOLOR, IRET )			*
C*									*
C* Input parameters:							*
C*	NCOL		INTEGER		Number of colors		*
C*	PRMLST(*)	CHAR*		Parameter names			*
C*	COLORS		CHAR*		COLORS input			*
C*									*
C* Output parameters:							*
C*	ICOLOR (*)	INTEGER		Colors array			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	Reorganized				*
C* S. Schotz/GSC	 8/90	Reorganized as SFMCLR			*
C************************************************************************
	CHARACTER*(*)	colors, prmlst (*)
	INTEGER		icolor (*)
C*
	INTEGER		icbuff (11)
C------------------------------------------------------------------------
	CALL IN_COLR  ( colors, ncol, icbuff, iret )
C
C*	Turn off colors for 'BLNK' and 'SPAC'.
C
	ic  = 0
	DO  i = 1, ncol
            IF  ( (prmlst (i) .eq. 'BLNK') .or. 
     1            (prmlst(i) .eq. 'SPAC') ) THEN
                icolor (i) = 0
              ELSE
		ic = ic + 1
		icolor (i) = icbuff (ic)
	    END IF
	END DO
C*
	RETURN
	END	
