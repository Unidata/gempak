	SUBROUTINE GLIMIT ( gglims, iret )
C************************************************************************
C* GLIMIT                                                               *
C*                                                                      *
C* This subroutine breaks apart the GGLIMS input parameter.		*
C*                                                                      *
C* GLIMIT ( GGLIMS, IRET )						*
C*                                                                      *
C* Input parameters:                                                    *
C*      GGLIMS           CHAR		LIMIT input parameter		*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP      9/99                                           *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'
C*
	CHARACTER*(*)	gglims
C*
	CHARACTER*32	clims(3)
	REAL		vals(2)
C*------------------------------------------------------------------------
C
	CALL ST_CLST ( gglims, '|', ' ',  3, clims, num, iret )
C
	CALL ST_RLST ( clims(1), ';', RMISSD,  2, vals, num, iret )
	lochk = vals(1)
	loval = vals(2)
C
	CALL ST_RLST ( clims(2), ';', RMISSD,  2, vals, num, iret )
	hichk = vals(1)
	hival = vals(2)
C
	CALL ST_RLST ( clims(3), ' ', RMISSD,  1, vals, num, iret )
	defalt = vals(1)
C
C
	RETURN
	END
