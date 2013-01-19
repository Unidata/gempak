        SUBROUTINE DG_GLEV ( intry, gdattm, ivcord, maxlev, levarr, 
     +                       nlev, iret )
C************************************************************************
C* DG_GLEV                                                              *
C*                                                                      *
C* This subroutine returns all the levels present in a grid file for    *
C* a given date and vertical coordinate.  The levels returned are       *
C* not sorted.                                                          *
C*                                                                      *
C* DG_GLEV  ( INTRY, GDATTM, IVCORD, MAXLEV, LEVARR, NLEV, IRET )    	*
C*                                                                      *
C* Input parameters:                                                    *
C*      INTRY           INTEGER         GDFILE entry number (usually 1) *
C*      GDATTM (2)      CHAR*20         GEMPAK times                    *
C*      IVCORD          INTEGER         Vertical coordinate             *
C*      MAXLEV          INTEGER         Maximum number of levels        *
C*                                                                      *
C* Output parameters:                                                   *
C*      LEVARR (2,NLEV) INTEGER         Levels found                    *
C*      NLEV            INTEGER         Number of levels found          *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                      -55 = file open failed		*
C*                                      -66 = cannot get levels		*
C**                                                                     *
C* Log:                                                                 *
C* R. Tian/SAIC          3/06   Fortran wrapper of DGC_GLEV             *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER*(*)   gdattm (2)
	INTEGER         levarr (2,*)
C*
	CHARACTER	gda1*(LLMXLN), gda2*(LLMXLN)	
	INTEGER		level1 (LLMXLV), level2 (LLMXLV)
C------------------------------------------------------------------------
	CALL ST_NULL ( gdattm (1), gda1, nt, ier )
	CALL ST_NULL ( gdattm (2), gda2, nt, ier )
        CALL DGC_GLEV ( intry, gda1, gda2, ivcord, maxlev, level1,
     +                  level2, nlev, iret )
	DO i = 1, nlev
	    levarr (1,i) = level1 (i)
	    levarr (2,i) = level2 (i)
	END DO
C*
	RETURN
	END
