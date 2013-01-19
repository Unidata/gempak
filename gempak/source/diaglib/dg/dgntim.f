        SUBROUTINE DG_NTIM  ( chngnv, coladd, time, nxtok, iret )
C************************************************************************
C* DG_NTIM                                                              *
C*                                                                      *
C* This subroutine initialize processing for the next time.             *
C*                                                                      *
C* DG_NTIM  ( CHNGNV, COLADD, TIME, NXTOK, IRET )			*
C*                                                                      *
C* Input parameters:                                                    *
C*      CHNGNV          LOGICAL         Flag to change navigation       *
C*	COLADD		LOGICAL		Flag to add a column of data	*
C*                                                                      *
C* Output parameters:                                                   *
C*	TIME (2)	CHAR*		Next time from dtmlst		*
C*	NXTOK		LOGICAL		Flag for existance of next time	*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*					-50 = navigation not same	*
C*					-57 = grid file open failed	*
C**                                                                     *
C* Log:                                                                 *
C* R. Tian/SAIC		 3/06	Fortran wrapper of DGC_NTIM		*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	LOGICAL		chngnv, coladd, nxtok
        CHARACTER*(*)  	time (2)
C------------------------------------------------------------------------
	CALL DGC_NTIM ( chngnv, coladd, time (1), time (2), nxtok, iret )
	CALL ST_RNUL ( time (1), time (1), lens, ier )
	CALL ST_RNUL ( time (2), time (2), lens, ier )
C*
	RETURN
	END
