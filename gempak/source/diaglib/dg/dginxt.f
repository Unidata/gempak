        SUBROUTINE DG_INXT  ( chngnv, coladd, time, iret )
C************************************************************************
C* DG_INXT                                                              *
C*                                                                      *
C* This subroutine initialize processing for the given time.            *
C*                                                                      *
C* DG_INXT  ( CHNGNV, COLADD, TIME, IRET )				*
C*                                                                      *
C* Input parameters:                                                    *
C*      CHNGNV          LOGICAL         Flag to change navigation       *
C*	COLADD		LOGICAL		Flag to add a column of data	*
C*	TIME (2)	CHAR*		The given time			*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*					-50 = navigation not same	*
C*					-57 = grid file open failed	*
C**                                                                     *
C* Log:                                                                 *
C* R. Tian/SAIC          3/06   Fortran wrapper of DGC_INXT             *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	LOGICAL		chngnv, coladd
        CHARACTER*(*)  	time (2)
C*
        CHARACTER       time1*(LLMXLN), time2*(LLMXLN)
C------------------------------------------------------------------------
	CALL ST_NULL ( time (1), time1, nt, ier )
	CALL ST_NULL ( time (2), time2, nt, ier )
        CALL DGC_INXT ( chngnv, coladd, time1, time2, iret )
C*
	RETURN
	END
