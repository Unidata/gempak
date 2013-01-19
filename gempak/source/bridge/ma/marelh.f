        SUBROUTINE  MA_RELH  ( marrpt, ipt, iret )
C************************************************************************
C* MA_RELH                                                              *
C*                                                                      *
C* This subroutine decodes the relative humidity group 29UUU, where UUU *
C* is the relative humidity of the air in percentage.                   *
C*                                                                      *
C* MA_RELH  ( MARRPT, IPT, IRET ) 	                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT          CHAR*           Report array                    *
C*                                                                      *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         On input, points to first U in  *
C*					29UUU group; on output, points  *
C*					to last U			*
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRRELH)  REAL            Relative humidity in percent    *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* D. Kidwell/NCEP      4/97	Removed inteface calls, reorganized     *
C*                              header and comments                     *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
	INCLUDE		'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER   	fld3*3
C------------------------------------------------------------------------
        iret = 0
C
        fld3 = marrpt (ipt:ipt+2)
        CALL  ST_CRNM ( fld3, rval, ier )
        ipt = ipt + 2
        IF ( ier .eq. 0 ) rivals ( irrelh ) = rval
C*
        RETURN
        END
