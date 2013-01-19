       SUBROUTINE MA_CGID ( cgrpt, mszrpt, ipt, iret )
C************************************************************************
C* MA_CGID                                                              *
C*                                                                      *
C* This subroutine decodes the Coast Guard station id in one report.    *
C* The value is stored in common /cintfv/.				*
C*                                                                      *
C* MA_CGID  ( CGRPT, MSZRPT, IPT, IRET )                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      CGRPT           CHAR*           Report array                    *
C*      MSZRPT          INTEGER         Length of report in bytes       *
C*                                                                      *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         Pointer to start of field.      *
C*                                                                      *
C* Output parameters:                                                   *
C*      CIVALS(ICSTID)  CHAR*           Station id                      *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                       -1 = ID is missing             *
C*                                       -2 = bad format                *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP  4/01	Original Author                         *
C* F. J. Yen/NCEP	 4/01	Cleaned up, reformatted, and renamed	*
C*				from CG_STID. Changed prologue.		*
C************************************************************************
       	INCLUDE       'macmn.cmn'
C*
       	CHARACTER*(*) cgrpt
C------------------------------------------------------------------------
       	iret = 0
C
C*  	First 3 characters of cgrpt will be id.
C*  	If ID consists of ???, set STID to missing.
C*  	IF 1st, 2nd, or 3rd char is blank, leave stid as missing (blank).
C
       	IF ( cgrpt(1:3) .ne. '???' ) THEN
            IF ( INDEX ( cgrpt(1:3), ' ' ) .eq. 0 ) THEN
        	civals(icstid)(1:1) = 'K'
        	civals(icstid)(2:4) = cgrpt(1:3)
              ELSE
                iret = -2
            END IF
          ELSE
            iret = -1
        END IF

        ipt = ipt + 4
C*
        RETURN
        END
