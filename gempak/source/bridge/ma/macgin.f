	SUBROUTINE MA_CGIN ( iret )
C************************************************************************
C* MA_CGIN                                                              *
C*                                                                      *
C* This subroutine initializes certain Coast Guard report parameters to	*
C* missing for for a new report.  All initialized values are in common. *
C*                                                                      *
C* MA_CGIN ( IRET )	       	                                        *
C*                                                                      *
C* Output parameters:                                                   *
C*      IWXVSB          INTEGER         Indicates position of wx/vis    *
C*      IWIND           INTEGER         Indicates position of wind      *
C*      IWAVE           INTEGER         Indicates position of wave data *
C*      ISEA            INTEGER         Indicates position of sst data  *
C*      IAIR            INTEGER         Indicates position of air temp. *
C*      IPRES           INTEGER         Indicates position of slp data  *
C*      IRMK            INTEGER         Indicates position of remarks   *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* C. Caruso Magee/NCEP  4/01   Created for Coast Guard data.		*
C* F. J. Yen/NCEP	 4/01	Renamed from CG_INIT.  Changed values	*
C*				of common variables since type changed. *
C************************************************************************
        INCLUDE 	'macmn.cmn'
C-----------------------------------------------------------------------
        iret   = 0
C
        iwxvsb = 0
        iwind  = 0
        iwave  = 0
        isea   = 0
        iair   = 0
        ipres  = 0
        irmk   = 0
        gothdr = .false.
C*
        RETURN
        END
