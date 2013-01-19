#
#	Makefile for GEMPAK 6.0
#		Builds melbufr.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/libmel_bufr.a

COBJS = \
	$(ALIB)(dump_print.o) \
	$(ALIB)(AL_Clear.o) \
	$(ALIB)(AL_Destroy.o) \
	$(ALIB)(AL_First.o) \
	$(ALIB)(AL_Init.o) \
	$(ALIB)(AL_Last.o) \
	$(ALIB)(AL_Print.o) \
	$(ALIB)(AL_Put.o) \
	$(ALIB)(AL_Remove.o) \
	$(ALIB)(AL_Size.o) \
	$(ALIB)(BS_Destroy.o) \
	$(ALIB)(BS_Flush.o) \
	$(ALIB)(BS_Get.o) \
	$(ALIB)(BS_Increase.o) \
	$(ALIB)(BS_Init.o) \
	$(ALIB)(BS_Position.o) \
	$(ALIB)(BS_Put.o) \
	$(ALIB)(BS_Rewind.o) \
	$(ALIB)(B_Abort.o) \
	$(ALIB)(B_Add_AF.o) \
	$(ALIB)(B_AtEOD.o) \
	$(ALIB)(B_AtEOF.o) \
	$(ALIB)(B_AtEOM.o) \
	$(ALIB)(B_BitWidth.o) \
	$(ALIB)(B_Cancel_AF.o) \
	$(ALIB)(B_Change_DW.o) \
	$(ALIB)(B_Change_RV.o) \
	$(ALIB)(B_Change_S.o) \
	$(ALIB)(B_Close.o) \
	$(ALIB)(B_DS_Empty.o) \
	$(ALIB)(B_DS_Entry_Put.o) \
	$(ALIB)(B_DS_Free.o) \
	$(ALIB)(B_DS_Init.o) \
	$(ALIB)(B_Debug.o) \
	$(ALIB)(B_DebugLevel.o) \
	$(ALIB)(B_Decode.o) \
	$(ALIB)(B_Def_Dataset.o) \
	$(ALIB)(B_Destroy.o) \
	$(ALIB)(B_Encode.o) \
	$(ALIB)(B_Err_Clear.o) \
	$(ALIB)(B_Err_Init.o) \
	$(ALIB)(B_Err_Log.o) \
	$(ALIB)(B_Err_Print.o) \
	$(ALIB)(B_Err_Set.o) \
	$(ALIB)(B_FTP_GetFile.o) \
	$(ALIB)(B_FindSeq.o) \
	$(ALIB)(B_Find_End.o) \
	$(ALIB)(B_Get_Array.o) \
	$(ALIB)(B_Get_Dataset.o) \
	$(ALIB)(B_Get_OptData.o) \
	$(ALIB)(B_Get_S3Data.o) \
	$(ALIB)(B_Get_S4Data.o) \
	$(ALIB)(B_Get_Value.o) \
	$(ALIB)(B_Globals.o) \
	$(ALIB)(B_Info_Init.o) \
	$(ALIB)(B_Info_Print.o) \
	$(ALIB)(B_Info_Verify.o) \
	$(ALIB)(B_Init.o) \
	$(ALIB)(B_Init_Sub.o) \
	$(ALIB)(B_IsComment.o) \
	$(ALIB)(B_IsError.o) \
	$(ALIB)(B_MaxVal.o) \
	$(ALIB)(B_Mix_Print.o) \
	$(ALIB)(B_NumDatasets.o) \
	$(ALIB)(B_Pos_Msg.o) \
	$(ALIB)(B_Print.o) \
	$(ALIB)(B_ProcMethod.o) \
	$(ALIB)(B_ProcType.o) \
	$(ALIB)(B_Put_Array.o) \
	$(ALIB)(B_Put_MixArray.o) \
	$(ALIB)(B_Put_OptData.o) \
	$(ALIB)(B_Put_S3Data.o) \
	$(ALIB)(B_Put_S4Data.o) \
	$(ALIB)(B_Put_String.o) \
	$(ALIB)(B_Put_Value.o) \
	$(ALIB)(B_Read_Msg.o) \
	$(ALIB)(B_Read_Tables.o) \
	$(ALIB)(B_Reset_DW.o) \
	$(ALIB)(B_Reset_RV.o) \
	$(ALIB)(B_Reset_S.o) \
	$(ALIB)(B_Set_Missing_Value.o) \
	$(ALIB)(B_Set_Multiple_Msg_Flag.o) \
	$(ALIB)(B_TO_Multiple_Msg_Flag.o) \
	$(ALIB)(B_Trace.o) \
	$(ALIB)(B_TraceLevel.o) \
	$(ALIB)(B_ValArray_Destroy.o) \
	$(ALIB)(B_Val_Array_free.o) \
	$(ALIB)(B_Val_Print.o) \
	$(ALIB)(B_Val_free.o) \
	$(ALIB)(B_Write_Msg.o) \
	$(ALIB)(B_perror.o) \
	$(ALIB)(B_strdup.o) \
	$(ALIB)(BytesInBits.o) \
	$(ALIB)(Check_Compress.o) \
	$(ALIB)(Compression.o) \
	$(ALIB)(Create_TableB.o) \
	$(ALIB)(Create_TableD.o) \
	$(ALIB)(DL_Destroy.o) \
	$(ALIB)(DL_Encode.o) \
	$(ALIB)(DL_First.o) \
	$(ALIB)(DL_Init.o) \
	$(ALIB)(DL_Last.o) \
	$(ALIB)(DL_NumFXYs.o) \
	$(ALIB)(DL_Print.o) \
	$(ALIB)(DL_Put.o) \
	$(ALIB)(DL_Size.o) \
	$(ALIB)(D_Compression.o) \
	$(ALIB)(EV_Destroy.o) \
	$(ALIB)(EV_Get.o) \
	$(ALIB)(EV_Init.o) \
	$(ALIB)(EV_IsBad.o) \
	$(ALIB)(EV_Print.o) \
	$(ALIB)(EV_RVSet.o) \
	$(ALIB)(EV_Set.o) \
	$(ALIB)(EncodeValue.o) \
	$(ALIB)(FL_Destroy.o) \
	$(ALIB)(FL_Expand.o) \
	$(ALIB)(FL_First.o) \
	$(ALIB)(FL_Get.o) \
	$(ALIB)(FL_Init.o) \
	$(ALIB)(FL_Insert.o) \
	$(ALIB)(FL_Last.o) \
	$(ALIB)(FL_Prev.o) \
	$(ALIB)(FL_Print.o) \
	$(ALIB)(FL_Put.o) \
	$(ALIB)(FL_Remove.o) \
	$(ALIB)(FL_Size.o) \
	$(ALIB)(FXY_Expand.o) \
	$(ALIB)(FXY_F_Value.o) \
	$(ALIB)(FXY_Get_DW.o) \
	$(ALIB)(FXY_Get_RV.o) \
	$(ALIB)(FXY_Get_S.o) \
	$(ALIB)(FXY_Get_Vals.o) \
	$(ALIB)(FXY_IsLocal.o) \
	$(ALIB)(FXY_IsRepl.o) \
	$(ALIB)(FXY_IsTableB.o) \
	$(ALIB)(FXY_IsTableC.o) \
	$(ALIB)(FXY_IsTableD.o) \
	$(ALIB)(FXY_Pack.o) \
	$(ALIB)(FXY_Pack_Dec.o) \
	$(ALIB)(FXY_Print.o) \
	$(ALIB)(FXY_PrintVals.o) \
	$(ALIB)(FXY_PtrInc.o) \
	$(ALIB)(FXY_String.o) \
	$(ALIB)(FXY_Units.o) \
	$(ALIB)(FXY_UnitsType.o) \
	$(ALIB)(FXY_Unpack.o) \
	$(ALIB)(FXY_Unpack_Dec.o) \
	$(ALIB)(FXY_X_Value.o) \
	$(ALIB)(FXY_Y_Value.o) \
	$(ALIB)(F_Print_R.o) \
	$(ALIB)(FileExists.o) \
	$(ALIB)(Find_Data_Type.o) \
	$(ALIB)(Find_Status.o) \
	$(ALIB)(Flag_Print.o) \
	$(ALIB)(Get_BUFR_Edition.o) \
	$(ALIB)(Get_BUFR_Log_File.o) \
	$(ALIB)(Get_BUFR_Log_File_prt.o) \
	$(ALIB)(Get_BUFR_MasterTableNum.o) \
	$(ALIB)(Get_Century.o) \
	$(ALIB)(Get_DataCategory.o) \
	$(ALIB)(Get_DataSubCategory.o) \
	$(ALIB)(Get_Day.o) \
	$(ALIB)(Get_Hour.o) \
	$(ALIB)(Get_HourMinute.o) \
	$(ALIB)(Get_LocalTableVer.o) \
	$(ALIB)(Get_MasterTableVer.o) \
	$(ALIB)(Get_Minute.o) \
	$(ALIB)(Get_Month.o) \
	$(ALIB)(Get_ObservedDataFlag.o) \
	$(ALIB)(Get_OriginatingCenter.o) \
	$(ALIB)(Get_OriginatingCenterID.o) \
	$(ALIB)(Get_SubCenter.o) \
	$(ALIB)(Get_UpdateSeqNum.o) \
	$(ALIB)(Get_Val_Double.o) \
	$(ALIB)(Get_Val_FXY.o) \
	$(ALIB)(Get_Val_Int.o) \
	$(ALIB)(Get_Val_Scale.o) \
	$(ALIB)(Get_Val_String.o) \
	$(ALIB)(Get_Val_Type.o) \
	$(ALIB)(Get_YMD.o) \
	$(ALIB)(Get_Year.o) \
	$(ALIB)(HS_Destroy.o) \
	$(ALIB)(HS_Get.o) \
	$(ALIB)(HS_GetBit.o) \
	$(ALIB)(HS_RVSet.o) \
	$(ALIB)(HS_Set.o) \
	$(ALIB)(HS_SetBit.o) \
	$(ALIB)(Int2ToInt.o) \
	$(ALIB)(Int3ToInt.o) \
	$(ALIB)(IntToInt2.o) \
	$(ALIB)(IntToInt3.o) \
	$(ALIB)(Is_Val_Missing.o) \
	$(ALIB)(Loop_T_Data_Type.o) \
	$(ALIB)(Miss_ValRep.o) \
	$(ALIB)(Num_Messages.o) \
	$(ALIB)(PrintDivider.o) \
	$(ALIB)(Prn_Comprss.o) \
	$(ALIB)(Set_Center_Info.o) \
	$(ALIB)(Set_Compress.o) \
	$(ALIB)(Set_DataCategory.o) \
	$(ALIB)(Set_Date.o) \
	$(ALIB)(Set_Edition_MastTab.o) \
	$(ALIB)(Set_Flag.o) \
	$(ALIB)(Set_MasterTableVer.o) \
	$(ALIB)(Set_ObservedData.o) \
	$(ALIB)(Set_Status.o) \
	$(ALIB)(Set_Time.o) \
	$(ALIB)(Set_UpdateSequence.o) \
	$(ALIB)(T0_Destroy.o) \
	$(ALIB)(T0_Init.o) \
	$(ALIB)(T0_Print.o) \
	$(ALIB)(T0_Read.o) \
	$(ALIB)(T0_Value.o) \
	$(ALIB)(TA_Destroy.o) \
	$(ALIB)(TA_Init.o) \
	$(ALIB)(TA_Print.o) \
	$(ALIB)(TA_Read.o) \
	$(ALIB)(TA_Value.o) \
	$(ALIB)(TB_Destroy.o) \
	$(ALIB)(TB_Get.o) \
	$(ALIB)(TB_GetUnits.o) \
	$(ALIB)(TB_Init.o) \
	$(ALIB)(TB_Print.o) \
	$(ALIB)(TB_Put.o) \
	$(ALIB)(TB_Read.o) \
	$(ALIB)(TC_Find.o) \
	$(ALIB)(TDS_Destroy.o) \
	$(ALIB)(TDS_Init.o) \
	$(ALIB)(TDS_Put.o) \
	$(ALIB)(TD_Destroy.o) \
	$(ALIB)(TD_Expand.o) \
	$(ALIB)(TD_Init.o) \
	$(ALIB)(TD_Match.o) \
	$(ALIB)(TD_Print.o) \
	$(ALIB)(TD_Read.o) \
	$(ALIB)(TenPow.o) \
	$(ALIB)(TruncateValue.o) \
	$(ALIB)(TwoPow.o) \
	$(ALIB)(VS_Clear.o) \
	$(ALIB)(VS_Destroy.o) \
	$(ALIB)(VS_Get.o) \
	$(ALIB)(VS_Init.o) \
	$(ALIB)(VS_IsEmpty.o) \
	$(ALIB)(VS_Pop.o) \
	$(ALIB)(VS_Push.o) \
	$(ALIB)(VoidInc.o) \
	$(ALIB)(VoidVal.o)

LOBJS = 

PROGS = bufr_dump dataset_dmp

CFLAGS = $(COPT) $(BUFRFLAGS) \
	-I../include \
	-DDEBUG_PRINT=0 \
	-DFNOC_KLUDGE=0 \
	-DIGNORE_NESTING=1 \
	-DMEMORY_DEBUG=0 \
	-DTRACE_PRINT=0 
 
.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROGS)

bufr_dump: bufr_dump.o
	$(LINK.f) $(NOFORMAIN) -o $@ $? $(ALIB)

dataset_dmp: dataset_dmp.o
	$(LINK.f) $(NOFORMAIN) -o $@ $? $(ALIB)

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


install: $(PROGS)
	$(CP) $(PROGS) $(GEMEXE)
	
clean:
	$(RM) *.o $(PROGS)

