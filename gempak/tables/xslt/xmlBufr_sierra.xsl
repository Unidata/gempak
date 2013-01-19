<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:import href="xmlBufr_common.xsl"/>
  <xsl:output method="xml" version="1.0" encoding="iso-8859-1" indent="yes"/>
  
  <!--
      xmlBufr_sierra.xsl
      
      Purpose: Create Sierra XML document for preparation of the GFA BUFR
      message.  Sierra is a bulletin designator for IFR and Mountain
      Obscuration hazards.  Input is a pre-XML document built internally
      by afcreatexml.c which uses the VGF el structure for input.
      
      Change Log:
      
      L. Hinson/AWC 01/06 Initial Coding
      L. Hinson/AWC 03/06 Updated for 5.9.2
      L. Hinson/AWC 08/09 Fixed MT_OBSC Hazard Parsing
  
  -->
    
  <xsl:variable name="x">
  </xsl:variable>
  
  <xsl:template match="hdr">
    <productValidTime>
      <xsl:call-template name="timePeriod">
        <xsl:with-param name="issueTime" select="issueTime"/>
        <xsl:with-param name="untilTime" select="untilTime"/>
      </xsl:call-template>
    </productValidTime>
  </xsl:template>
  <xsl:template match="smear">
    <xsl:element name="gfaObject">
      <xsl:attribute name="type"> <xsl:value-of select="hazard"/> </xsl:attribute>
      <xsl:element name="productStatus"> 
        <xsl:call-template name="productStatus">
          <xsl:with-param name="Status" select="Status"/> 
        </xsl:call-template>
        <xsl:element name="dataSig">
          <xsl:choose>
            <xsl:when test="contains(hazard,'MT_OBSC')">
              <xsl:attribute name="type">Mountain Obscuration</xsl:attribute>
              <xsl:attribute name="bufrCode">9</xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="type">IFR Ceiling and Visibility</xsl:attribute>
              <xsl:attribute name="bufrCode">8</xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:call-template name="GFAIdObsOrFcstLoc">
             <xsl:with-param name="id" select="tag"/>
             <xsl:with-param name="fcstHr" select="fcstHr"/>
             <xsl:with-param name="validFrom" select="validFrom"/>
             <xsl:with-param name="validUntil" select="validUntil"/>
             <xsl:with-param name="closeFlg" select="closeFlg"/>
             <xsl:with-param name="nPts" select="nLatLonPts"/>
             <xsl:with-param name="latPts" select="latPts"/>
             <xsl:with-param name="lonPts" select="lonPts"/>
             <xsl:with-param name="dueTo" select="DUE_TO"/>
             <xsl:with-param name="top" select="'0'"/>
             <xsl:with-param name="base" select="'0'"/>             
          </xsl:call-template>
          <flightRules type="IFR" bufrCode="1"/>
          <xsl:if test="contains(hazard,'IFR_CIG') or ((hazard='IFR') and contains(DUE_TO,'CIG BLW 010'))">
            <cloudBaseLimit type="Below" bufrCode="2"/>
            <cloudBase units="hundreds feet">010</cloudBase>
          </xsl:if>
          <xsl:if test="contains(hazard,'IFR_VIS')">
            <visibilityLimit type="Below" bufrCode="2"/>
            <visibility units="statute miles">3</visibility>
            <xsl:call-template name="parseObsc">
              <xsl:with-param name="obsc" select="substring-after(DUE_TO,'3SM ')"/>
              <xsl:with-param name="x" select="'1'"/>
            </xsl:call-template>
          </xsl:if>
          <xsl:if test="hazard='IFR'">
             <xsl:if test="contains(DUE_TO,'VIS BLW 3SM')">
               <visibilityLimit type="Below" bufrCode="2"/>
               <visibility units="statute miles">3</visibility>
               <xsl:call-template name="parseObsc">
                 <xsl:with-param name="obsc" select="substring-after(DUE_TO,'3SM ')"/>
                 <xsl:with-param name="x" select="'1'"/>
               </xsl:call-template>
             </xsl:if>
          </xsl:if>
          <xsl:if test="contains(hazard,'MT_OBSC')">
            <xsl:choose>
              <xsl:when test="contains(DUE_TO,'BY ')">
                <xsl:call-template name="parseObsc">
                  <xsl:with-param name="obsc" select="substring-after(DUE_TO,'BY ')"/>
                  <xsl:with-param name="x" select="'1'"/>  
                </xsl:call-template>
              </xsl:when>
              <xsl:otherwise>
                <obscuration type="CLDS" bufrFlag="14"/>
                <numberObscurations>1</numberObscurations>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:if>            
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>
         
  <xsl:template match="/">
    <gfaInfo>
      <bulletin type="SIERRA">
        <xsl:apply-templates/>
      </bulletin>
    </gfaInfo>    
  </xsl:template> 
</xsl:stylesheet>
      
