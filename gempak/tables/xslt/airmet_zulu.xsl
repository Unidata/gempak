<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:import href="no_hazard_report.xsl"/>
    <xsl:import href="get_status.xsl"/>
    <xsl:import href="get_update.xsl"/>   
    <xsl:import href="get_attention_line.xsl"/>
    <xsl:import href="output_header.xsl"/>
    <xsl:import href="output_footer.xsl"/>
    <xsl:import href="ok_to_use.xsl"/>
    <xsl:import href="output_outlook.xsl"/>
    <xsl:import href="set_hazard_name.xsl"/>
    <xsl:import href="make_flight_level.xsl"/>
    <xsl:import href="make_conditions.xsl"/>
        
    <xsl:output method="xml"/>

    <!--  SigmetKey is used to uniquely identify sigmet references that may
          occur in more than one airmet.  Regardless of how many times a given
          sigmet is referenced we only want a single sigmet in the text report.
     -->
    <xsl:key name="SigmetKey" 
            match="smear[hazard='ICE']/ReferToSigmet"
              use="."/>

    
    <!--
        airmet_zulu.xsl
    
        This is an xslt template document.  It is to be used to process an xml 
	document that conforms to the airmet.xsd schema and produce text output 
	in the form of a Zulu Airmet report.  

	Note:  Zulu airmets cover icing, freezing levels, and multiple freezing
	level hazards.  However, at this point this template only supports
	icing hazards.
    
        The processing is split up into the default template and several named 
	templates.  If you are not familiar with xslt, these templates are 
	roughly analogous to subroutines, but have much more limited 
	functionality, due to the xslt contraints.
    
    
        Change Log
        ====== === 
        E. Safford/SAIC		03/05	initial coding
        E. Safford/SAIC         03/05   add update number to header
        E. Safford/SAIC         03/05   make 'Z' output in hdr 
        E. Safford/SAIC         07/05   remove update number from header
        E. Safford/SAIC         07/05   output to xml 
        E. Safford/SAIC    10/05    overhaul by adding imported templates to improve code reuse & maintainability 
        E. Safford/SAIC    10/05    use OutputHeader and OutputFooter
        E. Safford/SAIC    12/05    add support for M_FZLVL hazards
    -->
    
    
   <!--  
        Default template
    
        First match the root level of the document.  Process the header then
	call the named templates to process each of the smears.

        Change Log
        ====== === 
        E. Safford/SAIC        03/05    initial coding
        E. Safford/SAIC        10/05    use GetStatus, SetStatus, NoHazardReport
        E. Safford/SAIC        12/05    add outlooks
        E. Safford/SAIC        02/06    use MakeFlightLevel on Freezing Range base to convert '000' to 'SFC'
        E. Safford/SAIC        03/06    reorder, place fzlvl paragraph after outlooks
        E. Saafford/SAIC       05/06	add FZLVL & M_FZLVL to GetStatus call
    -->
    <xsl:template match="/">
        
        <xsl:variable name="faArea" select="//hdr/faArea"/>
        <xsl:variable name="issueTime" select="//hdr/issueTime"/>
        <xsl:variable name="untilTime" select="//hdr/untilTime"/>
        <xsl:variable name="outlookEndTime" select="//hdr/outlookEndTime"/>
 
        <xsl:variable name="numSmearICE" select="count(//smear[hazard='ICE'])"/>
        <xsl:variable name="numSmears" select="$numSmearICE"/>
        
        <xsl:variable name="numOutlookICE" select="count(//outlook[hazard='ICE'])"/>
        <xsl:variable name="numOutlooks" select="$numOutlookICE"/>
        
        <xsl:variable name="numFreezingRange" select="count(//freezingRange)"/>
        
        
        <xsl:variable name="amdTest">
            <xsl:call-template name="GetStatus">
                <xsl:with-param name="haz1">ICE</xsl:with-param>
                <xsl:with-param name="haz2">FZLVL</xsl:with-param>
                <xsl:with-param name="haz3">M_FZLVL</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="AMD">
            <xsl:call-template name="SetStatus">
                <xsl:with-param name="amdTest" select="$amdTest"/>
            </xsl:call-template>
        </xsl:variable>

        
        <xsl:element name="airmet">  

            <!--  output the bulletin header  -->
            <xsl:call-template name="OutputHeader">
                <xsl:with-param name="report">ZULU</xsl:with-param>
                <xsl:with-param name="hazard">ICE AND FRZLVL</xsl:with-param>
                <xsl:with-param name="untilTime" select="$untilTime"/>
                <xsl:with-param name="faArea" select="$faArea"/>
                <xsl:with-param name="issueTime" select="$issueTime"/>
                <xsl:with-param name="amend" select="$AMD"/>
            </xsl:call-template>
            
            <!--   If no ice hazards output the no hazards report -->
            <xsl:if test="not($numSmearICE > 0)">
                <xsl:call-template name="NoHazardReport">
                    <xsl:with-param name="expectedHazard">ICE</xsl:with-param>
                </xsl:call-template>
            </xsl:if>
            
            <!--   If hazards are found output them -->
            <xsl:if test="$numSmears > 0">
                <xsl:call-template name="ZuluAirmet">
                    <xsl:with-param name="faArea" select="$faArea"/>
                    <xsl:with-param name="issueTime" select="$issueTime"/>
                    <xsl:with-param name="untilTime" select="$untilTime"/>
                </xsl:call-template>
            </xsl:if>
            
            
            <!--  output any outlooks -->
            <xsl:if test="$numOutlooks > 0">
                <xsl:call-template name="ZuluOutlook">
                    <xsl:with-param name="outlookStart" select="$untilTime"/>
                    <xsl:with-param name="outlookEnd" select="$outlookEndTime"/>
                    <xsl:with-param name="numICE" select="$numOutlookICE"/>
                    <xsl:with-param name="numOutlooks" select="$numOutlooks"/>
                </xsl:call-template>
            </xsl:if>
            
            
            <!--  add freezing paragraph  -->
            <xsl:choose>
                <xsl:when test="$numFreezingRange > 0">
                    
                    <xsl:variable name="FRbase">
                        <xsl:call-template name="GetFlightLevel">
                            <xsl:with-param name="flightLevel" select="//freezingRange/Base"/>
                            <xsl:with-param name="useFL">0</xsl:with-param>
                        </xsl:call-template>
                    </xsl:variable>
                    
                    <xsl:call-template name="ZuluFreezing">
                        <xsl:with-param name="FRbase" select="$FRbase"/>
                        <xsl:with-param name="FRtop" select="//freezingRange/Top"/> 
                    </xsl:call-template>
                </xsl:when>
                <xsl:otherwise>    <!-- if no freezingRange element exists then just add a placeholder line -->
                    <xsl:element name="line">
                        <xsl:attribute name="indent">0</xsl:attribute>.</xsl:element>
                    <xsl:element name="line">
                        <xsl:attribute name="indent">0</xsl:attribute>FRZLVL...</xsl:element>
                </xsl:otherwise>
            </xsl:choose>
            
            
            <!--  add bulletin footer -->
            <xsl:call-template name="OutputFooter"/>
            
        </xsl:element>
    </xsl:template>    

   
    
    
    <!--  
        ZuluAirmet
    
        This template formats the Zulu Airmet.  Note that the formatting lines 
	begin at the left margin.  

        Change Log
        ====== === 
        E. Safford/SAIC        03/05    initial coding
        E. Safford/SAIC        08/05    Add "...UPDT" to the state list following amended and corrected hazards
        E. Safford/SAIC        10/05    rm header and footer creation
        E. Safford/SAIC        01/06    use Base and Top, not Top_Bottom
        E. Safford/SAIC        02/06    no freq/sev/conds line (line 3) for CAN airmet
        E. Safford/SAIC        04/06    use MakeConditionsStatement
        E. Safford/SAIC        05/06    add params to GetFreqSevStatement
	B. Yin/SAIC	       07/07	add tag number
        B. Yin/SAIC            02/08    removed tag number for cancellation (added into attention line)
    -->    
    <xsl:template name="ZuluAirmet">
        <xsl:param name="faArea">{faArea}</xsl:param>
        <xsl:param name="issueTime">{issueTime}</xsl:param>
        <xsl:param name="untilTime">{untilTime}</xsl:param>

        <xsl:variable name="amdTest">
            <xsl:call-template name="GetStatus">
                <xsl:with-param name="haz1">ICE</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        
        <xsl:variable name="AMD">
            <xsl:call-template name="SetStatus">
                <xsl:with-param name="amdTest" select="$amdTest"/>
            </xsl:call-template>
        </xsl:variable>
         
        <xsl:variable name="sigmetHdr">
            <xsl:call-template name="GetSigmetHdr"/>
        </xsl:variable>
 
 
        <!--
            check for sigmet references.  Output the sigmet header if 
	    any are found.
        -->
	<xsl:if test="string-length($sigmetHdr) > 1">
            <xsl:element name="line">
                <xsl:attribute name="indent">0</xsl:attribute>.</xsl:element>

            <xsl:element name="line">
                <xsl:attribute name="indent">0</xsl:attribute><xsl:value-of select="normalize-space($sigmetHdr)"/></xsl:element>
	</xsl:if> 	
		
        <!--  
	    output Zulu Airmets 
	-->
        <xsl:for-each select="//smear">
            
            <xsl:if test="hazard = 'ICE'">
                
                <xsl:variable name="status"><xsl:value-of select="Status"/></xsl:variable>
                
                <xsl:variable name="hazardName">
                    <xsl:call-template name="SetHazardName">
                          <xsl:with-param name="hazard" select="hazard"/>
                    </xsl:call-template>
                </xsl:variable>
                
                <xsl:variable name="condStatement">
                    <xsl:call-template name="MakeConditionsStatement">
                        <xsl:with-param name="isSmear">1</xsl:with-param>
                        <xsl:with-param name="fromCondsDvlpg"><xsl:value-of select="fromCondsDvlpg"/></xsl:with-param>
                        <xsl:with-param name="fromCondsEndg"><xsl:value-of select="fromCondsEndg"/></xsl:with-param>
                        <xsl:with-param name="condsContg"><xsl:value-of select="condsContg"/></xsl:with-param>
                        <xsl:with-param name="otlkCondsDvlpg"><xsl:value-of select="otlkCondsDvlpg"/></xsl:with-param>
                        <xsl:with-param name="otlkCondsEndg"><xsl:value-of select="otlkCondsEndg"/></xsl:with-param>
                    </xsl:call-template>          
                </xsl:variable>
                
                <xsl:variable name="freqSevStatement">
                	<xsl:call-template name="GetFreqSevStatement">
                		<xsl:with-param name="frequency"><xsl:value-of select="Frequency"/></xsl:with-param>
                		<xsl:with-param name="severity"><xsl:value-of select="Severity"/></xsl:with-param>
                		<xsl:with-param name="hazard" select="hazard"/>
                	           <xsl:with-param name="base" select="Base"/>
                	           <xsl:with-param name="top" select="Top"/>
                	           <xsl:with-param name="fzlBase" select="FzlBase"/>
                	           <xsl:with-param name="fzlTop" select="FzlTop"/>     
                		<xsl:with-param name="topBottom"><xsl:value-of select="Top_Bottom"/></xsl:with-param>
                		<xsl:with-param name="dueTo"><xsl:value-of select="DUE_TO"/></xsl:with-param>
                		<xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                	</xsl:call-template>
                </xsl:variable>
                
                <xsl:variable name="updateFlag">
                    <xsl:call-template name="GetUpdate">
                        <xsl:with-param name="status"><xsl:value-of select="$status"/></xsl:with-param>
                    </xsl:call-template>
                </xsl:variable>
                
                <!-- 
                        Output begins here
                -->
                <xsl:element name="line">
                    <xsl:attribute name="indent">0</xsl:attribute>.</xsl:element>
                <xsl:text>
                </xsl:text>

                <!--  Hazard statement, state list, and update flag    -->
                <xsl:element name="line">
                    <xsl:attribute name="indent">0</xsl:attribute>AIRMET <xsl:value-of select="$hazardName"/>...<xsl:value-of select="normalize-space(stateList)"/>
                    <xsl:if test="string-length($updateFlag) > 1">
                        <xsl:value-of select="normalize-space($updateFlag)"/>
                    </xsl:if>
                </xsl:element>
                
                 
                <!-- From line -->
                <xsl:element name="line">
                    <xsl:attribute name="indent">0</xsl:attribute>FROM <xsl:value-of select="normalize-space(fromLine)"/></xsl:element>

                <!-- Frequency & Severity line -->
                <xsl:variable name="airTag"><xsl:value-of select="airmetTag"/></xsl:variable>
                <xsl:if test="not( contains( Status, 'CAN' ))">
                    <xsl:if test="string-length($freqSevStatement) > 1">
                        <xsl:element name="line">
                            <xsl:attribute name="indent">0</xsl:attribute><xsl:value-of select="normalize-space($freqSevStatement)"/><xsl:if test="string-length($airTag) > 1"><xsl:text> </xsl:text><xsl:value-of select="normalize-space($airTag)"/>.</xsl:if></xsl:element>
                    </xsl:if>
                </xsl:if>

                <!--  Add the attention line(s) -->
                <xsl:call-template name="GetAttentionLine">
                    <xsl:with-param name="status"><xsl:value-of select="$status"/></xsl:with-param>
                </xsl:call-template>  
                
            </xsl:if>
        </xsl:for-each>
        
    </xsl:template>

    <!--
        ZuluFreezing
    
        This template outputs the Zulu Freezing paragraph.
        
    
        Change Log
        ====== ===
        E. Safford/SAIC    11/05        initial coding
        E. Safford/SAIC    01/06        add FZLVL contour formatting
        E. Safford/SAIC    02/06        add level 160, closed param on OutputFzlvlContour
        E. Safford/SAIC    04/06        add AMD and COR status message
        J. Wu/SAIC    	 04/06        change indent from 9 to 3 spaces
        E. Safford/SAIC    04/06        correct test on amdTest strlen
        E. Safford/SAIC    10/06        test for both 000 and SFC
        E. Safford/SAIC    12/06        if top is SFC, rmv RANGING FROM 
        B. Yin/SAIC        10/07 	change 'ABV 160' to the actual number
    -->
    <xsl:template name="ZuluFreezing">
        <xsl:param name="FRbase">MissingBase</xsl:param>
        <xsl:param name="FRtop">MissingTop</xsl:param>
        
        <xsl:element name="line">
            <xsl:attribute name="indent">0</xsl:attribute><xsl:text>.</xsl:text>
        </xsl:element>
        
        <xsl:element name="line">
            <xsl:attribute name="indent">0</xsl:attribute>
            <xsl:text>FRZLVL...</xsl:text>

            <xsl:if test="not( contains( $FRtop, 'SFC' ))">
                <xsl:text>RANGING FROM </xsl:text>
            </xsl:if>
 
            <xsl:value-of select="$FRbase"/>
           
            <xsl:if test="not( contains( $FRtop, 'SFC' ))">
                <xsl:text>-</xsl:text> 
                <xsl:value-of select="$FRtop"/>
            </xsl:if>
 
            <xsl:text> ACRS AREA</xsl:text>
        </xsl:element>
        
        <!--  output all multiple level freezing hazard lines -->
        <xsl:for-each select="//smear[hazard='M_FZLVL'] ">
            <xsl:variable name="flightLevelStatement">
                <xsl:call-template name="GetM_FZLVL_FlightLevels">
                    <xsl:with-param name="base" select="Base"/>
                    <xsl:with-param name="top" select="Top"/>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:element name="line">
                <xsl:attribute name="indent">3</xsl:attribute>
                <xsl:text>MULT FRZLVL </xsl:text><xsl:value-of select="$flightLevelStatement"/>
                <xsl:text> BOUNDED BY </xsl:text><xsl:value-of select="fromLine"/>
            </xsl:element>
            
        </xsl:for-each>
        
        <!--  output all freezing level contour lines -->
        
        <xsl:for-each select="//smear[hazard='FZLVL' and Level='SFC'] | //smear[hazard='FZLVL' and Level='000'] ">
            <xsl:call-template name="OutputFzlvlContour">
                <xsl:with-param name="level" select="Level"/>
                <xsl:with-param name="fromLn" select="fromLine"/>
                <xsl:with-param name="closed" select="closeFlg"/>
            </xsl:call-template>
        </xsl:for-each>

        <xsl:for-each select="//smear[hazard='FZLVL' and Level='040'] ">
            <xsl:call-template name="OutputFzlvlContour">
                <xsl:with-param name="level" select="Level"/>
                <xsl:with-param name="fromLn" select="fromLine"/>
                <xsl:with-param name="closed" select="closeFlg"/>
            </xsl:call-template>
        </xsl:for-each>
        
        <xsl:for-each select="//smear[hazard='FZLVL' and Level='080'] ">
            <xsl:call-template name="OutputFzlvlContour">
                <xsl:with-param name="level" select="Level"/>
                <xsl:with-param name="fromLn" select="fromLine"/>
                <xsl:with-param name="closed" select="closeFlg"/>
            </xsl:call-template>
        </xsl:for-each>
        
        <xsl:for-each select="//smear[hazard='FZLVL' and Level='120'] ">
            <xsl:call-template name="OutputFzlvlContour">
                <xsl:with-param name="level" select="Level"/>
                <xsl:with-param name="fromLn" select="fromLine"/>
                <xsl:with-param name="closed" select="closeFlg"/>
            </xsl:call-template>
        </xsl:for-each>

        <xsl:for-each select="//smear[hazard='FZLVL' and Level='160'] ">
            <xsl:call-template name="OutputFzlvlContour">
                <xsl:with-param name="level" select="Level"/>
                <xsl:with-param name="fromLn" select="fromLine"/>
                <xsl:with-param name="closed" select="closeFlg"/>
            </xsl:call-template>
        </xsl:for-each>
        
        
        <!--  Add AMD/COR line -->
        <xsl:variable name="amdTest">
            <xsl:call-template name="GetStatus">
                <xsl:with-param name="haz1">FZLVL</xsl:with-param>
                <xsl:with-param name="haz2">M_FZLVL</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
         
        <xsl:if test="string-length($amdTest) > 1">    
            <xsl:variable name="AMD">
                <xsl:call-template name="SetStatus">
                    <xsl:with-param name="amdTest" select="$amdTest"/>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:choose>
                
                <xsl:when test="contains( $amdTest, 'AMD')">
                    <xsl:element name="line">
                        <xsl:attribute name="indent">0</xsl:attribute>
                        <xsl:text>...UPDT...</xsl:text>
                    </xsl:element> 
                </xsl:when>
                
                <xsl:when test="contains( $amdTest, 'COR')">
                    <xsl:element name="line">
                        <xsl:attribute name="indent">0</xsl:attribute>
                        <xsl:text>...CORRECTED FRZLVL...</xsl:text>
                    </xsl:element>
                </xsl:when>
                
            </xsl:choose>   
        </xsl:if>
        <!--   End AMD/COR line -->
        
    </xsl:template>
    
    
    
    <!--  
        ZuluOutlook
    
        This template outputs the Zulu Outlooks.
       
        Change Log
        ====== ===
        E. Safford/SAIC    11/05        initial coding
        E. Safford/SAIC    01/06        add stateList param to OutputOutlook
        E. Safford/SAIC    02/06        add outlookStart/End to OutputOutlook
        E. Safford/SAIC    02/06        add freqSevStatement to outlooks
        E. Safford/SAIC    04/06        use MakeConditionsStatement
        E. Safford/SAIC    05/06        add freezing level base/top to outlook
    -->    
    <xsl:template name="ZuluOutlook">
        <xsl:param name="outlookStart"></xsl:param>
        <xsl:param name="outlookEnd"></xsl:param>
        <xsl:param name="numICE">0</xsl:param>
        <xsl:param name="numOutlooks">0</xsl:param>


        <xsl:for-each select="//outlook[hazard='ICE']">
            <xsl:variable name="hazardName">
                <xsl:call-template name="SetHazardName">
                    <xsl:with-param name="hazard"><xsl:value-of select="hazard"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:variable name="condStatement">
                <xsl:call-template name="MakeConditionsStatement">
                    <xsl:with-param name="isSmear">0</xsl:with-param>
                    <xsl:with-param name="fromCondsDvlpg"><xsl:value-of select="fromCondsDvlpg"/></xsl:with-param>
                    <xsl:with-param name="fromCondsEndg"><xsl:value-of select="fromCondsEndg"/></xsl:with-param>
                    <xsl:with-param name="condsContg"><xsl:value-of select="condsContg"/></xsl:with-param>
                    <xsl:with-param name="otlkCondsDvlpg"><xsl:value-of select="otlkCondsDvlpg"/></xsl:with-param>
                    <xsl:with-param name="otlkCondsEndg"><xsl:value-of select="otlkCondsEndg"/></xsl:with-param>
                </xsl:call-template>          
            </xsl:variable>
            
            <xsl:variable name="freqSevStatement">
                <xsl:call-template name="GetFreqSevStatement">
                    <xsl:with-param name="frequency"><xsl:value-of select="Frequency"/></xsl:with-param>
                    <xsl:with-param name="severity"><xsl:value-of select="Severity"/></xsl:with-param>
                    <xsl:with-param name="hazard" select="hazard"/>
                    <xsl:with-param name="base" select="Base"/>
                    <xsl:with-param name="top" select="Top"/>
                    <xsl:with-param name="fzlBase" select="FzlBase"/>
                    <xsl:with-param name="fzlTop" select="FzlTop"/>     
                    <xsl:with-param name="topBottom"><xsl:value-of select="Top_Bottom"/></xsl:with-param>
                    <xsl:with-param name="dueTo"><xsl:value-of select="DUE_TO"/></xsl:with-param>
                    <xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:call-template name="OutputOutlook">
                <xsl:with-param name="outlookStart" select="$outlookStart"/>
                <xsl:with-param name="outlookEnd" select="$outlookEnd"/>
                <xsl:with-param name="hazard" select="$hazardName"/>
                <xsl:with-param name="stateList" select="stateList"/>
                <xsl:with-param name="outlookNumber" select="position()"/>
                <xsl:with-param name="totalOutlooks" select="$numOutlooks"/>
                <xsl:with-param name="boundedBy" select="fromLine"/>
                <xsl:with-param name="freqSevStatement" select="$freqSevStatement"/>                
            </xsl:call-template>
            
        </xsl:for-each>
        
    </xsl:template>    
     

    
    <!--
        GetNewStatement
        
        This template returns the "new" statement.  It will either output the 
        "...NEW AIRMET..." phrase or a single "."

        Change Log
        ====== === 
        E. Safford/SAIC		03/05	initial coding
    -->
    <xsl:template name="GetNewStatement">
        <xsl:param name="status"/>
        <xsl:if test="contains( $status, 'NEW')">
            ...NEW AIRMET...
        </xsl:if>
    </xsl:template>
    
    
    <!--
    	GetFreqSevStatement
    		
    	This tepmplate returns the frequency/severity/flight level/due 
	to/conditions statement.  It will return these in the correct order, 
	provided they exist in the smear.  If they do not, then a null string 
	will be returned.

        Change Log
        ====== === 
        E. Safford/SAIC            03/05	initial coding
        E. Safford/SAIC            09/05    call MakeFRZLVL template
        E. Safford/SAIC            10/05    add strlen check, normalize space to freq.
        E. Safford/SAIC            11/05    limit severity to only ICE hazards
        E. Safford/SAIC            11/05    remove "BTN" before TopBottom string
        E. Safford/SAIC            01/06    use MakeFlightLevel
        E. Safford/SAIC            04/06    add "." before conditions statement.
        E. Safford/SAIC            05/06    add fzlBase and fzlTop params       
        E. Safford/SAIC            05/06    rearrange "." placement to avoid dup
        E. Safford/SAIC            06/06    rm extra period after conditions statement.
    -->
    <xsl:template name="GetFreqSevStatement">
    	<xsl:param name="frequency"/>
    	<xsl:param name="severity"/>
    	<xsl:param name="hazard"/>
           <xsl:param name="base"/>
           <xsl:param name="top"/>
        <xsl:param name="fzlBase"/>
        <xsl:param name="fzlTop"/>
    	<xsl:param name="dueTo"/>
    	<xsl:param name="conditions"/>
        
           <xsl:variable name="flightLevelStatement">
               <xsl:call-template name="MakeFlightLevel">
                   <xsl:with-param name="base" select="$base"/>
                   <xsl:with-param name="top" select="$top"/>
               </xsl:call-template>
           </xsl:variable>


    	<xsl:if test="string-length($frequency) >1 or
    		          string-length($severity) > 1 or
    		          string-length($flightLevelStatement) > 1 or
    		          string-length($dueTo) > 1 or
    		          string-length($conditions) > 1" >
 
            <xsl:if test="string-length($frequency) >1">
                <xsl:variable name="validFrequency">
                    <xsl:call-template name="OkToUse">
                        <xsl:with-param name="test" select="$frequency"/>
                    </xsl:call-template>
                </xsl:variable>
    	       
                <xsl:if test="string-length($validFrequency) > 1">
                    <xsl:value-of select="normalize-space($validFrequency)"/>
                </xsl:if>
            </xsl:if>
  	    
            <xsl:if test="string-length($severity)>1 and contains($hazard, 'ICE')">
                <xsl:text> </xsl:text>
                <xsl:value-of select="$severity"/>
            </xsl:if>

            <xsl:if test="string-length($dueTo)>1" >
                <xsl:text> </xsl:text>
                <xsl:value-of select="$dueTo"/>
            </xsl:if>

            <xsl:if test="string-length($flightLevelStatement)>1" >
                <xsl:text> </xsl:text>
                <xsl:value-of select="$flightLevelStatement"/>
            </xsl:if>

    	 <xsl:if test="string-length($fzlBase) > 1" >
    	      <xsl:text>. FRZLVL </xsl:text>
    	      <xsl:value-of select="$fzlBase"/>
    	      <xsl:text>-</xsl:text>
    	      <xsl:value-of select="$fzlTop"/>
    	 </xsl:if>

    	 <xsl:if test="string-length($frequency) >1 or
    	        string-length($severity) > 1 or
    	        string-length($flightLevelStatement) > 1 or
    	        string-length($dueTo) > 1" >
    	     <xsl:text>. </xsl:text>       
    	 </xsl:if>
    	    
            <xsl:if test="string-length($conditions)>1" >
                <xsl:value-of select="$conditions"/>             
            </xsl:if>
            

        </xsl:if>

    </xsl:template>


    <!--
   	GetSigmetHdr
   			
   	This template builds and returns the sigmets header statement that 
	immediately follows the airmet header.  Note that we need to check 
	the hazard here so we don't pick up any smears of types that don't 
	conform to Zulu class airmets.  (ICE is the only type within
   	Tango that can reference a sigmet.)

        Change Log
        ====== === 
        E. Safford/SAIC		03/05	initial coding
    -->
    <xsl:template name="GetSigmetHdr">
  	<xsl:variable name="testSigmet">
  	    <xsl:call-template name="ListSigmetRefs"/>
   	</xsl:variable> 

   	<xsl:if test="string-length($testSigmet) > 1"> 
            ...SEE SIGMET <xsl:value-of select="$testSigmet"/>SERIES FOR POSS SEV ICE
        </xsl:if>
   </xsl:template>
    
    
    <!--
    	ListSigmetRefs
    		
    	This template returns any sigmets found in any smears.  The context 
	here is the smear which is selected in the calling template.  All 
	RefereToSigmet elements are selected and their values sent to output 
	by the value-of select"." statement (which selects the string 
	associated with the ReferToSigmet element).  The if test adds the AND 
	after the next to last sigmet reference if there is more than 1 
	reference.

        Change Log
        ====== === 
        E. Safford/SAIC		03/05	initial coding
        E. Safford/SAIC		03/05	use SigmetKey to ensure each referenced
                                          sigmet is listed only once
    -->
    <xsl:template name="ListSigmetRefs">
    	<xsl:for-each select="//smear[hazard='ICE']/ReferToSigmet[generate-id(.) = generate-id( key('SigmetKey', .)[1])]">
            <xsl:sort order="ascending"/>

            <!--
                output the ReferToSigmet and add on "AND " if 
                this is the next to last node to be processed.
             -->
 	    <xsl:value-of select="."/><xsl:text> </xsl:text>
 	        <xsl:if test="position() + 1 = last() and last() &gt; 1">AND </xsl:if>
 	</xsl:for-each>
    </xsl:template>
    

    <!-- 
            GetM_FZLVL_FlightLevels
    
            This template returns the correctly formatted and abreviated flight level information
            for Multiple Freezing level hazards.
    
            Change Log
            ====== ===
            E. Safford/SAIC            12/05 initial coding  
            E. Safford/SAIC            02/06 use FLbase to convert '000' to 'SFC'
            B. Yin/SAIC                10/07 change 'ABV 160' to the actual number
    -->
    <xsl:template name="GetM_FZLVL_FlightLevels">
        <xsl:param name="base">Missing Base</xsl:param>
        <xsl:param name="top">Missing Top</xsl:param>
        
        <xsl:variable name="FLbase">
            <xsl:call-template name="GetFlightLevel">
                <xsl:with-param name="flightLevel" select="$base"/>
                <xsl:with-param name="useFL">0</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        
        <xsl:choose>
            <xsl:when test="contains( $FLbase, 'SFC')">
                <xsl:text>BLW </xsl:text><xsl:value-of select="$top"/>
            </xsl:when>
            
            <xsl:otherwise>
                <xsl:value-of select="$base"/><xsl:text>-</xsl:text>
                <xsl:value-of select="$top"/>
            </xsl:otherwise>
            
        </xsl:choose>
        
    </xsl:template>
    
   
    <!--  
        OutputFzlvlContour
        
        This template outputs a freezing level contour
        
        Change Log
        ====== ===
        E. Safford/SAIC    01/06        initial coding
        E. Safford/SAIC    02/06        use BOUNDED BY if closed is true (1)
        J. Wu/SAIC    	 04/06        change indent from 9 to 3 spaces
        E. Safford/SAIC    10/06        Treat 000 as SFC
    -->    
    <xsl:template name="OutputFzlvlContour">
        <xsl:param name="level">missing level</xsl:param>
        <xsl:param name="fromLn">missing From line</xsl:param>
        <xsl:param name="closed">missing close value</xsl:param>
        <xsl:element name="line">        
            <xsl:attribute name="indent">3</xsl:attribute>
            
            <xsl:choose>
                <xsl:when test="contains($level, '000')">
                    <xsl:text>SFC</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="normalize-space($level)"/>
                </xsl:otherwise>
            </xsl:choose>
            
            <xsl:choose>
                <xsl:when test="number( $closed ) = number( '1' )">
                    <xsl:text> BOUNDED BY </xsl:text>
                </xsl:when>
                <xsl:otherwise> ALG </xsl:otherwise>
            </xsl:choose>
           
            <xsl:value-of select="normalize-space($fromLn)"/>
        </xsl:element>
    </xsl:template>
   
</xsl:stylesheet>
