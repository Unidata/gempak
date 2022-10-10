

var radarpathsObject;
var radarTimes = new Array();
var radarTimesJulian = new Array();
var radarVCP = new Array();
var radarSTPStart = new Array();
var radarSRMDirection = new Array();
var radarSRMSpeed = new Array();
var radarOpacity = 0.75;
var rid, pid;
var radarLayers = new Array();
var activeLayer = 0;
var numberOfFramesInAnimation = 1;
var markers = new Array();
var markerLatlng;
var markercount=0;
var animateActive = 0;
var aniTimeout, refreshTimeout;
var defaultLoopDelay = 300;
var defaultLoopDwell = 600;
var refreshInterval = 300;
var showingOnMaporNot;


function Ridge2radar(hererid,herepid){
	// CONSTRUCTOR
	rid = hererid;
	pid = herepid;
	  this.updateTimes(rid,pid);

		$( "#dialog-modal" ).dialog({
			height: 300,
			width: 700,
			modal: true,
			autoOpen: false
			
		});  
		
		this.countDownRefresh(refreshInterval, 1000);

		showingOnMaporNot = 1;
}

	Ridge2radar.prototype.updateRadarLegend = function(rid,pid){
		if(pid == "N0Q"){
			$( "#legendRadarDiv" ).html('<img src="images/Legend/N0R.gif"/>');
		}
		else{
			$( "#legendRadarDiv" ).html('<img src="images/Legend/'+pid+'.gif"/>');
		}
	}

	Ridge2radar.prototype.updateTimes = function(rid,pid){
		radarTimesJulian = [];
		radarVCP = [];
		radarSTPStart = [];
		radarSRMSpeed = [];
		radarSRMDirection = [];

		//console.log('updating times: '+rid);
		this.updateRadarLegend(rid,pid);
		
		this.populateRadarDescription(rid,pid);
			// http://mybrowncouch.com/codenoobs/examples/ridgenew2/proxy/radar_times_proxy.php?rid=NAT&pid=N0Q
	 		jqxhr = $.getJSON(radar_times_path+'?rid='+rid+'&pid='+pid,function(json){});
	
			
			
			jqxhr.success(function(json){	
		 			$.each(json.radarinfo, function(i,layerinfo){
			 				pattern = "[0-9]{12}";
			 				//console.log("Found: "+layerinfo.path.match(pattern));
			 				radarTimes.push(layerinfo.path.match(pattern));
			 				if(layerinfo.vcp){
			 					radarVCP.push(' - VCP'+layerinfo.vcp);
			 				}
			 				else{
				 				radarVCP.push('');
			 				}
			 				if(layerinfo.stpstart){
			 					radarSTPStart.push(layerinfo.stpstart);
			 				}
			 				else{
			 					radarSTPStart.push('');
			 				}
			 				if(layerinfo.srmspeed){
			 					radarSRMDirection.push(layerinfo.srmdirection);
			 					radarSRMSpeed.push(layerinfo.srmspeed);
			 				}
			 				else{
			 					radarSRMDirection.push('');
			 					radarSRMSpeed.push('');
			 				}
			 				
			 				
			 				//console.log('here: '+layerinfo.time);
			 				//radarTimes.push(layerinfo.time);
			 				radarTimesJulian.push(layerinfo.time);
		 			});	
		 			
		 			  for(i=0; i<numberOfFramesInAnimation; i++){
		 				 Ridge2radar.prototype.addLayer(i, rid, pid);
		 			  }
	 			  	if(radarSTPStart[activeLayer]){
				  		$('#radarTimeDisplay').html(Ridge2radar.prototype.formatDateForDisplay(radarSTPStart[activeLayer])+' - '+Ridge2radar.prototype.formatDateForDisplay(radarTimesJulian[activeLayer])+' '+radarVCP[activeLayer]);
				  	}
				 	else if(radarSRMDirection[activeLayer] != "1.4013e-45" && radarSRMDirection[activeLayer] != ""){
				 			$('#radarTimeDisplay').html(Ridge2radar.prototype.formatDateForDisplay(radarTimesJulian[activeLayer])+' - Storm Motion: '+radarSRMDirection[activeLayer]+'&deg; at '+radarSRMSpeed[activeLayer]+'kts '+radarVCP[activeLayer]);
					}
					else{
					  		$('#radarTimeDisplay').html(''+Ridge2radar.prototype.formatDateForDisplay(radarTimesJulian[activeLayer])+' '+radarVCP[activeLayer]);
		 			 }
			});
	
	 		jqxhr.complete(function(json){
				//alert ('Complete.');			
			 });
			
			
			jqxhr.error(function(){
				//alert ('ERROR!');
			});
		
	}

	Ridge2radar.prototype.formatDateForDisplay = function (time){
		
		var myDate = new Date(time*1000);
	    var ptz = myDate.getTimezoneOffset()/60;
	    //var pdate = myDate.toLocaleString() + " UTC-" + ptz;
	    var pdate = myDate.toLocaleString();
		//document.getElementById('timestamp').innerHTML = pdate;
	    
	    
		pattern = "GMT[\+\-][0-9]{4}";
		matchhere = pdate.match(pattern);
		pdate = pdate.replace(matchhere, "");
		pdate = pdate.replace("(", "");
		pdate = pdate.replace(")", "");
	    
		return pdate;
	}
	
	Ridge2radar.prototype.addLayer = function(num, rid, pid)  
	{  
	 	radarLayers[num] = new RadarMapType(new google.maps.Size(256, 256), radarTimes[num], num, rid, pid);
		map.overlayMapTypes.insertAt(num, radarLayers[num]);
	}; 
	
	
	Ridge2radar.prototype.showLayer = function(num){
		
	if(radarSTPStart[activeLayer]){
	  		$('#radarTimeDisplay').html(Ridge2radar.prototype.formatDateForDisplay(radarSTPStart[activeLayer])+' - '+Ridge2radar.prototype.formatDateForDisplay(radarTimesJulian[activeLayer])+' '+radarVCP[activeLayer]);
	}
	else if(radarSRMDirection[activeLayer] != "1.4013e-45" && radarSRMDirection[activeLayer] != ""){
 			$('#radarTimeDisplay').html(Ridge2radar.prototype.formatDateForDisplay(radarTimesJulian[activeLayer])+' - Storm Motion: '+radarSRMDirection[activeLayer]+'&deg; at '+radarSRMSpeed[activeLayer]+'kts '+radarVCP[activeLayer]);
	}
	else{
	  		$('#radarTimeDisplay').html(''+Ridge2radar.prototype.formatDateForDisplay(radarTimesJulian[activeLayer])+' '+radarVCP[activeLayer]);
		 }
		
		//$('#radarTimeDisplay').html(''+Ridge2radar.prototype.formatDateForDisplay(radarTimesJulian[num])+' '+radarVCP[activeLayer]);
		$('div#radarDiv'+radarTimes[num]).css('opacity',radarOpacity);
		$('div.radarDiv'+radarTimes[num]).css('opacity',radarOpacity);
		
		activeLayer = num;
	
	};
	
	Ridge2radar.prototype.hideLayer = function(num){
		$('div#radarDiv'+radarTimes[num]).css('opacity','0.0');
		$('div.radarDiv'+radarTimes[num]).css('opacity','0.0');	
	};
	
	Ridge2radar.prototype.animate = function(){
		//console.log(numberOfFramesInAnimation);
		//if(numberOfFramesInAnimation != 1){
			animateActive = 1;
			$('div#startAniDiv').css('visibility','hidden');
			$('div#stopAniDiv').css('visibility','visible');
			Ridge2radar.prototype.hideLayer(activeLayer);
			activeLayer--;
			if(activeLayer < 0){
				activeLayer = numberOfFramesInAnimation-1;
			}
			Ridge2radar.prototype.showLayer(activeLayer);
			//console.log(activeLayer);
			if(activeLayer != 0){
				aniTimeout = setTimeout(Ridge2radar.prototype.animate,defaultLoopDelay);
			}
			else{
				aniTimeout = setTimeout(Ridge2radar.prototype.animate,defaultLoopDwell);
			}
		//}
		
	};
	
	Ridge2radar.prototype.stopAnimate = function(){
		animateActive = 0;
		$('div#startAniDiv').css('visibility','visible');
		$('div#stopAniDiv').css('visibility','hidden');
		clearTimeout(aniTimeout);
		
		for(i=0;i<numberOfFramesInAnimation;i++){
			this.hideLayer(i);
		}
		this.showLayer(0);
		
		
	};
	
	Ridge2radar.prototype.removeAllRadarLayers = function(){
		
		for(i=0; i<20; i++){
			map.overlayMapTypes.setAt(i, null);
		}
		
	};
	
	Ridge2radar.prototype.changeRadar = function(newrid,newpid){
		
		for(i=0;i<numberOfFramesInAnimation;i++){
			this.hideLayer(i);
		}
		this.showLayer(0);
		activeLayer = 0;
		
		
		
		clearTimeout(refreshTimeout);
		rid = newrid;
		pid = newpid;
		$( "#dialog-modal" ).dialog( "close" );
		//console.log("Change radar: "+ newrid + ", "+ newpid)
		//this.stopAnimate();
		this.removeAllRadarLayers();
		radarTimes.length = 0;
		this.updateTimes(newrid,newpid);
		this.removeallmarkers();
		
		this.countDownRefresh(refreshInterval, 1000);
		
		
	};
	
	Ridge2radar.prototype.markerClickStuff = function(idhere){
		$('#radarPopUpInfo').html('Loading...');
		
 		jqxhr = $.getJSON(radar_info_json_path+'?id='+idhere,function(json){});
 		
		jqxhr.success(function(json){	
			var popUpHtml = '<span style="font-size:150%;">'+json.radardetails.name + '('+json.radardetails.rid+')</span><br/>'+json.radardetails.type+'<br/>'; 
			pupUpHtml = '<ul>';
			for(i=0; i<json.radarproducts.length; i++){
				popUpHtml += '<li><span  style="font-size:75%;"><a href="javascript:void(0)" onclick="radar.changeRadar(\''+json.radardetails.rid+'\',\''+json.radarproducts[i].pid+'\');">'+json.radarproducts[i].name+'</a></span></li>';
			}
			pupUpHtml = '</ul>';
			
			$('#radarPopUpInfo').html(popUpHtml);
			$( "#dialog-modal" ).dialog( "open" );

		});

 		jqxhr.complete(function(json){		
		 });
		
		
		jqxhr.error(function(){
			$('#radarPopUpInfo').html('Error Loading...please click marker again...');
		});
 	};

	Ridge2radar.prototype.populateRadarDescription = function(hererid,herepid){
		
		//console.log("description: "+hererid+", "+herepid);
		
		$('#radarInfoAboveMap').html('Loading...');
		
 		jqxhr = $.getJSON(radar_info_json_path+'?id='+hererid,function(json){});
 		
		jqxhr.success(function(json){
			
		      //initialLocation = new google.maps.LatLng(json.radardetails.lat,json.radardetails.lon);
		      //map.setCenter(initialLocation);
		      //map.setZoom(8);
			
			//console.log('click JSON success: ' + json.radardetails.name);
			
			var popUpHtml = '<span style="font-size:175%;">'+json.radardetails.name + ' - '+json.radardetails.rid+'</span><span style="font-size:100%;">&nbsp;&nbsp;<button id="selectNewRadarButton" onclick="radar.plotAllRadars();">Select New Radar</button><button onclick="goToPermalink()">Permalink</button><span id="radarSelectLegend" style="display:none;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;NWS Radar: <img height="12px" src="images/88d.png"/> &nbsp;&nbsp;&nbsp;&nbsp; Non-NWS Radar: <img height="12px" src="images/other.png"/></span><br/><b>Radar Type:</b> '+json.radardetails.type+''; 
			
			for(i=0; i<json.radarproducts.length; i++){
				if(json.radarproducts[i].pid == herepid){
					if(json.radardetails.rid == "NAT"){
						popUpHtml += ' <br/><b>Product Displayed:</b> '+json.radarproducts[i].name+' - '+herepid+'&nbsp;&nbsp;<button onclick="radar.markerClickStuff(\''+json.radardetails.rid+'\');">Change Radar Product</button></span>';
						
					}
					else{
						popUpHtml += ' <br/><b>Product Displayed:</b> '+json.radarproducts[i].name+' - '+herepid+'&nbsp;&nbsp;<button onclick="radar.markerClickStuff(\''+json.radardetails.rid+'\');">Change Radar Product</button></span>  <a href="../kml/singleRadarLoop_network.php?rid='+json.radardetails.rid+'&pid='+herepid+'"><img height="15px" src="images/kmllogo.png"/></a>';
						
					}
				}
				
			}
			
			$('#radarInfoAboveMap').html(popUpHtml);
			//console.log(popUpHtml);

		});

 		jqxhr.complete(function(json){
 			//console.log('Click JSON Complete.');			
		 });
		
		
		jqxhr.error(function(){

			$('#radarInfoAboveMap').html('Error Loading...please click marker again...');
		});
		
	};

	
	Ridge2radar.prototype.plotAllRadars = function(){
		//  http://www.srh.noaa.gov/ridge2/ajax/radars.php
	 		markercount = 0;
	 		$('div#mosaicSelect').css('display','block');
	 		$('span#radarSelectLegend').css('display','inline');
	 		$('button#selectNewRadarButton').css('display','none');
	 		jqxhr = $.getJSON(radars_path,function(json){
	 			$.each(json.features, function(i,radarpoint){
	
	 			    markerLatlng = new google.maps.LatLng(radarpoint.geometry.coordinates[1],radarpoint.geometry.coordinates[0]);
	
	 			    //console.log('Yo: '+radarpoint.properties.radar_type);
	 			   if(radarpoint.properties.radar_type == "WSR-88D"){
	 				  iconHere = 'images/88d.png';
	 			   }
	 			   else{
	 				   iconHere = 'images/other.png'; 
	 			   }
	 			    markers[i] = new google.maps.Marker({
	 			    	position: markerLatlng, 
	 			    	map: map, 
	 			    	title:radarpoint.properties.name+" ("+radarpoint.properties.id+') - '+radarpoint.properties.radar_type,
	 			    	icon: iconHere
	 			   }); 
	 			   google.maps.event.addListener(markers[i], 'click', function(){radar.markerClickStuff(radarpoint.properties.id);});
	 			  markercount++;
	 			});
	 		});
	
	
	 		jqxhr.complete(function(){			
			 });
			
			
			jqxhr.error(function(){
			});
			
			
			jqxhr.success(function(){	
			});

	};

	Ridge2radar.prototype.removeallmarkers = function(){
 		$('div#mosaicSelect').css('display','none');
 		$('span#radarSelectLegend').css('display','none');
 		$('button#selectNewRadarButton').css('display','inline');
 		for (i=0;i<markercount;i++){
 			markers[i].setMap(null);
 		}
 	};
 	
 	
 	
 	
 	
 	
	Ridge2radar.prototype.countDownRefresh = function(i, p) {
			// 		store parameters
			 	var pause = p;
			// 		make reference to div
			 	var countDownObj = document.getElementById("countDown");
			 	countDownObj.count = function(i) {

					 	countDownObj.innerHTML = i;
					 	if (i == 0) {
					 		//console.log("rid: "+rid);
						 	clearTimeout(refreshTimeout);
						 	
						 	Ridge2radar.prototype.changeRadar(rid,pid);
						 	wwaPoly.refresh();
						 	wwaLayerOverlay.refresh();
						 	
						 	return;
					 	}
					 	
					 	refreshTimeout = setTimeout(function() {
					 		countDownObj.count(i - 1);
					 	},
					 		pause
					 	);
			 	}
			// 		set it going
			 	countDownObj.count(i);
	}
	
	
	
 	
 	
	Ridge2radar.prototype.toggle = function(i, p) {
		
		if(showingOnMaporNot == 1){
			this.removeAllRadarLayers();
			showingOnMaporNot = 0;
		}
		else{
			this.changeRadar(rid,pid);
			showingOnMaporNot = 1;
		}

	}