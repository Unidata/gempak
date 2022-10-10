 /* MYBROWNCOUCH.com */
/*
var tileCacheURL = "http://radarcache2.srh.noaa.gov/tc/tc.py/1.0.0/";
var tileCacheNoPurgeURL = "http://radarcache2.srh.noaa.gov/c/tc.py/1.0.0/";
var radar_times_path = "http://mybrowncouch.com/codenoobs/examples/ridgenew2/proxy/radar_paths_proxy.php";
var radar_info_json_path = "http://mybrowncouch.com/codenoobs/examples/ridgenew2/proxy/radar_info_json_proxy.php";
var radars_path = "http://mybrowncouch.com/codenoobs/examples/ridgenew2/proxy/radars_proxy.php";
var getproducts_path = "http://mybrowncouch.com/codenoobs/examples/ridgenew2/proxy/getProductsForPoint_proxy.php";
*/


/* RADAR 1 (NIDS DEV) */
/*
var tileCacheURL = "http://radar1.dev.nids.noaa.gov/tilecache/tc.py/1.0.0";
var tileCacheNoPurgeURL = "http://radar1.dev.nids.noaa.gov/tilecache/tc.py/1.0.0";
var radar_times_path = "http://radar1.dev.nids.noaa.gov/ridge2/ajax/radar_paths.php";
var radar_info_json_path = "http://radar1.dev.nids.noaa.gov/ridge2/ajax/radar_info_json.php";
var radars_path = "http://radar1.dev.nids.noaa.gov/ridge2/ajax/radars.php";
var getproducts_path = "http://radar1.dev.nids.noaa.gov/ridge2/ajax/getProductsForPoint.php";
*/

/* SR RIDGE 2 */
/* */
var tileCacheURL = "http://radarcache.srh.noaa.gov/tc/tc.py/1.0.0/";
var tileCacheNoPurgeURL = "http://radarcache2.srh.noaa.gov/c/tc.py/1.0.0/";
var radar_times_path = "http://www.srh.noaa.gov/ridge2/ajax/radar_paths.php";
var radar_info_json_path = "http://www.srh.noaa.gov/ridge2/ajax/radar_info_json.php";
var radars_path = "http://www.srh.noaa.gov/ridge2/ajax/radars.php";
var getproducts_path = "http://www.srh.noaa.gov/ridge2/ajax/getProductsForPoint.php";






var latInit = 38.86537;
var lonInit = -98.04749;
if(gup('zoom')){
	var zInit = currentZoom  = newZoom = parseInt(gup('zoom'));
	//zInit = currentZoom  = newZoom = pid.toUpperCase();
}
else{
	var zInit = currentZoom  = newZoom = 4;
}	
if(gup('lat') && gup('lon')){
	var latInit = parseInt(gup('lat'));
	var lonInit = parseInt(gup('lon'));
}
else{
	//var zInit = currentZoom  = newZoom = 4;
}

var map;
var radar;
var rid, pid;
var menuForEverything = 1;
var fullScreen = growNoMatterWhat = 0;
var infowindow;
var event;


var kmlLayerObjects = new Array();
var kmlNamesForMap = new Array("lsr", "hpcd1", "hpcd2", "hpcd3","hpcs4d1","hpcs4d2","hpcs4d3","spclsrtoday","spclsryesterday");
var kmlURLsForMap = new Array(	"http://wdssii.nssl.noaa.gov/realtime/warnings/LSRs/NWS_LSRs.kml", 
								"http://www.hpc.ncep.noaa.gov/kml/qpf/QPF24hr_Day1_latest_netlink.kml", 
								"http://www.hpc.ncep.noaa.gov/kml/qpf/QPF24hr_Day2_latest_netlink.kml", 
								"http://www.hpc.ncep.noaa.gov/kml/qpf/QPF24hr_Day3_latest_netlink.kml",
								"http://www.hpc.ncep.noaa.gov/kml/winwx/Day1_psnow_gt_04inches.kml",
								"http://www.hpc.ncep.noaa.gov/kml/winwx/Day2_psnow_gt_04inches.kml",
								"http://www.hpc.ncep.noaa.gov/kml/winwx/Day3_psnow_gt_04inches.kml",
								"http://www.spc.noaa.gov/climo/reports/today.kmz",
								"http://www.spc.noaa.gov/climo/reports/yesterday.kmz");
var kmlOnOrOffForMap = new Array(false, false, false, false,false,false,false,false,false);
function addOrRemoveKMLLayer(layerName){
	
	var kmlIndex = $.inArray(layerName,kmlNamesForMap);
	
	if(kmlIndex != -1){
		
		if(kmlOnOrOffForMap[kmlIndex]){
			kmlLayerObjects[kmlIndex].setMap(null);
			kmlOnOrOffForMap[kmlIndex] = false;
		}
		else{
			kmlLayerObjects[kmlIndex] = new google.maps.KmlLayer(kmlURLsForMap[kmlIndex], { preserveViewport: true });
			kmlLayerObjects[kmlIndex].setMap(map);
			kmlOnOrOffForMap[kmlIndex] = true;
		}
		
	}
	
	else{
		//console.log("layer name does not exist");
	}
}



function initialize() {
	$('#menuToggleButton').hide();
	

	if(gup('rid')){
		rid = gup('rid');
		rid = rid.toUpperCase();
		
		if(gup('lat') && gup('lon')){
			//centerOnRadar(rid);
		}
		else{
			centerOnRadar(rid);
		}
		
		
	}
	else{
		rid = "NAT";
	}

	if(gup('pid')){
		pid = gup('pid');
		pid = pid.toUpperCase();
	}
	else{
		pid = "N0Q";
	}	
	
	
	
	
	  
    var myLatLng = new google.maps.LatLng(latInit,lonInit);
    
 
    
    var myOptions = {
      zoom: zInit,
      center: myLatLng,
      mapTypeId: google.maps.MapTypeId.TERRAIN,
      mapTypeControlOptions: {
    	
          mapTypeIds: [ 'light_layer','dark_layer',google.maps.MapTypeId.TERRAIN,google.maps.MapTypeId.HYBRID],
          style: google.maps.MapTypeControlStyle.HORIZONTAL_BAR,
          position: google.maps.ControlPosition.TOP_RIGHT
	      },
	      panControl: true,
	      panControlOptions: {
	          position: google.maps.ControlPosition.RIGHT_TOP
	      },
	      zoomControl: true,
	      zoomControlOptions: {
	          style: google.maps.ZoomControlStyle.LARGE,
	          position: google.maps.ControlPosition.RIGHT_CENTER
	      },
	      streetViewControl: false,
	      streetViewControlOptions: {
	          position: google.maps.ControlPosition.LEFT_TOP
	      }

    };
    
 
    map = new google.maps.Map(document.getElementById("map_canvas"),myOptions);
	

    map.mapTypes.set('dark_layer', darkMapType);
    map.setMapTypeId('dark_layer');
    
    
    map.mapTypes.set('light_layer', lightMapType);
    map.setMapTypeId('light_layer');
    
	google.maps.event.addListener(map, 'click', function(event) {
		//console.log("Tiles loaded");
		wwaPopUp(event.latLng.lat(), event.latLng.lng(), event);
	});
	
	google.maps.event.addListener(map, 'zoom_changed', function() {
		
		// stop animating upon zoom when not in full screen mode
		//if(fullScreen != 1){
			if(!$('input:radio[name=frames]')[0].checked){
				//console.log($('input:radio[name=frames]')[0].checked);
				numberOfFramesInAnimation = 1;
				radar.changeRadar(rid,pid);
				$('input:radio[name=frames]')[0].checked = true;
				$('input:radio[name=frames]')[1].checked = false;
				$('input:radio[name=frames]')[2].checked = false;
				$('input:radio[name=frames]')[3].checked = false;
			}
		//}
		// stop animating upon zoom.
		/*
		newZoom = map.getZoom();
		if(newZoom%2 != 0 && currentZoom < newZoom ){
			//console.log(currentZoom + ' ' + newZoom);
			currentZoom = newZoom + 1;
			
			if(currentZoom < 4){
				map.setZoom(4);
			}
			else{
				map.setZoom(currentZoom);
			}
		}
		if(newZoom%2 != 0 && currentZoom > newZoom ){
			//console.log(currentZoom + ' ' + newZoom);
			currentZoom = newZoom - 1;
			map.setZoom(currentZoom);
			
			if(currentZoom < 4){
				map.setZoom(4);
			}
			else{
				map.setZoom(currentZoom);
			}
		}
		
		*/
	});
	
	
	
	
	
	
    radar = new Ridge2radar(rid,pid);
    radar.animate();
    
    wwaPoly = new polyWarnings();
    wwaPoly.readPolyJSON();
 	
 	
 	googleroadOverlay = new GoogleroadsMapType(new google.maps.Size(256, 256));
	map.overlayMapTypes.setAt(29, googleroadOverlay);
	googleroadOverlay.toggle();
	
 	stateOverlay = new stateMapType(new google.maps.Size(256, 256));
	map.overlayMapTypes.setAt(30, stateOverlay);	
 	//stateOverlay.toggle();	
	
	
 	
 	countyOverlay = new countyMapType(new google.maps.Size(256, 256));
	map.overlayMapTypes.setAt(28, countyOverlay);	
 	//countyOverlay.toggle();
	

 	
 	wwaLayerOverlay = new wwaMapType(new google.maps.Size(256, 256));
 	wwaLayerOverlay.addItToMap();
 	wwaLayerOverlay.toggle();
 	//wwaLayerOverlay.toggle();
	$( "#wwadialog-modal" ).dialog({
		height: 400,
		width: 900,
		modal: true,
		autoOpen: false
		
	});  
	

	//var spcMesoscaleDicsussionsLayer = new google.maps.KmlLayer('http://www.spc.noaa.gov/climo/reports/110423_rpts.kmz',{preserveViewport: true});
	//spcMesoscaleDicsussionsLayer.setMap(map);
	
	//addOrRemoveKMLLayer("lsr");
	


	if(gup('fs')=='1'){
		//console.log('yup.');
		goFullScreenRadar();
		shrinkMenuForfFullscreen();
	}

  }


// some full-screen help...
$(window).resize(function() {
	  //console.log('window resized.');
	  google.maps.event.trigger(map, 'resize');
	});

$("#address").keypress(function(e) {
	//console.log(e.which);
    if (e.which == 13){
    	codeAddress();
    	
    }
});


	
function wwaPopUp(lat, lon, event){

			jqxhr = $.getJSON(getproducts_path+'?lat='+lat+'&lon='+lon+'&rand='+Math.random()*10000,function(json){
	 			//console.log('start');
				$('#wwaPopUpInfo').html("Loading...");
 				//$( "#wwadialog-modal" ).dialog( "open" );
	 		});

			jqxhr.success(function(json){	

				$('#wwaPopUpInfo').html("");
				popuphtml = "";
				i = 0;
	 			$.each(json.features, function(i,poly){

	 				var myDate = new Date( poly.properties.end *1000);
	 				var myDateReadable = (myDate.toLocaleString());
	 				
	 				popuphtml += poly.properties.phenomenon_string + ' ' + poly.properties.significance_string+'<br/>';
	 				
	 				//$('#wwaPopUpInfo').append(popuphtml);
	 				if(poly.properties.end){
	 					popuphtml += '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="font-size:75%;font-color:#777777;">Valid Until: '+myDateReadable+'</span><br/>';
	 					//$('#wwaPopUpInfo').append(popuphtml);
	 				}
	 				else{
	 					popuphtml += '<br/>';
	 				}

	 			});	
	 			
	 			if(json.features.length < 1){
					popuphtml = "<i>No active Watches, Warnings or Advisories.</i>";
	 			}
	 				infowindow = new google.maps.InfoWindow();
	 				infowindow.setContent(popuphtml);
	 				infowindow.setPosition(event.latLng);
	 				infowindow.open(map);
			});
	
	 		jqxhr.complete(function(json){
			
			 });
			
			
			jqxhr.error(function(){
	
			});
	
}


function codeAddress() {

    var geocoder = new google.maps.Geocoder();
    var address = document.getElementById("address").value;
    geocoder.geocode( { 'address': address}, function(results, status) {
      if (status == google.maps.GeocoderStatus.OK) {
        map.setCenter(results[0].geometry.location);
        map.setZoom(9);
      } else {
        alert("Geocode was not successful for the following reason: " + status);
      }
    });
  }


function goFullScreenRadar(){
	if(fullScreen == 0){
		//console.log('Full');
		javascript:scroll(0,0);
		$('body').append( $('#mapCanvasPlaceholder>#map_canvas') ); 
		$('div.mapstyle').toggleClass('mapstyle_fullscreen'); 
		$('div#layer_controls').toggleClass('layercontrolsstyle_fullscreen'); 
		$('div#ALLinfoAboveMap').toggleClass('ALLinfoAboveMap_fullscreen');
		
		$('#menuToggleButton').show();
		
		google.maps.event.trigger(map, 'resize');
		fullScreen = 1;
	}
	else{
		
		//console.log('Regular');
	    $('#mapCanvasPlaceholder').append( $('body>#map_canvas') ); 
	    $('div.mapstyle').toggleClass('mapstyle'); 
		$('div#layer_controls').toggleClass('layercontrolsstyle_fullscreen'); 
		$('div#ALLinfoAboveMap').toggleClass('ALLinfoAboveMap_fullscreen');
	    google.maps.event.trigger(map, 'resize');
	    fullScreen = 0;
		$('#menuToggleButton').hide();
	}
	
}


	function gup( name )
 	{
 	  name = name.replace(/[\[]/,"\\\[").replace(/[\]]/,"\\\]");
 	  var regexS = "[\\?&]"+name+"=([^&#]*)";
 	  var regex = new RegExp( regexS );
 	  var results = regex.exec( window.location.href );
 	  if( results == null )
 	    return "";
 	  else
	 	  //console.log(results[1]);
 	    return results[1];
 	}
	
	
	function centerOnRadar(idhere){
		
 		jqxhr = $.getJSON(radar_info_json_path+'?id='+idhere,function(json){});
 		
		jqxhr.success(function(json){
			var latHere = json.radardetails.lat;
			var lonHere = json.radardetails.lon;
			var myLatLng = new google.maps.LatLng(latHere,lonHere);
			map.setCenter(myLatLng);
			map.setZoom(6);
		});

 		jqxhr.complete(function(json){		
		 });
		
		
		jqxhr.error(function(){
			
		});

	}
	
	function goToPermalink(){
		var thisCenter = map.getCenter();
		var newLat = thisCenter.lat();
		var newLon = thisCenter.lng();
		var newZoom = map.getZoom();
		window.location = '?rid='+rid+'&pid='+pid+'&lat='+newLat+'&lon='+newLon+'&zoom='+newZoom;
	}
	
	function shrinkMenuForfFullscreen(growNoMatterWhat){
		if (menuForEverything == 1 || growNoMatterWhat == 0){
			//console.log('hide menu');
			menuForEverything = 0;
			
			$('#layer_controls').hide();
			
			$('#ALLinfoAboveMap').hide();
			
		}
		else{
			//console.log('show menu');
			menuForEverything = 1;

			$('#layer_controls').show();

			$('#ALLinfoAboveMap').show();
		}
		growNoMatterWhat = 0;
		
	}



