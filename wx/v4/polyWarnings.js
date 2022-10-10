
var polyGon = new Array();
var polyGonCoords = new Array();
var warningTypes = new Array("Tornado", "Severe Thunderstorm", "Flash Flood");
var warningColors = new Array("#f0f", "#00f", "#0f0");
var usethisColor = new Array();
var wwaInfoWindow = new Array();


function polyWarnings() {
	// constructor 
	this.onMapOrNot = 1;

	}



polyWarnings.prototype.toggle = function() {
	if(this.onMapOrNot == 1){
		this.removeAll();

		this.onMapOrNot = 0;
	}
	else{
		this.readPolyJSON();

		this.onMapOrNot = 1;
	}
	
	
};



polyWarnings.prototype.refresh = function() {
	if(this.onMapOrNot == 1){
		this.removeAll();
		this.readPolyJSON();
	}
};



polyWarnings.prototype.removeAll = function() {
	
	for(i=0;i<polyGon.length;i++){
		//console.log(i + " " + polyGon.length);
		
		if(polyGon[i]){
			polyGon[i].setMap(null);
		}
		
	}
	
	
};
	
	polyWarnings.prototype.readPolyJSON = function() {

 		jqxhr = $.getJSON(getproducts_path+'?rand='+Math.random()*10000,function(json){
 			//console.log('start');
 		});
 		
		
		
		jqxhr.success(function(json){	

			
			//console.log('success?');
			i = 0;
 			$.each(json.features, function(i,poly){
 				//console.log(""+poly.properties.phenomenon_string + " "+ poly.properties.significance_string + " "+ jQuery.inArray(poly.properties.phenomenon_string, warningTypes));
 				
 				if(jQuery.inArray(poly.properties.phenomenon_string, warningTypes) >= 0){
 				polyGonCoords[i] = [];
 				//console.log("Found! "+poly.properties.phenomenon_string + " "+ poly.properties.significance_string + " "+ jQuery.inArray(poly.properties.phenomenon_string, warningTypes));
 			    
 				for(j=0; j<poly.geometry.coordinates[0].length; j++){

 					polyGonCoords[i].push(new google.maps.LatLng(poly.geometry.coordinates[0][j][1], poly.geometry.coordinates[0][j][0]));
 					//console.log("Lng: "+poly.geometry.coordinates[0][j][0]+" Lat: "+poly.geometry.coordinates[0][j][1]);
 				}
 				

				usethisColor[i] =  warningColors[jQuery.inArray(poly.properties.phenomenon_string, warningTypes)];
				//console.log(jQuery.inArray(poly.properties.phenomenon_string, warningTypes) + ' ' + usethisColor[i]);
				
				
 				polyGon[i] = new google.maps.Polygon({
 			       paths: polyGonCoords[i],
 			       strokeColor: warningColors[jQuery.inArray(poly.properties.phenomenon_string, warningTypes)],
 			      //strokeColor: '#000000',
 			       strokeOpacity: 1.0,
 			       strokeWeight: 3,
 			       fillColor: warningColors[jQuery.inArray(poly.properties.phenomenon_string, warningTypes)],
 			       fillOpacity: 0.35
 			     });
 				
 				wwaInfoWindow[i] = new google.maps.InfoWindow();
 				
 				var popUpTestwwa = '<b>'+poly.properties.phenomenon_string + ' '+ poly.properties.significance_string+'</b><br/>Click for more information.';
 				wwaInfoWindow[i].setContent(popUpTestwwa);
 				
 				
 				google.maps.event.addListener(polyGon[i],"mouseover",function(event){ 
 					this.setOptions({strokeOpacity: 0.0,fillOpacity: 0.75, fillColor: "#000000"}); 
	 				//wwaInfoWindow[i].setPosition(event.latLng);
	 				//wwaInfoWindow[i].open(map);
 					
 				
 				});

 				google.maps.event.addListener(polyGon[i],"mouseout",function(event){ 
 					
 					this.setOptions({strokeOpacity: 0.8,fillOpacity: 0.35, fillColor: warningColors[jQuery.inArray(poly.properties.phenomenon_string, warningTypes)]}); 
 					//wwaInfoWindow[i].setPosition(event.latLng);
 					//wwaInfoWindow[i].close(map);
 					
 				});
 				
 				google.maps.event.addListener(polyGon[i], 'click', function(event) {
 					//console.log("Right click on poly: "+event.latLng.lat());
 					wwaPopUp(event.latLng.lat(), event.latLng.lng(), event);
 				});
 				
 				
 				
 				polyGon[i].setMap(map);
 	 			
 	 			}
 			});	
 			
 			//console.log(polyGon.length);
		});

 		jqxhr.complete(function(json){

			//console.log('complete?');
			//alert ('Complete.');			
		 });
		
		
		jqxhr.error(function(){

			//console.log('error?');
			//alert ('ERROR!');
		});

		
		
	};