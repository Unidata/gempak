function wwaMapType(tileSize) {
	  this.tileSize = tileSize;
	  this.opacity = 0.5;
	}




wwaMapType.prototype.addItToMap = function() {
	map.overlayMapTypes.setAt(21, wwaLayerOverlay);
};




wwaMapType.prototype.refresh = function() {

	map.overlayMapTypes.setAt(22, null);
	map.overlayMapTypes.setAt(21, wwaLayerOverlay);
};




wwaMapType.prototype.setOpacity = function(newOpacity) {
	//console.log(newOpacity);
	//this.toggle();
	this.opacity = '0';
	this.opacity = newOpacity;
	//this.toggle();
	this.refresh();
};

	wwaMapType.prototype.getTile = function(coord, zoom, ownerDocument) {
		  var div = ownerDocument.createElement('DIV');
		  div.innerHTML = coord;
	  

		//var url = 'http://mt1.google.com/vt/lyrs=h@132&hl=en&s=sphericalMercator&z='+zoom+'&x='+coord.x+'&y='+coord.y;
		var url = tileCacheURL+'threat/'+zoom+'/'+coord.x+'/'+coord.y+'.png';
		  div.innerHTML = '<img src="'+url+'"/>';
		  div.style.width = this.tileSize.width + 'px';
		  div.style.height = this.tileSize.height + 'px';
		  div.style.fontSize = '10';
		  div.style.borderStyle = 'solid';
		  div.style.borderWidth = '0px';
		  div.style.borderColor = '#AAAAAA';
		  div.style.opacity = this.opacity;
		  div.id = "wwaLayerDiv";
		  div.name = "wwaLayerDiv";
		  return div;
	};
	
	
	wwaMapType.prototype.toggle = function() {
		//console.log('here: '+this.opacity);
		if(this.opacity != 0.0){
			this.opacity = 0.0;
			$('div#wwaLayerDiv').css('opacity',this.opacity);
		}
		else{
			this.opacity = 0.5;
			$('input:radio[name=wwaopacity]')[0].checked = false;
			$('input:radio[name=wwaopacity]')[1].checked = true;
			$('input:radio[name=wwaopacity]')[2].checked = false;
			$('input:radio[name=wwaopacity]')[3].checked = false;
			$('div#wwaLayerDiv').css('opacity',this.opacity);
		}
		
	}