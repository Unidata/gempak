function spcMapType(tileSize, spclayerName) {
	this.spclayerName = spclayerName;
	  this.tileSize = tileSize;
	  this.opacity = 0.2;
	}

	spcMapType.prototype.getTile = function(coord, zoom, ownerDocument) {
		  var div = ownerDocument.createElement('DIV');
		  div.innerHTML = coord;
	  

			var url = tileCacheURL+''+this.spclayerName+'/'+zoom+'/'+coord.x+'/'+coord.y+'.png';
			  div.innerHTML = '<img src="'+url+'"/>';
			  div.style.width = this.tileSize.width + 'px';
			  div.style.height = this.tileSize.height + 'px';
			  div.style.fontSize = '10';
			  div.style.borderStyle = 'solid';
			  div.style.borderWidth = '0px';
			  div.style.borderColor = '#AAAAAA';
			  div.style.opacity = this.opacity;
			  div.id = this.spclayerName;
			  div.name = this.spclayerName;
			  return div;
	};
	
	
	spcMapType.prototype.toggle = function() {
		//console.log('here: '+opacityheres);
		if(this.opacity != 0.0){
		this.opacity = 0.0;
		$('div#'+this.spclayerName).css('opacity',this.opacity);
		}
		else{
			this.opacity = 0.3;
			$('div#'+this.spclayerName).css('opacity',this.opacity);
		}
		
	}