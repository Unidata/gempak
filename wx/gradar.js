
NIDSLayerOverlay.prototype = new google.maps.OverlayView();

function NIDSLayerOverlay(obj){
	this.bbox=obj.bbox ? obj.bbox : {};
	this.size=obj.size ? obj.size : {};
	this.last=obj.last ? obj.last :  0;
	this.skew=obj.skew ? obj.skew :  0;
	this.wrap=obj.wrap ? obj.wrap : 10;
	this.name=obj.name ? obj.name : "";
	this.filter=1;
	this.adjust=1;
	var div = this.div_=document.createElement("DIV");
}

NIDSLayerOverlay.prototype.onAdd=function(set){
	//this.set=set;
	this.div_.style.position="absolute";
	//thisset.getPane(G_MAP_MAP_PANE).appendChild(this.div);
        var panes = this.getPanes().overlayLayer;
        panes.appendChild(this.div_)
	for (var i=0;this.wrap-i;i++){
		var gif=document.createElement("IMG");
		gif.style.position="absolute";
		gif.style.visibility="hidden";
		gif.style.display="none";
		gif.style.left  ="0px";
		gif.style.top   ="0px";
		gif.style.width =this.adjust ? "100%" : "";
		gif.style.height=this.adjust ? "100%" : "";
		gif.onload =this.loaded;
		gif.onerror=this.failed;
		gif.galleryImg="no";
		this.div_.appendChild(gif);
	}
}

NIDSLayerOverlay.prototype.onRemove=function(){
	for (var i=0;this.wrap-i;i++){
		var gif=this.div.childNodes[this.wrap-i-1];
		this.div_.removeChild(gif);
	}
	this.div_.parentNode.removeChild(this.div_);
	this.div_ = null;
	//this.set.getPane(G_MAP_MAP_PANE).removeChild(this.div);
}

NIDSLayerOverlay.prototype.draw=function(repair){
	if (!repair) return
	console.log("ha");
	var z0=this.set.fromLatLngToDivPixel(new GLatLng(this.bbox.y0,this.bbox.x0));
	var z1=this.set.fromLatLngToDivPixel(new GLatLng(this.bbox.y1,this.bbox.x1));
	console.log(z1);
	if (this.adjust){
		this.div.style.left  =Math.min(z1.x,z0.x)+"px";
		this.div.style.top   =Math.min(z1.y,z0.y)+"px";
		this.div.style.width =Math.abs(z1.x-z0.x)+"px";
		this.div.style.height=Math.abs(z1.y-z0.y)+"px";
	} else {
		this.div.style.left  =(z1.x+z0.x-this.size.x)>>1+"px";
		this.div.style.top   =(z1.y+z0.y-this.size.y)>>1+"px";
		this.div.style.width =this.size.x+"px";
		this.div.style.height=this.size.y+"px";
	}
}

NIDSLayerOverlay.prototype.copy=function(){
	return new NIDSLayerOverlay({bbox:this.bbox,size:this.size,last:this.last,skew:this.skew,wrap:this.wrap,name:this.name});
}

NIDSLayerOverlay.prototype.loaded=function(gif){
	gif=window.event ? window.event.srcElement : gif.currentTarget;
	gif.style.display="";
	gif.loaded=1;
	gif.failed=0;
}

NIDSLayerOverlay.prototype.failed=function(gif){
	gif=window.event ? window.event.srcElement : gif.currentTarget;
	if (!gif.failed) gif.src=gif.src;
	gif.style.display="none";
	gif.loaded=0;
	gif.failed=1;
}

NIDSLayerOverlay.prototype.hide=function(){
	this.div.style.visibility="hidden";
}

NIDSLayerOverlay.prototype.show=function(){
	this.div.style.visibility="";
}

NIDSLayerOverlay.prototype.opacity=function(filter){
	this.filter=filter;
	if (document.all){
		this.div.style.filter="alpha(opacity=100)";
		if ((this.filter>0)*(this.filter<1)){
			this.div.filters.alpha.opacity=Math.round(this.filter*100);
			this.div.filters.alpha.enabled=1;
		} else {
			this.div.filters.alpha.opacity=100;
			this.div.filters.alpha.enabled=0;
		}
	} else {
		if ((this.filter>0)*(this.filter<1)){
			this.div.style.opacity=this.filter;
		} else {
			this.div.style.opacity=1;
		}
	}
}

NIDSLayerOverlay.prototype.rescale=function(adjust){
	this.adjust=adjust;
	for (var i=0;this.wrap-i;i++){
		var gif=this.div.childNodes[i];
		gif.style.left  ="0px";
		gif.style.top   ="0px";
		gif.style.width =this.adjust ? "100%" : "";
		gif.style.height=this.adjust ? "100%" : "";
	}
	this.draw(true);
}

NIDSLayerOverlay.prototype.select=function(offset){
	offset=(this.skew+offset)%this.wrap;
	var gif=this.div.childNodes[offset];
	if (gif.loaded) this.div.childNodes[this.last].style.visibility="hidden";
	if (gif.loaded) this.last=offset;
	if (gif.loaded) this.div.childNodes[this.last].style.visibility="";
	return this.div.childNodes[this.last].src;
}

NIDSLayerOverlay.prototype.feed=function(recall){
	var div=document.getElementById("("+this.name+")");
	if (div) document.body.removeChild(div);
	div=document.createElement("SCRIPT");
	if (div) document.body.appendChild(div);
	div.src=recall+"&random="+(""+Math.random().toFixed(10)+"").slice(-10);
	div.id="("+this.name+")";
}

NIDSLayerOverlay.prototype.reload=function(recall){
	var gif,src;
	var set=[0,0];
	for (var i=0;recall[i];i++){
		src=recall[i];
		if (gif=this.div.childNodes[(this.skew+i+0)%this.wrap]) set[0]+=gif.loaded*(gif.src==src);
		if (gif=this.div.childNodes[(this.skew+i+1)%this.wrap]) set[1]+=gif.loaded*(gif.src==src);
	}

	if ((set[0]==(i-0))*(set[1]==0))
		return;

	if ((set[0]==0)*(set[1]==(i-1))){
		gif=this.div.childNodes[(this.skew+0)%this.wrap];
		this.skew=(this.skew+1)%this.wrap;
//		this.div.removeChild(gif);
//		this.div.appendChild(gif);
		gif.style.display="none";
		gif.loaded=0;
		gif.failed=0;
		gif.src=src;
		return;
	}

	for (var i=0;recall[i];i++){
		src=recall[i];
		gif=this.div.childNodes[(this.skew+i)%this.wrap];
		gif.style.display="none";
		gif.loaded=0;
		gif.failed=0;
		gif.src=src;
	}
}

