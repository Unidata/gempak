/**
    Displays an animated Game Object
    @author <a href="mailto:matthewcasperson@gmail.com">Matthew Casperson</a>
    @class
*/
function AnimatedGameObject()
{
    /**
        Defines the current frame that is to be rendered
        @type Number
     */
    this.currentFrame = 0;
    /**
        Defines the frames per second of the animation
        @type Number
     */
    this.timeBetweenFrames = 0;
    /**
        The number of individual frames held in the image
        @type Number
     */
    /**
        Time until the next frame
        @type number
     */
    this.timeSinceLastFrame = 0;
    /**
        The width of each individual frame
        @type Number
     */
    this.frameWidth = 0;

    /**
        Initialises this object
        @param image The image to be displayed
		@param x The position on the X axis
        @param y The position on the Y axis
		@param z The depth
        @param frameCount The number of animation frames in the image
        @param fps The frames per second to animate this object at
    */
    this.startupAnimatedGameObject = function(/**Image*/ image, /**Number*/ x, /**Number*/ y, /**Number*/ z, /**Number*/ frameCount, /**Number*/ fps)
    {
        if (frameCount <= 0) throw "framecount can not be <= 0";
        if (fps <= 0) throw "fps can not be <= 0"

        this.startupVisualGameObject(image, x, y, z);
        this.currentFrame = 0;
        this.frameCount = frameCount;
        this.timeBetweenFrames = 1/fps;
        this.timeSinceLastFrame = this.timeBetweenFrames;
        this.frameWidth = this.image.width / this.frameCount;
    }

    /**
        Draws this element to the back buffer
        @param dt Time in seconds since the last frame
		@param context The context to draw to
		@param xScroll The global scrolling value of the x axis
		@param yScroll The global scrolling value of the y axis
    */
    this.draw = function(/**Number*/ dt, /**CanvasRenderingContext2D*/ context, /**Number*/ xScroll, /**Number*/ yScroll)
    {
        var sourceX = this.frameWidth * this.currentFrame;
        context.drawImage(this.image, sourceX, 0, this.frameWidth, this.image.height, this.x - xScroll, this.y - yScroll, this.frameWidth, this.image.height);

        this.timeSinceLastFrame -= dt;
        if (this.timeSinceLastFrame <= 0)
        {
           this.timeSinceLastFrame = this.timeBetweenFrames;
           ++this.currentFrame;
           this.currentFrame %= this.frameCount;
        }
    }
}

AnimatedGameObject.prototype = new VisualGameObject;/**
    The ApplicationManager is used to manage the application itself.
    @author <a href="mailto:matthewcasperson@gmail.com">Matthew Casperson</a>
    @class
*/
function ApplicationManager()
{
    /**
        Initialises this object
        @return A reference to the initialised object
    */
    this.startupApplicationManager = function()
    {
		this.runner = new AnimatedGameObject().startupAnimatedGameObject(g_run, 300, 200, 1, 12, 20);
        return this;
    }
}/**
    The base class for all elements that appear in the game.
    @author <a href="mailto:matthewcasperson@gmail.com">Matthew Casperson</a>
    @class
*/
function GameObject()
{
    /** Display depth order. A smaller zOrder means the element is rendered first, and therefor
        in the background.
        @type Number
    */
    this.zOrder = 0;
    /**
        The position on the X axis
        @type Number
    */
    this.x = 0;
    /**
        The position on the Y axis
        @type Number
    */
    this.y = 0;
    
    /**
        Initialises the object, and adds it to the list of objects held by the GameObjectManager.
        @param x        The position on the X axis
        @param y        The position on the Y axis
        @param z        The z order of the element (elements in the background have a lower z value)
    */
    this.startupGameObject = function(/**Number*/ x, /**Number*/ y, /**Number*/ z)
    {
        this.zOrder = z;
        this.x = x;
        this.y = y;
        g_GameObjectManager.addGameObject(this);
        return this;
    }
    
    /**
        Cleans up the object, and removes it from the list of objects held by the GameObjectManager.
    */
    this.shutdownGameObject = function()
    {
        g_GameObjectManager.removeGameObject(this);
    }
}/**
    A manager for all the objects in the game
    @author <a href="mailto:matthewcasperson@gmail.com">Matthew Casperson</a>
    @class
*/
function GameObjectManager()
{
    /** An array of game objects 
        @type Arary
    */
    this.gameObjects = new Array();
    /** The time that the last frame was rendered  
        @type Date
    */
    this.lastFrame = new Date().getTime();
    /** The global scrolling value of the x axis  
        @type Number
    */
    this.xScroll = 0;
    /** The global scrolling value of the y axis  
        @type Number
    */
    this.yScroll = 0;
    /** A reference to the ApplicationManager instance  
        @type ApplicationManager
    */
    this.applicationManager = null;
    /** A reference to the canvas element  
        @type HTMLCanvasElement
    */
    this.canvas = null;
    /** A reference to the 2D context of the canvas element
        @type CanvasRenderingContext2D
    */
    this.context2D = null;
    /** A reference to the in-memory canvas used as a back buffer 
        @type HTMLCanvasElement
    */
    this.backBuffer = null;
    /** A reference to the backbuffer 2D context 
        @type CanvasRenderingContext2D
    */
    this.backBufferContext2D = null;

    /**
        Initialises this object
        @return A reference to the initialised object
    */
    this.startupGameObjectManager = function()
    {
        // set the global pointer to reference this object
        g_GameObjectManager = this;
        
        // get references to the canvas elements and their 2D contexts
        this.canvas = document.getElementById('canvas');
        this.context2D = this.canvas.getContext('2d');
        this.backBuffer = document.createElement('canvas');
        this.backBuffer.width = this.canvas.width;
        this.backBuffer.height = this.canvas.height;
        this.backBufferContext2D = this.backBuffer.getContext('2d');
        
        // create a new ApplicationManager
        this.applicationManager = new ApplicationManager().startupApplicationManager();
        
        // use setInterval to call the draw function 
        setInterval(function(){g_GameObjectManager.draw();}, SECONDS_BETWEEN_FRAMES);
        
        return this;        
    }
    
    /**
        The render loop
    */
    this.draw = function ()
    {
        // calculate the time since the last frame
        var thisFrame = new Date().getTime();
        var dt = (thisFrame - this.lastFrame)/1000;
        this.lastFrame = thisFrame;
        
        // clear the drawing contexts
        this.backBufferContext2D.clearRect(0, 0, this.backBuffer.width, this.backBuffer.height);
        this.context2D.clearRect(0, 0, this.canvas.width, this.canvas.height);
        
        // first update all the game objects
        for (x in this.gameObjects)
        {
            if (this.gameObjects[x].update)
            {
                this.gameObjects[x].update(dt, this.backBufferContext2D, this.xScroll, this.yScroll);
            }
        }

        // then draw the game objects
        for (x in this.gameObjects)
        {
            if (this.gameObjects[x].draw)
            {
                this.gameObjects[x].draw(dt, this.backBufferContext2D, this.xScroll, this.yScroll);
            }
        }
        
        // copy the back buffer to the displayed canvas
        this.context2D.drawImage(this.backBuffer, 0, 0);
    };
    
    /**
        Adds a new GameObject to the gameObjects collection
        @param gameObject The object to add
    */
    this.addGameObject = function(gameObject)
    {
        this.gameObjects.push(gameObject);
        this.gameObjects.sort(function(a,b){return a.zOrder - b.zOrder;})
    };
    
    /**
        Removes a GameObject from the gameObjects collection
        @param gameObject The object to remove
    */
    this.removeGameObject = function(gameObject)
    {
        this.gameObjects.removeObject(gameObject);
    }
}/** target frames per second  
    @type Number
*/
var FPS = 30;
/** time between frames 
    @type Number
*/
var SECONDS_BETWEEN_FRAMES = 1 / FPS;
/** A global reference to the GameObjectManager instance  
    @type GameObjectManager
*/
var g_GameObjectManager = null;
/** An image to be used by the application  
    @type Image
*/
var g_run = new Image();
g_run.src = "run.png";

// The entry point of the application is set to the init function
window.onload = init;

/**
    Application entry point
*/
function init()
{
    new GameObjectManager().startupGameObjectManager();
}/**
    A class that display a repeating texture that can optionall be offset in either
	the x or y axis
    @author <a href="mailto:matthewcasperson@gmail.com">Matthew Casperson</a>
    @class
*/
function RepeatingGameObject()
{
    /** The width that the final image will take up
		@type Number
	*/
	this.width = 0;
	/** The height that the final image will take up
		@type Number
	*/
    this.height = 0;
	/** How much of the scrollX and scrollY to apply when drawing
		@type Number
	*/
    this.scrollFactor = 1;
	
    /**
        Initialises this object
        @return A reference to the initialised object
    */
    this.startupRepeatingGameObject = function(image, x, y, z, width, height, scrollFactor)
    {
        this.startupVisualGameObject(image, x, y, z);
        this.width = width;
        this.height = height;
        this.scrollFactor = scrollFactor;
        return this;
    }
	
    /**
        Clean this object up
    */
    this.shutdownstartupRepeatingGameObject = function()
    {
        this.shutdownVisualGameObject();
    }
    
	/**
        Draws this element to the back buffer
        @param dt Time in seconds since the last frame
		@param context The context to draw to
		@param xScroll The global scrolling value of the x axis  
		@param yScroll The global scrolling value of the y axis  
    */
    this.draw = function(dt, canvas, xScroll, yScroll)
    {
        var areaDrawn = [0, 0];
        
        for (var y = 0; y < this.height; y += areaDrawn[1])
        {
            for (var x = 0; x < this.width; x += areaDrawn[0])
            {
                // the top left corner to start drawing the next tile from
				var newPosition = [this.x + x, this.y + y];
				// the amount of space left in which to draw
                var newFillArea = [this.width - x, this.height - y];
				// the first time around you have to start drawing from the middle of the image
				// subsequent tiles alwyas get drawn from the top or left
                var newScrollPosition = [0, 0];
                if (x==0) newScrollPosition[0] = xScroll * this.scrollFactor;
                if (y==0) newScrollPosition[1] = yScroll * this.scrollFactor;
                areaDrawn = this.drawRepeat(canvas, newPosition, newFillArea, newScrollPosition);
            }
        }
    }
    
    this.drawRepeat = function(canvas, newPosition, newFillArea, newScrollPosition)
    {
        // find where in our repeating texture to start drawing (the top left corner)
        var xOffset = Math.abs(newScrollPosition[0]) % this.image.width;
        var yOffset = Math.abs(newScrollPosition[1]) % this.image.height;
        var left = newScrollPosition[0]<0?this.image.width-xOffset:xOffset;
        var top = newScrollPosition[1]<0?this.image.height-yOffset:yOffset;
        var width = newFillArea[0] < this.image.width-left?newFillArea[0]:this.image.width-left;
        var height = newFillArea[1] < this.image.height-top?newFillArea[1]:this.image.height-top;
        
        // draw the image
        canvas.drawImage(this.image, left, top, width, height, newPosition[0], newPosition[1], width, height);
        
        return [width, height];
    }
    
    
}
RepeatingGameObject.prototype = new VisualGameObject();/**
    Removes a number of objects from the array
    @param from The first object to remove
    @param to (Optional) The last object to remove
*/
Array.prototype.remove = function(/**Number*/ from, /**Number*/ to)
{
  var rest = this.slice((to || from) + 1 || this.length);
  this.length = from < 0 ? this.length + from : from;
  return this.push.apply(this, rest);
};

/**
    Removes a specific object from the array
    @param object The object to remove
*/
Array.prototype.removeObject = function(object)
{
    for (var i = 0; i < this.length; ++i)
    {
        if (this[i] === object)
        {
            this.remove(i);
            break;
        }
    }
}/**
    The base class for all elements that appear in the game.
    @author <a href="mailto:matthewcasperson@gmail.com">Matthew Casperson</a>
    @class
*/
function VisualGameObject()
{
    /**
        The image that will be displayed by this object
        @type Image
    */
    this.image = null;
    
    /**
        Draws this element to the back buffer
        @param dt Time in seconds since the last frame
		@param context The context to draw to
		@param xScroll The global scrolling value of the x axis  
		@param yScroll The global scrolling value of the y axis  
    */
    this.draw = function(/**Number*/ dt, /**CanvasRenderingContext2D*/ context, /**Number*/ xScroll, /**Number*/ yScroll)
    {
        context.drawImage(this.image, this.x - xScroll, this.y - yScroll);
    }
    
    /**
        Initialises this object
        @param image The image to be displayed
		@param x The position on the X axis
        @param y The position on the Y axis
		@param z The depth
    */
    this.startupVisualGameObject = function(/**Image*/ image, /**Number*/ x, /**Number*/ y, /**Number*/ z)
    {
        this.startupGameObject(x, y, z);
        this.image = image;
        return this;
    }
    
    /**
        Clean this object up
    */
    this.shutdownVisualGameObject = function()
    {
        this.shutdownGameObject();
    }
}
VisualGameObject.prototype = new GameObject;