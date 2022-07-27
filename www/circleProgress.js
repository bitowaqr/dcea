function pingpoliCircleProgressBar( ctx , options )
{
    this.ctx = ctx;
    if ( options ) this.options = options;
    else this.options = {};

    if ( !this.options.hasOwnProperty("enableBackground") ) this.options.enableBackground = true;
    if ( !this.options.hasOwnProperty("backgroundColor") ) this.options.backgroundColor = "#555555";
    if ( !this.options.hasOwnProperty("color") ) this.options.color = "#00ffff";
    if ( !this.options.hasOwnProperty("radius") ) this.options.radius = 50;
    if ( !this.options.hasOwnProperty("backgroundLineWidth") ) this.options.backgroundLineWidth = 10;
    if ( !this.options.hasOwnProperty("lineWidth") ) this.options.lineWidth = 8;
    if ( !this.options.hasOwnProperty("xPos") ) this.options.xPos = 100;
    if ( !this.options.hasOwnProperty("yPos") ) this.options.yPos = 100;
    if ( !this.options.hasOwnProperty("enableRoundLineCaps") ) this.options.enableRoundLineCaps = false;

    this.percent = 0.0;
    this.targetPercent = 0.0;
    this.deltaPercent = 0.0;
    this.stepSize = 0.0;
    this.b_animating = false;

    this.onchange = null;
}



pingpoliCircleProgressBar.prototype.animateTo = function( percent , seconds )
{
    if ( !this.b_animating )
    {
        if ( this.targetPercent != percent )
        {
            this.targetPercent = percent;
            // delta percent is the change of the percent per second
            this.deltaPercent = (this.targetPercent-this.percent)/seconds;
            // every second has 50 steps, therefore the step size is the change per second divided by 50
            this.stepSize = this.deltaPercent/50;
            this.b_animating = true;
            this.step();
        }
    }
}



pingpoliCircleProgressBar.prototype.step = function()
{
    this.percent += this.stepSize;

    if ( Math.abs( this.targetPercent - this.percent ) < Math.abs( this.stepSize ) )
    {
        this.percent = this.targetPercent;
        this.b_animating = false;
    }
    else 
    {
        setTimeout( () => {
            this.step();
        } , 20 );
    }

    if ( this.onchange !== null ) this.onchange();
}



pingpoliCircleProgressBar.prototype.setPercent = function( percent )
{
    this.percent = percent;
    if ( this.percent < 0 ) this.percent = 0;
    if ( this.percent > 1 ) this.percent = 1;
    if ( this.onchange !== null ) this.onchange();
}



pingpoliCircleProgressBar.prototype.draw = function()
{
    // draw the background
    if ( this.options.enableBackground )
    {
        this.ctx.beginPath();
        this.ctx.arc( this.options.xPos , this.options.yPos , this.options.radius , Math.PI*3/2 , Math.PI*3/2+Math.PI*2 );
        this.ctx.strokeStyle = this.options.backgroundColor;
        this.ctx.lineWidth = this.options.backgroundLineWidth;
        this.ctx.stroke();
    }

    if( this.options.enableRoundLineCaps )
    {
        this.ctx.lineCap = 'round';
    }

    this.ctx.beginPath();
    this.ctx.arc( this.options.xPos , this.options.yPos , this.options.radius , Math.PI*3/2 , Math.PI*3/2+this.percent*Math.PI*2 );
    this.ctx.strokeStyle = this.options.color;
    this.ctx.lineWidth = this.options.lineWidth;
    this.ctx.stroke();
}