$(function()
{
	var children = [];
	$('.container').children().each(function ()
		{
			children.push(this);
		}
	);
	
	for (var i = 0, len = children.length; i < len; i++)
	{
		if ($(children[i]).is('hr'))
		{
			$('.currslide').wrapAll("<div class='slide' />");
			$('.currslide').removeClass('currslide');
			$(children[i]).remove();
		}
		else
		{
			$(children[i]).addClass('currslide');
		}
	}
	
	if ($('.currslide').length > 0)
	{
		$('.currslide').wrapAll("<div class='slide' />");
		$('.currslide').removeClass('currslide');
	}
	
	var btn1 = $('<input type="button" title="Or use left-arrow"  value="Prev" onclick="prev()" class="nav-button" />');
	var btn2 = $('<input type="button" title="Or use right-arrow" value="Next" onclick="next()" class="nav-button" />');
    btn2.prependTo($("body"));
    btn1.prependTo($("body"));
	
	$(".slide").hide();
	$(".slide").first().addClass("current").show();
	
	$(document).keydown(function(e) {
		switch(e.which) {
			case 37: // left
				prev();
			break;

			case 38: // up
			break;

			case 39: // right
				next();
			break;

			case 40: // down
			break;

			default: return; // exit this handler for other keys
		}
		e.preventDefault(); // prevent the default action (scroll / move caret)
	});
});

function prev()
{
	var current = $(".current");
	if (current.prev().is('div'))
	{
		current.prev().addClass("current").show();
		current.removeClass("current").hide();
	}
}

function next()
{
	var current = $(".current");
	if (current.next().is('div'))
	{
		current.next().addClass("current").show();
		current.removeClass("current").hide();
	}
}
