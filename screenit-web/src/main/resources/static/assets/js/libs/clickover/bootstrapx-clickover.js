/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

!function($)
{
	"use strict"

	var Clickover = function(element, options)
	{

		this.cinit('clickover', element, options);
	}

	Clickover.prototype = $.extend({}, $.fn.popover.Constructor.prototype, {

		constructor: Clickover

		,
		cinit: function(type, element, options)
		{
			this.attr = {};

			this.attr.me = ((Math.random() * 10) + "").replace(/\D/g, '');
			this.attr.click_event_ns = "click." + this.attr.me + " touchstart." + this.attr.me;

			if (!options)
				options = {};

			options.trigger = 'manual';

			this.init(type, element, options);

			this.$element.on('click', this.options.selector, $.proxy(this.clickery, this));

		},
		clickery: function(e)
		{

			if (e)
			{
				e.preventDefault();
				e.stopPropagation();
			}

			this.options.width && this.tip().width(this.options.width);
			this.options.height && this.tip().height(this.options.height);

			this.options.tip_id && this.tip().attr('id', this.options.tip_id);

			this.options.class_name && this.tip().addClass(this.options.class_name);

			this[this.isShown() ? 'hide' : 'show']();

			if (this.isShown())
			{
				var that = this;

				this.options.global_close && $('body').on(this.attr.click_event_ns, function(e)
				{
					if (!that.tip().has(e.target).length)
					{
						that.clickery();
					}
				});

				this.options.esc_close && $(document).bind('keyup.clickery', function(e)
				{
					if (e.keyCode == 27)
					{
						that.clickery();
					}
					return;
				});

				!this.options.allow_multiple && $('[data-clickover-open=1]').each(function()
				{
					$(this).data('clickover') && $(this).data('clickover').clickery();
				});

				this.$element.attr('data-clickover-open', 1);

				this.tip().on('click', '[data-dismiss="clickover"]', $.proxy(this.clickery, this));

				if (this.options.auto_close && this.options.auto_close > 0)
				{
					this.attr.tid = setTimeout($.proxy(this.clickery, this), this.options.auto_close);
				}

				typeof this.options.onShown == 'function' && this.options.onShown.call(this);
				this.$element.trigger('shown');
			}
			else
			{
				this.$element.removeAttr('data-clickover-open');

				this.options.esc_close && $(document).unbind('keyup.clickery');

				$('body').off(this.attr.click_event_ns);

				if (typeof this.attr.tid == "number")
				{
					clearTimeout(this.attr.tid);
					delete this.attr.tid;
				}

				typeof this.options.onHidden == 'function' && this.options.onHidden.call(this);
				this.$element.trigger('hidden');
			}
		},
		isShown: function()
		{
			return this.tip().hasClass('in');
		},
		resetPosition: function()
		{
			var $tip, inside, pos, actualWidth, actualHeight, placement, tp

			if (this.hasContent() && this.enabled)
			{
				$tip = this.tip()

				placement = typeof this.options.placement == 'function' ? this.options.placement.call(this, $tip[0], this.$element[0]) : this.options.placement

				inside = /in/.test(placement)

				pos = this.getPosition(inside)

				actualWidth = $tip[0].offsetWidth
				actualHeight = $tip[0].offsetHeight

				switch (inside ? placement.split(' ')[1] : placement)
				{
				case 'bottom':
					tp = {
						top: pos.top + pos.height,
						left: pos.left + pos.width / 2 - actualWidth / 2
					}
					break
				case 'top':
					tp = {
						top: pos.top - actualHeight,
						left: pos.left + pos.width / 2 - actualWidth / 2
					}
					break
				case 'left':
					tp = {
						top: pos.top + pos.height / 2 - actualHeight / 2,
						left: pos.left - actualWidth
					}
					break
				case 'right':
					tp = {
						top: pos.top + pos.height / 2 - actualHeight / 2,
						left: pos.left + pos.width
					}
					break
				}

				$tip.css(tp)
			}
		},
		debughide: function()
		{
			var dt = new Date().toString();

			console.log(dt + ": clickover hide");
			this.hide();
		}
	})

	$.fn.clickover = function(option)
	{
		return this.each(function()
		{
			var $this = $(this), data = $this.data('clickover'), options = typeof option == 'object' && option

			if (!data)
				$this.data('clickover', (data = new Clickover(this, options)))
			if (typeof option == 'string')
				data[option]()
		})
	}

	$.fn.clickover.Constructor = Clickover

	$.fn.clickover.defaults = $.extend({}, $.fn.popover.defaults, {
		trigger: 'manual',
		auto_close: 0, 
		global_close: 1, 
		esc_close: 1, 
		onShown: null, 
		onHidden: null, 
		width: null, 
		height: null, 
		tip_id: null, 
		class_name: 'clickover', 
		allow_multiple: 0

	})

}(window.jQuery);
