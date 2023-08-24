/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

(function($)
{
	"use strict";

	function getjQueryObject(string)
	{

		var jqObj = $("");
		try
		{
			jqObj = $(string).clone();
		}
		catch (e)
		{
			jqObj = $("<span />").html(string);
		}
		return jqObj;
	}

	function isNode(o)
	{

		return !!(typeof Node === "object" ? o instanceof Node : o && typeof o === "object" && typeof o.nodeType === "number" && typeof o.nodeName === "string");
	}

	$.print = $.fn.print = function()
	{

		var options, $this, self = this;

		if (self instanceof $)
		{

			self = self.get(0);
		}

		if (isNode(self))
		{

			$this = $(self);
			if (arguments.length > 0)
			{
				options = arguments[0];
			}
		}
		else
		{
			if (arguments.length > 0)
			{

				$this = $(arguments[0]);
				if (isNode($this[0]))
				{
					if (arguments.length > 1)
					{
						options = arguments[1];
					}
				}
				else
				{

					options = arguments[0];
					$this = $("html");
				}
			}
			else
			{

				$this = $("html");
			}
		}

		var defaults = {
			globalStyles: true,
			mediaPrint: false,
			stylesheet: null,
			noPrintSelector: ".no-print",
			iframe: true,
			append: null,
			prepend: null
		};

		options = $.extend({}, defaults, (options || {}));

		var $styles = $("");
		if (options.globalStyles)
		{

			$styles = $("style, link, meta, title");
		}
		else if (options.mediaPrint)
		{

			$styles = $("link[media=print]");
		}
		if (options.stylesheet)
		{

			$styles = $.merge($styles, $('<link rel="stylesheet" href="' + options.stylesheet + '">'));
		}

		var copy = $this.clone();

		copy = $("<span/>").append(copy);

		copy.find(options.noPrintSelector).remove();

		copy.append($styles.clone());

		copy.append(getjQueryObject(options.append));

		copy.prepend(getjQueryObject(options.prepend));

		var inputs = copy.find("input.string");
		inputs.each(function()
		{
			var val = $(this).val();
			$(this).replaceWith(val);
		});

		var inputs = copy.find("input[type=radio]");
		inputs.each(function()
		{
			var checked = $(this).attr('checked');
			if (checked != null && checked != undefined)
			{
				$(this).attr("checked", "checked");

			}
		});

		var content = copy.html();

		copy.remove();

		var w, wdoc;
		if (options.iframe)
		{

			try
			{
				var $iframe = $(options.iframe + "");
				var iframeCount = $iframe.length;
				if (iframeCount === 0)
				{

					$iframe = $('<iframe height="0" width="0" border="0" wmode="Opaque"/>').prependTo('body').css({
						"position": "absolute",
						"top": -999,
						"left": -999
					});
				}
				w = $iframe.get(0);
				w = w.contentWindow || w.contentDocument || w;
				wdoc = w.document || w.contentDocument || w;
				wdoc.open();
				wdoc.write(content);
				wdoc.close();
				setTimeout(function()
				{

					w.focus();
					try
					{

						if (!w.document.execCommand('print', false, null))
						{

							w.print();
						}
					}
					catch (e)
					{
						w.print();
					}
					setTimeout(function()
					{

						if (iframeCount === 0)
						{

							$iframe.remove();
						}
					}, 100);
				}, 250);
			}
			catch (e)
			{

				console.error("Failed to print from iframe", e.stack, e.message);
				w = window.open();
				w.document.write(content);
				w.document.close();
				w.focus();
				w.print();
				w.close();
			}
		}
		else
		{

			w = window.open();
			w.document.write(content);
			w.document.close();
			w.focus();
			w.print();
			w.close();
		}
		return this;
	};

})(jQuery);
