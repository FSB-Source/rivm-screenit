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
	var _size = $.ui.dialog.prototype._size;
	$.ui.dialog.prototype._size = function()
	{
		var self = this;
		var options = this.options;

		_size.apply(this, arguments);

		if ('window' === options.height)
		{
			var maxHeight = options.maxHeight;

			if (maxHeight > 0)
			{
				self.element.height(Math.min($(window).height() - 40, options.maxHeight));
			}
			else
			{
				self.element.height($(window).height() - 40);
			}
		}
	};

	var open = $.ui.dialog.prototype.open;
	$.ui.dialog.prototype.open = function()
	{
		try
		{
			document.documentElement.focus();
		}
		catch (e)
		{
		}

		open.apply(this, arguments);
	};
})(jQuery);
