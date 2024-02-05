/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

	$.extend($.fn.select2.defaults, {
		formatNoMatches: function()
		{
			return "Geen resultaten gevonden";
		},
		formatInputTooShort: function(input, min)
		{
			var n = min - input.length;
			return "Vul " + n + " karakter" + (n == 1 ? "" : "s") + " meer in";
		},
		formatInputTooLong: function(input, max)
		{
			var n = input.length - max;
			return "Vul " + n + " karakter" + (n == 1 ? "" : "s") + " minder in";
		},
		formatSelectionTooBig: function(limit)
		{
			return "Maximaal " + limit + " item" + (limit == 1 ? "" : "s") + " toegestaan";
		},
		formatLoadMore: function(pageNumber)
		{
			return "Meer resultaten laden...";
		},
		formatSearching: function()
		{
			return "Zoeken...";
		}
	});
})(jQuery);
