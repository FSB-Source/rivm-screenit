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
(function(screenit, $, undefined)
{

	screenit.initClientResponse = function()
	{
		Wicket.Event.subscribe('/ajax/call/beforeSend', function(event, attrs, jqXHR, settings)
		{
			if (window.clientTimeVariable && !attrs.mp)
			{
				var status = 'clientTime=' + window.clientTimeVariable + '&serverTime=' + window.serverTimeVariable;
				if (settings.type.toLowerCase() === 'post') {
					separator = settings.data.length > 0 ? '&' : '';
					settings.data = settings.data + separator + status;
				} else {
					separator = settings.url.indexOf('?') > 1 ? '&' : '?';
					settings.url = settings.url + separator + status;
				}
			}
		});
	}
}(window.screenit = window.screenit || {}, jQuery));

function fadeAlertSucces() {
	setTimeout(function () {
		jQuery(document.getElementsByClassName('alert-success')).fadeOut('slow');
	}, 1500);
}
