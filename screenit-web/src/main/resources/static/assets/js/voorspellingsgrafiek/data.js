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

var alleDeelnamekansen;

function leesDeelnamekansen(initialisatiefunctie)
{
	var applicationUrl = document.getElementsByClassName('applicationUrl')[0].value;
	if (applicationUrl.slice(-1) == '/') {
		applicationUrl = applicationUrl.slice(0, applicationUrl.length - 1);
	}

	var subUrl = document.getElementsByClassName('subUrl')[0].value;
    if (subUrl.slice(-1) == '/') {
        subUrl = subUrl.slice(0, subUrl.length - 1);
    }

	var req = new XMLHttpRequest();
	req.onreadystatechange = function()
	{
		if (req.readyState == 4)
		{
			            alleDeelnamekansen = JSON.parse(req.responseText);
            initialisatiefunctie();
		}
	}
	req.open("GET", applicationUrl + subUrl, true);
    req.send();
}
