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
function checkboxWicket() {
	let checkbox = $('.checkboxWicket')

	if (checkbox !== null) {
		checkbox.on('click', function (event) {
			TZ.Wicket.CheckAll.checkAll(this);
		});
	}
}

if (typeof (TZ) == "undefined") {
	TZ = {};
}

if (typeof (TZ.Wicket) == "undefined") {
	TZ.Wicket = {};
}

TZ.Wicket.CheckAll = {};

TZ.Wicket.CheckAll.checkAll = function (checkbox) {
	var th = TZ.Wicket.CheckAll.getAncestor(checkbox, 'TH');
	var table = TZ.Wicket.CheckAll.getAncestor(th, 'TABLE');

	for (i = 0; i < table.rows.length; i++) {
		var row = table.rows[i];
		if (row.cells != null && th.cellIndex < row.cells.length) {
			var inputs = row.cells[th.cellIndex].getElementsByTagName('INPUT');
			if (inputs != null && inputs[0] != null && inputs[0].type == 'checkbox') {
				inputs[0].checked = checkbox.checked;
			}
		}
	}
}

TZ.Wicket.CheckAll.getAncestor = function (element, tagName) {
	if (element.nodeName == tagName) {
		return element;
	}
	return TZ.Wicket.CheckAll.getAncestor(element.parentNode, tagName);
}

document.addEventListener('DOMContentLoaded', function () {
	checkboxWicket();
});
