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
function addBrievenUitklapToggleFunction() {
	let briefTemplateUitklappen = $('.briefTemplate');

	if (briefTemplateUitklappen !== null) {
		briefTemplateUitklappen.on('click', function () {
			toggleBriefTemplateVersions(this);
		});
	}
}

function toggleBriefTemplateVersions(briefType) {
	var briefTypeClass = briefType.getAttribute('data-value');
	if (briefType.subrowsVisible) {
		$('.' + briefTypeClass)
			.removeClass('display-inline-block')
			.addClass('display-none');
		briefType.src = '../../assets/images/icons/arrow.gif';
		briefType.subrowsVisible = false;
	} else {
		$('.' + briefTypeClass)
			.removeClass('display-none')
			.addClass('display-inline-block');
		briefType.src = '../../assets/images/icons/arrowDown.gif';
		briefType.subrowsVisible = true;
	}
}

document.addEventListener('DOMContentLoaded', function () {
	addBrievenUitklapToggleFunction();
});
