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
function checkboxOnderzoeken() {
	let deselectOnderzoek = $('.js-checkbox-deselecteer-onderzoeken')

	if (deselectOnderzoek !== null) {
		deselectOnderzoek.click(function () {
			$('.selectonderzoek').prop('checked', false);
		});
	}

	let selectOnderzoek = $('.js-checkbox-selecteer-onderzoeken')

	if (selectOnderzoek !== null) {
		selectOnderzoek.click(function () {
			$('.selectonderzoek').prop('checked', true);
		});
	}
}

document.addEventListener('DOMContentLoaded', function () {
	checkboxOnderzoeken();
});
