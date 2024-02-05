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
function biradsKeuze() {
	let biradsKeuze = $('.biradskeuze');
	if (biradsKeuze !== null) {
		$('.biradskeuze .radio.biradsverwijzen>input:checked').each(function () {
			$(this).closest('.biradskeuze').addClass('biradsverwijzen');
		});

		$('.biradskeuze input:radio').click(function () {
			$(this).closest('.biradskeuze').removeClass('biradsverwijzen');
			if ($(this).closest('.radio').hasClass('biradsverwijzen')) {
				$(this).closest('.biradskeuze').addClass('biradsverwijzen');
			}
		});
	}
}

document.addEventListener('DOMContentLoaded', function () {
	biradsKeuze();
});
