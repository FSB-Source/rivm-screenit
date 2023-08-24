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
function datepickerFilterDatumJaar() {
	let datepickerFilterJaar = $('.js-datepicker-filter-datum-jaar')

	if (datepickerFilterJaar !== null) {
		datepickerFilterJaar.on('click', function (event) {
			$('.filterDatum.startDatum').datepicker('setDate', new Date(new Date().getFullYear(), 0, 1));
			$('.filterDatum.endDatum').datepicker('setDate', new Date(new Date().getFullYear(), 11, 31));
			$('.filterDatum').change();
		});
	}
}

function datepickerFilterDatum() {
	let datepickerFilterVandaag = $('.js-datepicker-filter-datum-vandaag');

	if (datepickerFilterVandaag !== null) {
		datepickerFilterVandaag.on('click', function (event) {
			$('.filterDatum').datepicker('setDate', new Date());
			$('.filterDatum').change();
		});
	}
}

document.addEventListener('DOMContentLoaded', function () {
	datepickerFilterDatumJaar();
	datepickerFilterDatum();
});
