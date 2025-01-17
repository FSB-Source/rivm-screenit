/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
function borstDoorsnedeVerticaalRechts() {
	$(".RECHTER_BORST_verticaleDoorsnede").each(function (index) {
		$(this).on('drop', (event) => {
			drop(event, $(this), 'RECHTER_BORST', 'verticaleDoorsnede')
			event.stopImmediatePropagation();
		});
		$(this).on('dragover', (event) => {
			dragover(event);
		});
	});

}

function borstDoorsnedeVerticaalLinks() {
	$(".LINKER_BORST_verticaleDoorsnede").each(function (index) {
		$(this).on('drop', (event) => {
			drop(event, $(this), 'LINKER_BORST', 'verticaleDoorsnede')
			event.stopImmediatePropagation();
		});
		$(this).on('dragover', (event) => {
			dragover(event);
		});
	});

}

function borstDoorsnedeHorizontaalRechts() {
	$(".RECHTER_BORST_horizontaleDoorsnede").each(function (index) {
		$(this).on('drop', (event) => {
			drop(event, $(this), 'RECHTER_BORST', 'horizontaleDoorsnede')
			event.stopImmediatePropagation();
		});
		$(this).on('dragover', (event) => {
			dragover(event);
		});
	});

}

function borstDoorsnedeHorizontaalLinks() {
	$(".LINKER_BORST_horizontaleDoorsnede").each(function (index) {
		$(this).on('drop', (event) => {
			drop(event, $(this), 'LINKER_BORST', 'horizontaleDoorsnede')
			event.stopImmediatePropagation();
		});
		$(this).on('dragover', (event) => {
			dragover(event);
		});
	});
}

function laadBorstDoorsnedes() {
	borstDoorsnedeVerticaalRechts();
	borstDoorsnedeVerticaalLinks();
	borstDoorsnedeHorizontaalRechts();
	borstDoorsnedeHorizontaalLinks();
}

document.addEventListener('DOMContentLoaded', function () {
	laadBorstDoorsnedes()
});
