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
function updateLine(line, x1, x2, y1, y2)
{
	line.setAttribute('x1', x1);
	line.setAttribute('x2', x2);
	line.setAttribute('y1', y1);
	line.setAttribute('y2', y2);
}

function createLine(svg, x1, x2, y1, y2, color, thickness)
{

	var result = document.createElementNS(svg.namespaceURI, 'line');
	updateLine(result, x1, x2, y1, y2);

	result.style.stroke = color;
	result.style.strokeWidth = thickness + 'px';

	svg.appendChild(result);
	return result;
}

function createText(svg, x, y, text) {
	var result = document.createElementNS(svg.namespaceURI, 'text');
	result.setAttribute('x', x);
	result.setAttribute('y', y);

	result.style.fontFamily = "Arial";
	result.style.WebkitUserSelect = "none";
	result.style.userSelect = "none";
	result.style.MozUserSelect = "none";
	result.style.MsUserSelect = "none";

	result.textContent = text;
	svg.appendChild(result);
	return result;
}

function add(total, number)
{
	return total + number;
}
