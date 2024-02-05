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

function VoorspellingsgrafiekenDeelnamekans() {
	const theseGrafieken = this;

	const svg = document.getElementById('voorspellingsgrafiekDeelnamekans');
	const svgMatrix1 = document.getElementById('voorspellingsGrafiekMatrix1');
	const svgMatrix2 = document.getElementById('voorspellingsGrafiekMatrix2');
	const svgDiv = document.getElementById('voorspellingsgrafiekDeelnamekansDiv');
	const inputbox = document.getElementsByClassName('afspraakDrempel')[0];
	const afspraakDrempelSOElement = document.getElementsByClassName('afspraakDrempelSO')[0];

	svg.setAttribute('width', 910);
	svg.setAttribute('height', 500);
	svgMatrix1.setAttribute('width', 375);
	svgMatrix2.setAttribute('width', 475);
	const texthoogte = 10;
	const textregelafstand = 6;
	const textregelafstandXAsTekst = 16;
	const marginLeft = 60;
	const afstandTussenGrafieken = 40;
	const marginTop = 30;
	const marginBottom = 20;
	const xMetadataBovenaan = 750;
	const yMetadataBovenaan = texthoogte;
	const streepjeLengte = 5;
	const grafiekWidth = (svg.getAttribute('width') - afstandTussenGrafieken) / 2 - marginLeft - 20;
	const grafiekHeight = 263;
	const deelnamekanslijnColor = '#429bde';
	const afspraakDrempellijnHorizontaalColor = '#9300c8';
	const afspraakDrempellijnVerbindingColor = '#f4e3ff';
	const afspraakDrempellijnVerticaalOnderAfspraakDrempelColor = afspraakDrempellijnHorizontaalColor;
	const onzekerheidslijnColor = '#ff901c';
	const afspraakDrempellijnVerticaalBovenAfspraakDrempelColor = onzekerheidslijnColor;
	const onzekerheidsvlakColor = '#ffe7c1';
	const assenColor = '#474747';
	const deelnamekanslijndikte = 2;
	const onzekerheidslijndikte = 1;
	const afspraakDrempellijndikte = 1;
	const aantalKansstreepjeVakken = 10;
	const aantalKansstreepjeVakkenTussenGetallen = 2;
	const aantalClientstreepjeVakken = 10;
	const aantalClientstreepjeVakkenTussenGetallen = 10;

	const legendaMarginLeft = 460;
	const legendaMarginTop = 40;
	const legendaLijnlengte = 40;
	const legendaVlakHoogte = 20;
	const legendaAfstandTussenLijnEnTekst = 4;
	const legendaAfstandTussenLijnen = 25;
	const grafiekRight = marginLeft + grafiekWidth;
	const grafiekBottom = marginTop + grafiekHeight;
	const svgNS = svg.namespaceURI;
	const afspraakDrempelnGekoppeld = true;

	const legendaBovenkant = marginTop + grafiekHeight + streepjeLengte + texthoogte + legendaMarginTop;
	const legendaOnderkant = legendaBovenkant + legendaVlakHoogte;

	const initieleAfspraakDrempel = inputbox.value !== '' ? inputbox.value : afspraakDrempelSOElement.innerHTML;

	var grafiekVervolgRonde = new Voorspellingsgrafiek('vervolgRonde', 0, 'Vervolgronde');
	var grafiekEersteRonde = new Voorspellingsgrafiek('eersteRonde', grafiekVervolgRonde.getRight() + afstandTussenGrafieken, 'Eerste ronde');

	var lengendalijnX = legendaMarginLeft;
	var lengendalijnY = marginTop + grafiekHeight + streepjeLengte + texthoogte + legendaMarginTop;

	$('#voorspellingsgrafiekDeelnamekansDiv').mousemove(function (event) {
		deVoorspellingsgrafiekenDeelnamekans.mouseMove(event);
	});

	$('#voorspellingsgrafiekDeelnamekansDiv').mousedown(function (event) {
		deVoorspellingsgrafiekenDeelnamekans.mouseDown(event)
	});

	$('#voorspellingsgrafiekDeelnamekansDiv').mouseup(function (event) {
		deVoorspellingsgrafiekenDeelnamekans.mouseUp(event)
	});

	$('#js-voorspelling-grafiek').change(function (event) {
		deVoorspellingsgrafiekenDeelnamekans.onAfspraakDrempelChange();
	});

	svg.style.marginLeft = '20px';
	svg.style.display = 'block';
	svgMatrix1.style.marginLeft = '20px';
	svgMatrix1.style.display = 'block';
	svgMatrix2.style.marginLeft = '20px';
	svgMatrix2.style.display = 'block';

	createLegendaLijn(deelnamekanslijnColor, deelnamekanslijndikte, 'Deelnamekans', 85);
	createLegendaLijn(onzekerheidslijnColor, onzekerheidslijndikte, 'Onzekerheid', 80);
	createLegendaVlak(onzekerheidsvlakColor, 'Totale onzekerheid');

	var matrix1;
	var matrix2;

	leesDeelnamekansen(function () {
		createText(xMetadataBovenaan, yMetadataBovenaan, 'Geboortejaren ' + alleDeelnamekansen.vanafGeboortejaar + '-' + alleDeelnamekansen.totEnMetGeboortejaar, '');

		matrix1 = new VoorspellingsgrafiekMatrix(svgMatrix1, marginBottom, '', ['Totaal', 'Vervolgronde', 'Eerste ronde']);
		matrix2 = new VoorspellingsgrafiekMatrix(svgMatrix2, marginBottom, 'De onderstaande groepen overlappen', ['Dubbele tijd', 'Minder valide', 'Tehuis', 'Suspect']);

		grafiekVervolgRonde.init(alleDeelnamekansen.kolommen.find(function (kolom) {
			return kolom.naam === 'Vervolgronde';
		}).cumulatieveDeelnamekansVerdeling);
		grafiekEersteRonde.init(alleDeelnamekansen.kolommen.find(function (kolom) {
			return kolom.naam === 'Eerste ronde';
		}).cumulatieveDeelnamekansVerdeling);

		svg.style.height = legendaOnderkant;

	});

	function legendaTekst(tekst, y, tekstbreedte) {
		var textLines = tekst.match(/[^\r\n]+/g);
		for (var i in textLines) {
			const regelhoogte = (texthoogte + textregelafstand);
			var text = createText(lengendalijnX + legendaAfstandTussenLijnEnTekst,
				y - ((textLines.length - 1) * regelhoogte / 2) + texthoogte / 2 - 2 + regelhoogte * i,
				textLines[i]);
		}
		var textlengte = text.getComputedTextLength();
		if (!textlengte) {
			textlengte = tekstbreedte ? tekstbreedte : 60;
		}
		lengendalijnX += legendaAfstandTussenLijnEnTekst + textlengte + legendaAfstandTussenLijnen;
	}

	function createLegendaLijn(color, lijndikte, text, tekstbreedte) {
		var x = lengendalijnX;
		var y = lengendalijnY;
		if (color != null) {
			var result = document.createElementNS(svgNS, 'line');
			updateLine(result, x, x + legendaLijnlengte, y, y, color, lijndikte);
			lengendalijnX += legendaLijnlengte;
			result.style.stroke = color;
			result.style.strokeWidth = lijndikte + 'px';
			svg.appendChild(result);
		}
		legendaTekst(text, y, tekstbreedte);
	}

	function createLegendaVlak(color, text) {
		var x = lengendalijnX;
		var y = lengendalijnY;
		lengendalijnX += legendaLijnlengte;
		if (color != null) {
			var result = document.createElementNS(svgNS, 'rect');

			result.setAttribute('width', legendaLijnlengte);
			result.setAttribute('height', legendaVlakHoogte);
			result.setAttribute('x', x);
			result.setAttribute('y', y - legendaVlakHoogte / 2);
			result.style.fill = color;

			svg.appendChild(result);
		}
		legendaTekst(text, y);
	}

	function Voorspellingsgrafiek(vervolgRondeOfEersteRonde, left, naam) {
		var thisGrafiek = this; 

		var afspraakDrempellijnVerticaalOnderAfspraakDrempel; 
		var afspraakDrempellijnVerticaalBovenAfspraakDrempel; 
		var afspraakDrempellijnHorizontaal; 
		var afspraakDrempellijnHorizontaalVerbinding;
		var onzekerheidsvlak; 
		var xAtMouseDown; 
		var afspraakDrempel = initieleAfspraakDrempel; 
		var height = grafiekHeight;
		var top = marginTop;
		var bottom = marginTop + height; 
		var right = marginLeft + grafiekWidth;
		var cumulatieveDeelnamekansVerdeling;

		this.init = function (cdv) {
			cumulatieveDeelnamekansVerdeling = cdv;
			onzekerheidsvlak = createOnzekerheidsvlak();
			createDeelnamekanslijn();
			createOnzekerheidslijn();
			afspraakDrempellijnHorizontaal = createLine(marginLeft, marginLeft, grafiekBottom, grafiekBottom, afspraakDrempellijnHorizontaalColor, afspraakDrempellijndikte);
			if (afspraakDrempelnGekoppeld) {
				afspraakDrempellijnHorizontaalVerbinding = createLine(marginLeft, marginLeft, grafiekBottom, grafiekBottom, afspraakDrempellijnVerbindingColor,
					afspraakDrempellijndikte);
			}
			afspraakDrempellijnVerticaalOnderAfspraakDrempel = createLine(marginLeft,
				marginLeft,
				grafiekBottom,
				grafiekBottom,
				afspraakDrempellijnVerticaalOnderAfspraakDrempelColor,
				afspraakDrempellijndikte);
			afspraakDrempellijnVerticaalBovenAfspraakDrempel = createLine(marginLeft,
				marginLeft,
				grafiekBottom,
				grafiekBottom,
				afspraakDrempellijnVerticaalBovenAfspraakDrempelColor,
				afspraakDrempellijndikte);

			createLine(marginLeft, marginLeft, marginTop, grafiekBottom, assenColor, 1); 
			for (var kansStreepjeIndex = 0; kansStreepjeIndex <= aantalKansstreepjeVakken; kansStreepjeIndex++) {
				var deelnamekans = 100 * kansStreepjeIndex / aantalKansstreepjeVakken; 

				var y = yObvKans(deelnamekans);
				createLine(marginLeft - streepjeLengte, marginLeft, y, y, assenColor, 1);
				if (kansStreepjeIndex % aantalKansstreepjeVakkenTussenGetallen == 0) {
					createText(left + marginLeft - streepjeLengte - 1, y + texthoogte / 2, deelnamekans.toFixed(0) + '%', 'text-anchor: end');
				}
			}
			var x = left + texthoogte;
			var y = top + grafiekHeight / 2;
			var text = createText(x, y, 'Gemiddeld ' + gemiddeldeDeelnamekans() + '%', 'text-anchor: middle').setAttribute('transform', 'rotate(270 ' + x + ', ' + y + ')');

			createLine(marginLeft, grafiekRight, grafiekBottom, grafiekBottom, assenColor, 1); 
			for (var clientStreepjeIndex = 0; clientStreepjeIndex <= aantalClientstreepjeVakken; clientStreepjeIndex++) {
				var x = marginLeft + grafiekWidth * clientStreepjeIndex / aantalClientstreepjeVakken;
				createLine(x, x, bottom, bottom + streepjeLengte, assenColor, 1);
				if (clientStreepjeIndex % aantalClientstreepjeVakkenTussenGetallen == 0) { 
					var number = Math.round(cumulatieveDeelnamekansVerdeling[100]
						* clientStreepjeIndex / aantalClientstreepjeVakken);
					createText(left + x, bottom + streepjeLengte + texthoogte + 3, number.toLocaleString('nl'), 'text-anchor: middle');
				}
			}
			createText(left + marginLeft + grafiekWidth / 2, bottom + streepjeLengte + texthoogte + textregelafstandXAsTekst, naam, 'text-anchor: middle');

			thisGrafiek.setAfspraakDrempel(afspraakDrempel);
			matrix1.updateData(afspraakDrempel);
			matrix2.updateData(afspraakDrempel);
		};

		this.isInGrafiek = function (x, y) {
			return x >= this.getLeft() + marginLeft && x <= this.getRight() && y >= this.getTop() && y <= this.getBottom();
		};

		function gemiddeldeDeelnamekans() {
			switch (vervolgRondeOfEersteRonde) {
				case 'vervolgRonde':
					return alleDeelnamekansen.gemiddeldeDeelnamekansVervolgRonde;
				case 'eersteRonde':
					return alleDeelnamekansen.gemiddeldeDeelnamekansEersteRonde;
				default:
					return 'onbekende categorie: ' + vervolgRondeOfEersteRonde;
			}
		}

		this.getAfspraakDrempel = function () {
			return afspraakDrempel;
		};

		this.setAfspraakDrempel = function (newValue) {
			afspraakDrempel = newValue;
			var x = xVanDeelnamekans(Math.max(afspraakDrempel, 0));
			var y = yObvKans(afspraakDrempel);
			var yOnzekerheid = yObvKans(onzekerheidObvDeelnamekans(afspraakDrempel));
			updateLine(afspraakDrempellijnVerticaalOnderAfspraakDrempel, x, x, y, grafiekBottom);
			updateLine(afspraakDrempellijnVerticaalBovenAfspraakDrempel, x, x, yOnzekerheid, y);
			updateLine(afspraakDrempellijnHorizontaal, marginLeft, x, y, y);
			if (afspraakDrempelnGekoppeld) {
				if (thisGrafiek.isRechter()) {
					updateLine(afspraakDrempellijnHorizontaalVerbinding, x, right, y, y);
				} else {
					updateLine(afspraakDrempellijnHorizontaalVerbinding, x, andereGrafiek(thisGrafiek).getLeft() + marginLeft - thisGrafiek.getLeft(), y, y);
				}
			}
			updateOnzekerheidsvlak(left + x);
		};

		function updateAfspraakDrempellijnenObvEvent(e) {
			if (!inputbox.disabled) {
				var x = e.clientX - svg.getBoundingClientRect().left - left;
				if (x < marginLeft) {
					x = marginLeft;
				} else if (x > grafiekRight - 1) {
					x = grafiekRight - 1;
				}
				var y = e.clientY - svg.getBoundingClientRect().top;
				var y = Math.min(Math.max(top, y), bottom);
				const deDeelnamekans = deelnamekansVanY(y);
				thisGrafiek.setAfspraakDrempel(deDeelnamekans);
				matrix1.updateData(deDeelnamekans);
				matrix2.updateData(deDeelnamekans);
				inputbox.value = deDeelnamekans;
			}
		}

		this.mouseDown = function (e) {
			updateAfspraakDrempellijnenObvEvent(e);
			xAtMouseDown = e.clientX - svg.getBoundingClientRect().left - left;
			verwerkEventueleKoppeling(thisGrafiek);
		};

		this.mouseUp = function (e) {
			if (typeof xAtMouseDown !== 'undefined') {
				updateAfspraakDrempellijnenObvEvent(e);
				xAtMouseDown = undefined;
				verwerkEventueleKoppeling(thisGrafiek);
			}
		};

		this.mouseMove = function (e) {
			if (typeof xAtMouseDown !== 'undefined' && e.which === 1) {
				updateAfspraakDrempellijnenObvEvent(e);
				verwerkEventueleKoppeling(thisGrafiek);
			} else {
				xAtMouseDown = undefined;
			}
		};

		function updateLine(line, x1, x2, y1, y2) {
			line.setAttribute('x1', left + x1);
			line.setAttribute('x2', left + x2);
			line.setAttribute('y1', y1);
			line.setAttribute('y2', y2);
		}

		function createLine(x1, x2, y1, y2, color, thickness) {
			var result = document.createElementNS(svgNS, 'line');
			updateLine(result, x1, x2, y1, y2, color, thickness);
			result.style.stroke = color;
			result.style.strokeWidth = thickness + "px";
			svg.appendChild(result);
			return result;
		}

		function deelnamekansVanY(y) {
			return Math.round(100 * (bottom - y) / height);
		}

		function xVanDeelnamekans(deelnamekans) {
			return marginLeft + grafiekWidth * cumulatieveDeelnamekansVerdeling[deelnamekans] / cumulatieveDeelnamekansVerdeling[100];
		}

		function yObvKans(deelnamekans) {
			return grafiekBottom - grafiekHeight * deelnamekans / 100;
		}

		function createDeelnamekanslijn() {
			var result = document.createElementNS(svgNS, 'polyline');
			var points = '';
			var deltaY = grafiekHeight / 100;

			for (var deelnamekansProcenten = 1; deelnamekansProcenten <= 100; deelnamekansProcenten++) {
				points += (marginLeft + left + grafiekWidth * cumulatieveDeelnamekansVerdeling[deelnamekansProcenten - 1] / cumulatieveDeelnamekansVerdeling[100]) + ','
					+ (grafiekBottom - grafiekHeight * deelnamekansProcenten / 100) + ' ';
			}
			result.setAttribute('points', points);
			result.style.fill = "none";
			result.style.stroke = deelnamekanslijnColor;
			result.style.strokeWidth = deelnamekanslijndikte
			svg.appendChild(result);
			return result;
		}

		function onzekerheidsPoints(vanafX) {
			var points = '';

			for (var deelnamekans = 1; deelnamekans <= 50; deelnamekans++) {
				const x = left + xVanDeelnamekans(deelnamekans);
				if (x >= vanafX) {
					points += x + ',' + (grafiekBottom - grafiekHeight * deelnamekans / 50) + ' ';
				}
			}

			for (var deelnamekans = 51; deelnamekans <= 100; deelnamekans++) {
				const x = left + xVanDeelnamekans(deelnamekans);
				if (x >= vanafX) {
					points += x + ',' + (grafiekBottom - grafiekHeight * (100 - deelnamekans) / 50) + ' ';
				}
			}

			return points;
		}

		function onzekerheidObvDeelnamekans(deelnamekans) {
			return 2 * (deelnamekans <= 50 ? deelnamekans : 100 - deelnamekans);
		}

		function createOnzekerheidslijn() {
			var result = document.createElementNS(svgNS, 'polyline');
			result.setAttribute('points', onzekerheidsPoints(marginLeft));
			result.style.fill = "none";
			result.style.stroke = "onzekerheidslijnColor";
			result.style.strokeWidth = onzekerheidslijndikte;
			svg.appendChild(result);
			return result;
		}

		function updateOnzekerheidsvlak(vanafX) {
			onzekerheidsvlak.setAttribute('points', onzekerheidsPoints(vanafX) + (left + grafiekRight) + ',' + grafiekBottom + ' ' + (vanafX) + ',' + grafiekBottom);
		}

		function createOnzekerheidsvlak() {
			var vanafX = xVanDeelnamekans(Math.max(afspraakDrempel, 1));
			var result = document.createElementNS(svgNS, 'polyline');
			var xScherm = left + vanafX;
			result.setAttribute('points', onzekerheidsPoints(vanafX) + xScherm + ',' + grafiekBottom);
			result.style.fill = onzekerheidsvlakColor;
			result.style.stroke = "none";
			result.style.strokeWidth = 2;
			svg.appendChild(result);
			return result;
		}

		this.getLeft = function () {
			return left;
		};

		this.getRight = function () {
			return left + marginLeft + grafiekWidth;
		};
		this.getTop = function () {
			return top;
		};
		this.getBottom = function () {
			return bottom;
		};

		this.isRechter = function () {
			return thisGrafiek.getRight() > andereGrafiek(thisGrafiek).getRight();
		};
	}

	function grafiekVanEvent(e) {
		if (grafiekEersteRonde.getLeft() > grafiekVervolgRonde.getLeft()
			&& e.clientX - svg.getBoundingClientRect().left >= grafiekEersteRonde.getLeft()) {
			return grafiekEersteRonde;
		} else {
			return grafiekVervolgRonde;
		}
	}

	function andereGrafiek(grafiek) {
		if (grafiek == grafiekVervolgRonde) {
			return grafiekEersteRonde;
		} else {
			return grafiekVervolgRonde;
		}
	}

	function verwerkEventueleKoppeling(grafiek) {
		if (afspraakDrempelnGekoppeld) {
			andereGrafiek(grafiek).setAfspraakDrempel(grafiek.getAfspraakDrempel());
		}
	}

	this.mouseDown = function (e) {
		var x = e.clientX - svg.getBoundingClientRect().left;
		var y = e.clientY - svg.getBoundingClientRect().top;
		if (grafiekVervolgRonde.isInGrafiek(x, y)) {
			grafiekVervolgRonde.mouseDown(e);
		} else if (grafiekEersteRonde.isInGrafiek(x, y)) {
			grafiekEersteRonde.mouseDown(e);
		}
	};

	this.mouseUp = function (e) {
		grafiekVanEvent(e).mouseUp(e);
	};

	this.mouseMove = function (e) {
		grafiekVanEvent(e).mouseMove(e);
	};

	function updateLine(line, x1, x2, y1, y2) {
		line.setAttribute('x1', x1);
		line.setAttribute('x2', x2);
		line.setAttribute('y1', y1);
		line.setAttribute('y2', y2);
	}

	function createText(x, y, text, style) {
		var result = document.createElementNS(svgNS, 'text');
		result.setAttribute('x', x);
		result.setAttribute('y', y);

		result.style = style;
		result.style.fontFamily = "Arial";
		result.style.MozUserSelect = "none";
		result.style.WebkitUserSelect = "none";
		result.style.MsUserSelect = "none";
		result.style.userSelect = "none";

		result.textContent = text;
		svg.appendChild(result);
		return result;
	}

	this.onAfspraakDrempelChange = function () {
		var value = inputbox.value !== '' ? inputbox.value : afspraakDrempelSOElement.innerHTML;
		if (value == null || value == '') {
			document.getElementsByClassName('afspraakDrempel')[0].value = grafiekVervolgRonde.getAfspraakDrempel();
		} else {
			if (value < 0) {
				value = 0;
			} else if (value > 100) {
				value = 100;
			}
			grafiekVervolgRonde.setAfspraakDrempel(value);
			grafiekEersteRonde.setAfspraakDrempel(value);
			matrix1.updateData(value);
			matrix2.updateData(value);
		}
	};

	$('.afspraakDrempelBk').change(function () {
		theseGrafieken.onAfspraakDrempelChange();
	});
}

var deVoorspellingsgrafiekenDeelnamekans;

function percentage(aantal, totaal) {
	return Math.floor(100 * aantal / totaal) + '%';
}
