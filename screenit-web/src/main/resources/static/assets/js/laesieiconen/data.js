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
var laesies = []; 

var laesieImg2lezingId = new Map(); 
var laesieImg2icoon = new Map(); 
var laesieIcoon2laesie = new Map(); 
var icoon2laesieImg = new Map();

var laesieImg2nummerDiv = new Map();

var draggedLaesieImg;
var laesieMouseOffset;
var draggedLaesieImgSize; 
var laesiesAlleenInzien = []; 

function initialiseerLaesieLijsten(lezingId) {
	laesies[lezingId] = {RECHTER_BORST: legeLaesielijstenVanEenBorst(), LINKER_BORST: legeLaesielijstenVanEenBorst()};
}

function clearLaesies(lezingId) {
    for (let entry of laesieImg2lezingId[Symbol.iterator]()) {
        let entryLaesieImg = entry[0];
        let entryLezingId = entry[1];
        if (entryLezingId === lezingId) {
            removeLaesie(lezingId, entryLaesieImg);
        }
    }
    initialiseerLaesieLijsten(lezingId);
}

function legeLaesielijstenVanEenBorst() {
    return {
        ARCHITECTUURVERSTORING: [],
        ASYMMETRIE: [],
        CALCIFICATIES: [],
        MASSA: [],
        LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES: [],
        LEGACY_MASSA_MET_ARCHITECTUURVERSTORING: [],
        LEGACY_CONFORM: [],
        LEGACY_GEEN_BIJZONDERHEDEN: [],
        LEGACY_MASSA_MET_SPICULAE: [],
        LEGACY_PROJECTIE_NAAR_LINKS: [],
        LEGACY_PROJECTIE_NAAR_RECHTS: [],
        LEGACY_MASSA_MET_CALCIFICATIES: [],
        LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES: [],
        LEGACY_MARKERING: [],
        LEGACY_BENIGNE_KALK: [],
    };
}

function registreerLaesieEnIcoon(img, icoon, laesie, lezingId) {
    laesieImg2icoon.set(img, icoon);
    laesieIcoon2laesie.set(icoon, laesie);
    laesieImg2lezingId.set(img, lezingId);
    icoon2laesieImg.set(icoon, img);
}

function createLaesieIcoon(img, welkeBorst, welkeDoorsnede, lezingId) {
    const laesietype = src2laesietype(img);

    let laesie = laesies[lezingId][welkeBorst][laesietype].find(laesie => !laesie[welkeDoorsnede]);
    if (!laesie) {
        laesie = {laesietype, welkeBorst};
        laesies[lezingId][welkeBorst][laesietype].push(laesie);
    }
    const icoon = px2dbCoordinates(img.offsetLeft, img.offsetTop, img);
    registreerLaesieEnIcoon(img, icoon, laesie, lezingId);
    laesie[welkeDoorsnede] = icoon;
    return icoon;
}

function removeLaesie(lezingId, laesieImg) {
    const laesieIcoon = laesieImg2icoon.get(laesieImg);
    const laesie = laesieIcoon2laesie.get(laesieIcoon);
    const nummerDiv = laesieImg2nummerDiv.get(laesieImg);
    removeLaesieIcoon(laesieImg);
    const parentDiv = laesieImg.parentElement;
    parentDiv.removeChild(laesieImg);
    parentDiv.removeChild(nummerDiv);
    laesieImg2nummerDiv.delete(laesieImg);
    laesieImg2lezingId.delete(laesieImg);
    const laesiesVanZelfdeBorstEnType = laesies[lezingId][laesie.welkeBorst][laesie.laesietype];
    hernummerLaesies(laesiesVanZelfdeBorstEnType);
}

function removeLaesieIcoon(img) {
	const icoon = laesieImg2icoon.get(img);
	const laesie = laesieIcoon2laesie.get(icoon);
	const lezingId = laesieImg2lezingId.get(img);
	const welkeDoorsnede = icoon2welkeDoorsnede(icoon, laesie);
	laesieIcoon2laesie.delete(icoon);
	laesieImg2icoon.delete(img);
	if (laesie[andereDoorsnede(welkeDoorsnede)]) {
		laesie[welkeDoorsnede] = undefined;
	} else {
		const laesiesVanDezeBorstEnType = laesies[lezingId][laesie.welkeBorst][laesie.laesietype];
		laesiesVanDezeBorstEnType.splice(laesiesVanDezeBorstEnType.indexOf(laesie), 1);
	}
}

function src2laesietype(img) {
    const withoutExtension = img.src.replace('.svg', '');
    return withoutExtension.substring(withoutExtension.lastIndexOf('/') + 1);
}

function andereDoorsnede(welkeDoorsnede) {
    if (welkeDoorsnede === 'verticaleDoorsnede') {
        return 'horizontaleDoorsnede';
    } else if (welkeDoorsnede === 'horizontaleDoorsnede') {
        return 'verticaleDoorsnede';
    } else {
        throw 'Error - onbekende doorsnede: ' + welkeDoorsnede;
    }
}

function icoon2welkeDoorsnede(icoon, laesie) {
    if (laesie.verticaleDoorsnede === icoon) {
        return 'verticaleDoorsnede';
    } else if (laesie.horizontaleDoorsnede === icoon) {
        return 'horizontaleDoorsnede';
    } else {
        throw 'Error - icoon hoort niet bij laesie';
    }
}

function backgroundImgLayout(backgroundImg) {
    var result = {};
    result.paddingLeft = valueOr0(backgroundImg.style.paddingLeft);
    result.paddingTop = valueOr0(backgroundImg.style.paddingTop);
    result.paddingRight = valueOr0(backgroundImg.style.paddingRight);
    result.paddingBottom = valueOr0(backgroundImg.style.paddingBottom);
    result.width = backgroundImg.width;
    result.height = backgroundImg.height;
    return result;

    function valueOr0(value) {
        return value ? parseInt(value) : 0;
    }
}

function px2dbCoordinates(xPx, yPx, laesieImg) {
    const backgroundImg = laesieImg.parentNode.children[0];
	const bgImgLayout = backgroundImgLayout(backgroundImg);
    const xPxCenter = xPx - bgImgLayout.paddingLeft + laesieImg.width / 2;
    const yPxCenter = yPx - bgImgLayout.paddingTop + laesieImg.height / 2;
    return {x: 100 * xPxCenter / bgImgLayout.width, y: 100 * yPxCenter / bgImgLayout.width};
}

function db2pxCoordinates(xDb, yDb, laesieImg) {
    const backgroundImg = laesieImg.parentElement.children[0];
    const bgImgLayout = backgroundImgLayout(backgroundImg);
    return {x: xDb * bgImgLayout.width / 100 + bgImgLayout.paddingLeft - laesieImg.width / 2, y: yDb * bgImgLayout.width / 100 + bgImgLayout.paddingTop - laesieImg.height / 2};
}

function moetLaesieIcoonZichtbaarZijn(icoon, backgroundImg) {
    return icoon.x >= 0 && icoon.x <= 100 && icoon.y >= 0 && icoon.y <= 100 * backgroundImg.height / backgroundImg.width;
}
