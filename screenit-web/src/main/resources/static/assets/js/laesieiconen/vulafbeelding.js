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
const calcLaesiePositions = () => {
    icoon2laesieImg.forEach((laesieImg, icoon) => {
        setLaesieImgPosition(icoon, laesieImg);
    });
};

window.addEventListener('resize', calcLaesiePositions);

function createLaesieImg(src, parentDiv, alleenInzien) {
	const laesieImg = document.createElement('IMG');
	laesieImg.src = src;
	laesieImg.style.position = 'absolute';
	if (!alleenInzien) {
		laesieImg.style.cursor = 'move';
	}
	laesieImg.onmousedown = mouseDown;
	laesieImg.onmouseup = mouseUpLaesie;
	laesieImg.ondragstart = preventDragStart;
	laesieImg.className = 'laesieImg';
	$(parentDiv).each(function (index) {
		$(parentDiv)[index].appendChild(laesieImg)
	});
	return laesieImg;
}

var huidigeLezingId;
var huidigeLezingIniteeleValues;
var copyHuidigeLezingInitieeleValues;

function toonLaesies(json, inzien, lezingId, opgeslagenCallback) {
    laesiesAlleenInzien[lezingId] = inzien;
    laesieIconenOpgeslagenCallbacks[lezingId] = opgeslagenCallback;
    initialiseerLaesieLijsten(lezingId);
    laadLaesies(json, lezingId);
    if (!inzien) {
        huidigeLezingId = lezingId;
        huidigeLezingIniteeleValues = JSON.stringify(laesies[lezingId]);
    }
}

function huidigeLezingGewijzigd() {
    return huidigeLezingId && huidigeLezingIniteeleValues !== JSON.stringify(laesies[huidigeLezingId]);
}

function updateHuidigeLezingInitieeleValues() {
    if (huidigeLezingId) {
        huidigeLezingIniteeleValues = JSON.stringify(laesies[huidigeLezingId]);
    }
}

function backupHuidigeLezingInitieeleValues() {
    copyHuidigeLezingInitieeleValues = huidigeLezingIniteeleValues;
}

function zetBackupHuidigeLezingInitieeleValuesTerug() {
    huidigeLezingIniteeleValues = copyHuidigeLezingInitieeleValues;
}

function neemLezingOver(sourceJson, biradsOpmerking, lezingId) {
    clearLaesies(lezingId);
    laadLaesies(sourceJson, lezingId);
    laesieIconenOpslaan(lezingId);
    getBiradsOpmerkingVeldVanLezing(lezingId).value = biradsOpmerking.replace(/"/g, '');
}

function laadLaesies(json, lezingId) {
    JSON.parse(json).forEach(laesie => toonLaesie(laesie, lezingId));
    toonLaesieNummers(lezingId);
}

function setLaesieImgPosition(icoon, laesieImg) {
    const pxCoords = db2pxCoordinates(icoon.x, icoon.y, laesieImg);
    laesieImg.style.left = pxCoords.x + 'px';
    laesieImg.style.top = pxCoords.y + 'px';
    setLaesienummerpositie(icoon, laesieImg);
}

function toonLaesie(laesie, lezingId) {
    laesies[lezingId][laesie.welkeBorst][laesie.laesietype].push(laesie);
    if (laesie.verticaleDoorsnede) {
        toonLaesieIcoon(laesie, laesie.verticaleDoorsnede, 'verticaleDoorsnede', lezingId);
    }
    if (laesie.horizontaleDoorsnede) {
        toonLaesieIcoon(laesie, laesie.horizontaleDoorsnede, 'horizontaleDoorsnede', lezingId);
    }

    function toonLaesieIcoon(laesie, icoon, welkeDoorsnede, lezingId) {
        const src = '../assets/images/mamma/iconen_beoordeling/' + laesie.laesietype + '.svg';
        const parentDiv = $('[x-lezing-id=' + lezingId + ']')[0].getElementsByClassName(laesie.welkeBorst + '_' + welkeDoorsnede)[0];
        const laesieImg = createLaesieImg(src, parentDiv, laesiesAlleenInzien[lezingId]);
        registreerLaesieEnIcoon(laesieImg, icoon, laesie, lezingId);
        const backgroundImg = laesieImg.parentElement.children[0];
        const poll = setInterval(function () { // Doe dit pas als laesieImg geladen is, omdat dan pas de afmetingen bekend zijn. Niet dmv onload, omdat dat misschien al gebeurd is:
            if (laesieImg.width && backgroundImg.width) {
                clearInterval(poll);
                setLaesieImgPosition(icoon, laesieImg);
            }
        }, 10);
    }
}

function updateNummerDiv(laesieImg) {
    const nummerDiv = laesieImg2nummerDiv.get(laesieImg);
    const laesieIcoon = laesieImg2icoon.get(laesieImg);
    const laesie = laesieIcoon2laesie.get(laesieIcoon);
    const lezingId = laesieImg2lezingId.get(laesieImg);
    setLaesienummerpositie(laesieIcoon, laesieImg);
    const laesiesVanZelfdeBorstEnType = laesies[lezingId][laesie.welkeBorst][laesie.laesietype];
    if (laesiesVanZelfdeBorstEnType.length > 1) {
        nummerDiv.innerHTML = laesiesVanZelfdeBorstEnType.indexOf(laesie) + 1;
    } else {
        nummerDiv.innerHTML = '';
    }
}

function hernummerLaesies(laesiesVanZelfdeBorstEnType) {
    for (let laesieIndex in laesiesVanZelfdeBorstEnType) {
        const laesie = laesiesVanZelfdeBorstEnType[laesieIndex];
        if (laesie['verticaleDoorsnede']) {
            updateNummerDiv(icoon2laesieImg.get(laesie['verticaleDoorsnede']));
        }
        if (laesie['horizontaleDoorsnede']) {
            updateNummerDiv(icoon2laesieImg.get(laesie['horizontaleDoorsnede']));
        }
    }
}

function setLaesienummerpositie(icoon, laesieImg) {
    const nummerDiv = laesieImg2nummerDiv.get(laesieImg);
    if (nummerDiv) {
        const pxCoordinates = db2pxCoordinates(icoon.x, icoon.y, laesieImg);
        nummerDiv.style.left = (pxCoordinates.x + laesieImg.width) + 'px';
        nummerDiv.style.top = pxCoordinates.y + 'px';
    }
}

function createNummerDiv2(laesieImg, laesiesVanZelfdeBorstEnType, laesie, parentDiv, laesieIcoon) {
    const nummerDiv = document.createElement('DIV');
    nummerDiv.style.zIndex = '9999999';
    laesieImg2nummerDiv.set(laesieImg, nummerDiv);
    if (laesiesVanZelfdeBorstEnType.length > 1) {
        nummerDiv.innerHTML = laesiesVanZelfdeBorstEnType.indexOf(laesie) + 1;
    }
    nummerDiv.style.position = 'absolute';
    nummerDiv.setAttribute('class', 'laesienummer');
    parentDiv.appendChild(nummerDiv);
    setLaesienummerpositie(laesieIcoon, laesieImg);
}

function createNummerDiv(laesieImg, lezingId, metPoller) {
    const laesieIcoon = laesieImg2icoon.get(laesieImg);
    const laesie = laesieIcoon2laesie.get(laesieIcoon);
    const parentDiv = laesieImg.parentElement;
    const backgroundImg = parentDiv.children[0];
    const laesiesVanZelfdeBorstEnType = laesies[lezingId][laesie.welkeBorst][laesie.laesietype];
    if (metPoller) {
        const poll = setInterval(function () { // Doe dit pas als laesieImg geladen is, omdat dan pas de afmetingen bekend zijn. Niet dmv onload, omdat dat misschien al gebeurd is:
            if (laesieImg.width && backgroundImg.width) {
                clearInterval(poll);
                createNummerDiv2(laesieImg, laesiesVanZelfdeBorstEnType, laesie, parentDiv, laesieIcoon);
            }
        }, 10);
    } else {
        createNummerDiv2(laesieImg, laesiesVanZelfdeBorstEnType, laesie, parentDiv, laesieIcoon);
    }
}

function toonLaesieNummers(lezingId) {
    Array.from(document.getElementsByClassName('laesieImg')).filter(laesieImg => laesieImg2lezingId.get(laesieImg) === lezingId).forEach(laesieImg => {
        createNummerDiv(laesieImg, lezingId, true);
    });
}

function getBiradsOpmerkingVeldVanLezing(lezingId) {
    return document.querySelector('[x-lezing-id="' + lezingId + '"]').closest(".afbeeldingPanel").parentNode.querySelector('.biradsopmerking');
}
