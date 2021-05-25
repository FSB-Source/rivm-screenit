/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
var dataTransfer = {}; 

function dragstart(ev) {
    dataTransfer.src = ev.target.src;
    dataTransfer.deltaX = ev.offsetX; 
    dataTransfer.deltaY = ev.offsetY; 
    ev.dataTransfer.effectAllowed = 'move';
}

function preventDragStart(ev) {
    ev.preventDefault();
}

function drop(ev, tgt, welkeBorst, welkeDoorsnede) {
    const isAmputatie = tgt.getAttribute("x-amputatie");
    const lezingId = getLezingId(tgt);
    if(isAmputatie !== "true") {
        if (!laesiesAlleenInzien[lezingId]) {
            let xPx = parseInt(ev.offsetX - dataTransfer.deltaX + 2);
            let yPx = parseInt(ev.offsetY - dataTransfer.deltaY + 2);
            if (ev.target.className === 'laesieImg') { 
                xPx += parseInt(ev.target.style.left.split('px')[0]);
                yPx += parseInt(ev.target.style.top.split('px')[0]);
            }
            const src = dataTransfer.src;
            const laesieImg = createLaesieImg(src, tgt, false);
            laesieImg.style.left = xPx + 'px';
            laesieImg.style.top = yPx + 'px';
            const laesieIcoon = createLaesieIcoon(laesieImg, welkeBorst, welkeDoorsnede, lezingId);
            const laesie = laesieIcoon2laesie.get(laesieIcoon);
            createNummerDiv(laesieImg, lezingId, false);
            const laesiesVanZelfdeBorstEnType = laesies[lezingId][laesie.welkeBorst][laesie.laesietype];
            hernummerLaesies(laesiesVanZelfdeBorstEnType);
            laesieIconenOpslaan(lezingId);
        }
    }
    ev.preventDefault();
}

function dragover(ev) {
    ev.preventDefault(); 
}

function mouseDown(ev) {
    const lezingId = getLezingId(ev.target);
    if (!laesiesAlleenInzien[lezingId]) {
        draggedLaesieImg = ev.target;
        draggedLaesieImgSize = {width: draggedLaesieImg.offsetWidth, height: draggedLaesieImg.offsetHeight};
        laesieMouseOffset = {x: ev.clientX - parseInt(draggedLaesieImg.style.left, 10), y: ev.clientY - parseInt(draggedLaesieImg.style.top, 10)};
    }
}

function mouseUpLaesie() {
    if (draggedLaesieImg) {
        const lezingId = getLezingId(draggedLaesieImg);
        finishDragExistingLeasieIcoon(lezingId);
    }
}

function finishDragExistingLeasieIcoon(lezingId) {
    if (draggedLaesieImg.style.display === 'none') { 
        removeLaesie(lezingId, draggedLaesieImg);
    }
    draggedLaesieImg = undefined;
    laesieMouseOffset = undefined;
    draggedLaesieImgSize = undefined;
    laesieIconenOpslaan(lezingId);
}

function mouseMoveLaesie(ev) {
    if (draggedLaesieImg) {
        const lezingId = getLezingId(draggedLaesieImg);
        if (ev.buttons) {
            const xPx = ev.clientX - laesieMouseOffset.x;
            draggedLaesieImg.style.left = xPx + 'px';
            const yPx = ev.clientY - laesieMouseOffset.y;
            draggedLaesieImg.style.top = yPx + 'px';
            const dbCoordinates = px2dbCoordinates(xPx, yPx, draggedLaesieImg);
            const icoon = laesieImg2icoon.get(draggedLaesieImg);
            icoon.x = dbCoordinates.x;
            icoon.y = dbCoordinates.y;
            const backgroundImg = draggedLaesieImg.parentElement.children[0];
            draggedLaesieImg.style.display = moetLaesieIcoonZichtbaarZijn(icoon, backgroundImg) ? 'block' : 'none';
            setLaesienummerpositie(icoon, draggedLaesieImg);
        } else {
            finishDragExistingLeasieIcoon(lezingId);
        }
    }
}

function getLezingId(target) {
    return target.closest(".afbeeldingPanel").getAttribute("x-lezing-id");
}
