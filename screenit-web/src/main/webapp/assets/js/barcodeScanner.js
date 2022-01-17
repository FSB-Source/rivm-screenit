/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
var barcodeScanner_DetectionTimeout = null; 
var barcodeScanner_ingetypteBarcodekarakters = '';
var barcodeScanner_timerGestartOp = null;
var barcodeScanner_klaarMetBarcodeScannenOp = null;

const barcodeScanner_maxDuur = 1000; 
const barcodeScanner_maxTijdTussenBarcodeEnToetsErna = 100; 
const barcodeScanner_minimumAantalCijfersUitstrijkje = 9;
const barcodeScanner_minimumAantalKaraktersZAS = 9;

function detecteerBarcodescanner(event) {
    const keyboardEvent = event; 

    setBarcodescannerMelding('');

    if (barcodeScanner_DetectionTimeout) { 
        if (keyboardEvent.key === 'Enter') {
            if (barcodeScanner_ingetypteBarcodekarakters.length >= barcodeScanner_minimumAantalCijfersUitstrijkje || (barcodeScanner_ingetypteBarcodekarakters[0] === 'Z' && barcodeScanner_ingetypteBarcodekarakters.length >= barcodeScanner_minimumAantalKaraktersZAS)) {

                clearTimeout(barcodeScanner_DetectionTimeout);
                barcodeScanner_klaarMetBarcodeScannenOp = now();
                barcodeScanner_timerGestartOp = null;
                barcodeScanner_DetectionTimeout = null;
                barcodeScanner_ingetypteBarcodekarakters = '';

            } else {
                eenMensHeeftInHetBarcodeveldGetypt(keyboardEvent);
            }
        } else if (isCijfertoets(keyboardEvent)) {
            barcodeScanner_ingetypteBarcodekarakters += keyboardEvent.key;
        } else if (keyboardEvent.key !== 'j' || ! keyboardEvent.ctrlKey) {
            eenMensHeeftInHetBarcodeveldGetypt(keyboardEvent);
        }
    } else { 
        if (isCijfertoets(keyboardEvent) || (keyboardEvent.key === 'Z' && keyboardEvent.shiftKey)) {
            barcodeScanner_ingetypteBarcodekarakters = keyboardEvent.key;
            barcodeScanner_timerGestartOp = now();
            function handler() { 
                clearInput(keyboardEvent);
                eenMensHeeftInHetBarcodeveldGetypt(keyboardEvent);
            }
            barcodeScanner_DetectionTimeout = setTimeout(handler, barcodeScanner_maxDuur);
        } else if (barcodeScanner_klaarMetBarcodeScannenOp && (now() - barcodeScanner_klaarMetBarcodeScannenOp <= barcodeScanner_maxTijdTussenBarcodeEnToetsErna)) {
            keyboardEvent.preventDefault(); 
        } else {
            eenMensHeeftInHetBarcodeveldGetypt(keyboardEvent);
        }
    }

    function now() {
        return new Date().getTime();
    }

    function isCijfertoets(keyboardEvent) {
        return keyboardEvent.charCode >= 48 && keyboardEvent.charCode <= 57;
    }

    function eenMensHeeftInHetBarcodeveldGetypt(keyboardEvent) {
        setBarcodescannerMelding('Gebruik voor handmatige invoer deze knop.');
        keyboardEvent.preventDefault();
        barcodeScanner_DetectionTimeout = null;
        barcodeScanner_ingetypteBarcodekarakters = '';
    }

    function clearInput(keyboardEvent) {
        function handler2() { 
            keyboardEvent.target.value = '';
        }
        setTimeout(handler2, 20);
    }
}

function setBarcodescannerMelding(tekst) {
    document.getElementById('meldingBijHandmatigeInvoerInBarcodeVeld').innerText = tekst;
}
