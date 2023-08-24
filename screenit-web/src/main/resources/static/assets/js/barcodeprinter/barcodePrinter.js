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
var printer = undefined;
var errorCallback = undefined;

function zoekPrinter(callback) {
    BrowserPrint.getDefaultDevice("printer", function (gevondenPrinter) {
        printer = gevondenPrinter;

        if (!gevondenPrinter) {
            BrowserPrint.getLocalDevices(function (device_list) {
                for (var i = 0; i < device_list.length; i++) {
                    var device = device_list[i];
                    if (!selected_device || device.uid !== selected_device.uid) {
                        printer = device;
                        if (callback) {
                            callback();
                        }
                    }
                }

            }, function () {
                foutMelding('Fout bij zoeken naar Zebra printer')
            }, "printer");
        }

    }, function () {
        foutMelding('Kon niet verbinden met de Zebra printer')
    });
}

function printBarcode(bsn, monsterId) {
    if (!printer) {
        zoekPrinter(() => {
            printBarcode(bsn, monsterId)
        })
        return;
    }
    printer.send("" +
        "^XA\n" +
        "^LH0,0\n" +
        "^FO50,110^A0B,40,40^FD" + bsn + "^FS\n" +
        "^FO125,45\n" +
        "^BY3,,450\n" +
        "^BCB,,Y,N,N,A\n" +
        "^FD" + monsterId + "^FS\n" +
        "^XZ", undefined,
        () => foutMelding('Fout bij afdrukken op Zebra printer'))
}

function printControleMonsterBarcodes(uniekeIds) {
    if (!printer) {
        zoekPrinter(() => {
            printControleMonsterBarcodes(uniekeIds);
        })
        return;
    }
    uniekeIds.forEach(id => {
        printBarcode('Controlemonster', id);
    })
}

function foutMelding(bericht) {
    console.log('Zebra print error: ' + bericht)
    if (errorCallback) {
        errorCallback(bericht);
    }
}
