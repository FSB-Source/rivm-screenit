/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
function VoorspellingsgrafiekMatrix(svg, marginBottom, metadataBovenaan, toonKolommen)
{

    const top = metadataBovenaan == '' ? 0 : 20;
    const left = 0;
    const right = svg.getAttribute("width");
    const dikteLijnOnderKolomkoppen = 1;
    const tekstHoogte = 10;
    const xMetadataBovenaan = 0;
    const yMetadataBovenaan = tekstHoogte;
    const margeLinksVanTekst = 2;
    const margeBovenTekst = 2;
    const margeOnderTekst = 4;
    const breedteVoorAantallen = 50;
    const rijnamenBreedte = 75;
    const ruimteBovenBovensteRij = 2;
    const lijnKleur = '#444444';

    const kolommen = alleDeelnamekansen.kolommen.filter(function (kolom)
    {
        return toonKolommen.includes(kolom.naam);
    })

    var rijen = [
        new Rij('Totaal', svg, kolommen, 'totaal'),
        new Rij('Afspraak', svg, kolommen, 'afspraak'),
        new Rij('Open', svg, kolommen, 'open')
    ]

    const kolombreedte = (right - rijnamenBreedte) / kolommen.length;
    const rijHoogte = tekstHoogte + margeBovenTekst + margeOnderTekst;
    const hoogte = rijHoogte + rijHoogte * rijen.length;

    svg.setAttribute('height', top + hoogte + marginBottom);
    creeerMetadataBovenaan();
    creeerLijnen();
    creeerKolomkoppen();
    vulRijen();

    function creeerMetadataBovenaan() {
        createText(svg, xMetadataBovenaan, yMetadataBovenaan, metadataBovenaan);
    }

    function creeerLijnen() {
        var y = top + margeBovenTekst + tekstHoogte + margeOnderTekst;
        createLine(svg, left, right, y, y, lijnKleur, dikteLijnOnderKolomkoppen);
        var x = rijnamenBreedte;
        for (var kolomIndex in kolommen) {
            createLine(svg, x, x, top, top + hoogte, lijnKleur, 1);
            x += kolombreedte;
        }
    }

    function creeerKolomkoppen() {
        var x = rijnamenBreedte + kolombreedte / 2;
        const y = top + margeBovenTekst + tekstHoogte;

        for (var kolomIndex in kolommen) {
            var text = createText(svg, x, y, kolommen[kolomIndex].naam);
            text.setAttribute('text-anchor', 'middle');
            x += kolombreedte;
        }
    }

    function vulRijen() {
        const x = margeLinksVanTekst;
        var y = top + dikteLijnOnderKolomkoppen + ruimteBovenBovensteRij + tekstHoogte;
        for (var rijIndex in rijen) {
            y += rijHoogte;
            createText(svg, x, y, rijen[rijIndex].getNaam());
            rijen[rijIndex].maakCellen(x - margeLinksVanTekst + rijnamenBreedte + kolombreedte / 2 + breedteVoorAantallen / 2 - margeLinksVanTekst,
                y, kolombreedte);
        }
    }

    this.updateData = function(afspraakDrempel) {
        for (var rijIndex in rijen) {
            rijen[rijIndex].updateCellen(afspraakDrempel);
        }
    }
}
