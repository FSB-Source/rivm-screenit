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
function Rij(naam, svg, kolommen,  soortUitnodiging)
{
	const maxCdvIndex = 100;
	var svgTextsMetAantal = [];

	this.getNaam = function()
	{
		return naam;
	}

	this.maakCellen = function(vanafX, y, kolombreedte)
	{
		var x = vanafX;
		for ( var kolomIndex in kolommen)
		{
            svgTextsMetAantal[kolomIndex] = createText(svg, x, y, '');
            svgTextsMetAantal[kolomIndex].setAttribute('text-anchor', 'end');

			x += kolombreedte;
		}
	}

	this.updateCellen = function(afspraakDrempel)
	{
		for (var kolomIndex in kolommen)
		{
            const aantal = getAantal(afspraakDrempel, kolommen[kolomIndex]);
            svgTextsMetAantal[kolomIndex].textContent = aantal.toLocaleString('nl');
            svgTextsMetAantal[kolomIndex].setAttribute('id', this.getNaam() + '.' + kolommen[kolomIndex].naam)
		}
	}

    function getAantal(afspraakDrempel, kolom)
    {
        var totaal = kolom.cumulatieveDeelnamekansVerdeling[maxCdvIndex];
        if (soortUitnodiging == 'totaal')
        {
            return totaal;
        }

        if(kolom.cumulatieveDeelnamekansVerdelingDrempelToepassen)
        {
            var afspraak = null;

            var totaalDrempelToepassen = kolom.cumulatieveDeelnamekansVerdelingDrempelToepassen[maxCdvIndex];
            if (afspraakDrempel == 0)
            {
                afspraak = totaalDrempelToepassen;
            }
            else
            {
                var openDrempelToepassen = kolom.cumulatieveDeelnamekansVerdelingDrempelToepassen[afspraakDrempel - 1];
                afspraak = totaalDrempelToepassen - openDrempelToepassen;
            }

            switch (soortUitnodiging)
            {
                case 'afspraak':
                    return afspraak;
                case 'open':
                    return totaal - afspraak;
            }
        }

        return '';
    }
}
