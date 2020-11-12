package nl.rivm.screenit.util.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;

public class MammaScreeningRondeUtil
{

	private MammaScreeningRondeUtil()
	{

	}

	public static MammaAfspraak getLaatsteAfspraak(MammaScreeningRonde screeningRonde)
	{
		if (screeningRonde != null && screeningRonde.getLaatsteUitnodiging() != null)
		{
			return screeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();
		}
		return null;
	}

	public static MammaOnderzoek getOnderzoekVanLaatsteAfspraak(MammaScreeningRonde screeningRonde)
	{
		MammaAfspraak afspraak = getLaatsteAfspraak(screeningRonde);
		if (afspraak != null)
		{
			return afspraak.getOnderzoek();
		}
		return null;
	}

	public static MammaOnderzoek getLaatsteOnderzoek(MammaScreeningRonde screeningRonde)
	{
		MammaOnderzoek result = null;
		if (screeningRonde != null)
		{
			for (MammaUitnodiging uitnodiging : screeningRonde.getUitnodigingen())
			{
				for (MammaAfspraak afspraak : uitnodiging.getAfspraken())
				{
					if (afspraak.getOnderzoek() != null
						&& (result == null || result.getCreatieDatum().compareTo(afspraak.getOnderzoek().getCreatieDatum()) < 0))
					{
						result = afspraak.getOnderzoek();
					}
				}
			}
		}
		return result;
	}

	public static String bepaalNaamBiradsWaarde(MammaZijde zijde, MammaBIRADSWaarde mammaBIRADSWaarde)
	{
		if (MammaZijde.RECHTER_BORST.equals(zijde))
		{
			return mammaBIRADSWaarde != null ? "R" + mammaBIRADSWaarde.getNaam() : " ";
		}
		else if (MammaZijde.LINKER_BORST.equals(zijde))
		{
			return mammaBIRADSWaarde != null ? " L" + mammaBIRADSWaarde.getNaam() : " ";
		}
		else
		{
			throw new IllegalArgumentException("De zijde " + zijde + "is ongeldig");
		}
	}

	public static MammaBeoordeling getLaatsteBeoordeling(MammaOnderzoek onderzoek)
	{
		if (onderzoek != null)
		{
			return onderzoek.getLaatsteBeoordeling();
		}
		return null;
	}

	public static MammaBeoordeling getLaatsteBeoordeling(MammaScreeningRonde ronde)
	{
		MammaOnderzoek onderzoek = getLaatsteOnderzoek(ronde);
		return getLaatsteBeoordeling(onderzoek);
	}

	public static boolean heeftActiefUitstel(MammaScreeningRonde screeningRonde)
	{
		return screeningRonde.getLaatsteUitstel() != null && screeningRonde.getLaatsteUitstel().getGeannuleerdOp() == null
			&& screeningRonde.getLaatsteUitstel().getUitnodiging() == null;
	}

	public static MammaAfspraak getAfspraakVanLaatsteOnderzoek(MammaDossier dossier)
	{
		MammaScreeningRonde laatsteScreeningRonde = dossier != null ? dossier.getLaatsteScreeningRonde() : null;
		if (laatsteScreeningRonde != null && laatsteScreeningRonde.getLaatsteOnderzoek() != null)
		{
			return laatsteScreeningRonde.getLaatsteOnderzoek().getAfspraak();
		}
		return null;
	}

	public static MammaBeoordeling getLaatsteBeoordelingVanLaatsteOnderzoek(Client client)
	{
		MammaAfspraak afspraak = getAfspraakVanLaatsteOnderzoek(client.getMammaDossier());
		return afspraak != null ? getLaatsteBeoordeling(afspraak.getOnderzoek()) : null;
	}

}
