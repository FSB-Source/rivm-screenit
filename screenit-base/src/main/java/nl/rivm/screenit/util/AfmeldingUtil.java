package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.colon.ColonAfmelding;

public class AfmeldingUtil
{

	private AfmeldingUtil()
	{
	}

	public static boolean isAfgemeld(Dossier dossier)
	{
		boolean isAfgemeld = false;

		if (isActieveDefinitieveAfmelding(dossier.getLaatsteAfmelding()))
		{
			isAfgemeld = true;
		}
		ScreeningRonde laatsteScreeningsRonde = dossier.getLaatsteScreeningRonde();

		if (laatsteScreeningsRonde != null && isActieveEenmaligeAfmelding(laatsteScreeningsRonde.getLaatsteAfmelding()))
		{
			isAfgemeld = true;
		}
		return isAfgemeld;
	}

	public static boolean isActieveDefinitieveAfmelding(Afmelding afmelding)
	{
		return afmelding != null && afmelding.getAfmeldingStatus().equals(AanvraagBriefStatus.VERWERKT) && afmelding.getType() == AfmeldingType.DEFINITIEF
			&& afmelding.getHeraanmeldDatum() == null && afmelding.getHeraanmeldStatus() != AanvraagBriefStatus.VERWERKT;
	}

	public static boolean isActieveEenmaligeAfmelding(Afmelding afmelding)
	{
		return afmelding != null && afmelding.getHeraanmeldDatum() == null && afmelding.getHeraanmeldStatus() == null && afmelding.getType() == AfmeldingType.EENMALIG;
	}

	public static ColonAfmelding getOpEenNaLaatsteAfmelding(List<ColonAfmelding> afmeldingen, ColonAfmelding withoutAfmelding)
	{
		ColonAfmelding eenNaLaatsteAfmelding = null;
		for (ColonAfmelding afmelding : afmeldingen)
		{
			if ((eenNaLaatsteAfmelding == null || DateUtil.compareAfter(eenNaLaatsteAfmelding.getAfmeldDatum(), afmelding.getAfmeldDatum()))
				&& !afmelding.getId().equals(withoutAfmelding.getId()))
			{
				eenNaLaatsteAfmelding = afmelding;
			}
		}
		return eenNaLaatsteAfmelding;
	}

	public static Client getClientFromAfmelding(Afmelding afmelding)
	{
		if (AfmeldingType.DEFINITIEF.equals(afmelding.getType()))
		{
			return afmelding.getDossier().getClient();
		}
		else
		{
			return afmelding.getScreeningRonde().getDossier().getClient();
		}
	}

	public static <A extends Afmelding<?, ?, ?>> A getLaatsteAfmelding(ScreeningRonde<?, ?, A, ?> screeningRonde, Dossier<?, A> dossier)
	{
		A afmelding;

		if (Boolean.FALSE.equals(dossier.getAangemeld()))
		{
			afmelding = dossier.getLaatsteAfmelding();
		}
		else
		{
			afmelding = screeningRonde.getLaatsteAfmelding();
		}
		return afmelding;
	}

	public static <A extends Afmelding<?, ?, ?>,S extends ScreeningRonde<?, ?, A, ?>> A getVerwerkteAfmelding(Dossier<S, A> dossier)
	{
		if (DossierStatus.INACTIEF.equals(dossier.getStatus()))
		{
			List<A> dossierAfmeldingen = dossier.getAfmeldingen();
			for (A afmelding : dossierAfmeldingen)
			{
				if (AanvraagBriefStatus.VERWERKT.equals(afmelding.getAfmeldingStatus()) && afmelding.getHeraanmeldStatus() != AanvraagBriefStatus.VERWERKT)
				{
					return afmelding;
				}
			}
		}
		else if (DossierStatus.ACTIEF.equals(dossier.getStatus()))
		{
			if (dossier.getLaatsteScreeningRonde() != null)
			{
				for (A afmelding : dossier.getLaatsteScreeningRonde().getAfmeldingen())
				{
					if (AanvraagBriefStatus.VERWERKT.equals(afmelding.getAfmeldingStatus()) && AfmeldingType.EENMALIG.equals(afmelding.getType())
						&& !AanvraagBriefStatus.VERWERKT.equals(afmelding.getHeraanmeldStatus()))
					{
						return afmelding;
					}
				}
			}
		}
		throw new IllegalStateException("DossierStatus is onbekend of een ongeldige status.");
	}
}
