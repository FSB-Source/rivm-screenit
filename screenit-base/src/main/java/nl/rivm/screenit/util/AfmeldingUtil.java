package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.ScreeningRonde;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public final class AfmeldingUtil
{

	public static boolean isEenmaligOfDefinitefAfgemeld(Dossier<?, ?> dossier)
	{
		boolean isAfgemeld = false;

		if (isAfgerondeDefinitieveAfmelding(dossier.getLaatsteAfmelding()))
		{
			isAfgemeld = true;
		}
		ScreeningRonde laatsteScreeningsRonde = dossier.getLaatsteScreeningRonde();

		if (laatsteScreeningsRonde != null &&
			(isAfgerondeEenmaligeAfmelding(laatsteScreeningsRonde.getLaatsteAfmelding())
				|| isAfgerondeTijdelijkeAfmelding(laatsteScreeningsRonde.getLaatsteAfmelding())))
		{
			isAfgemeld = true;
		}
		return isAfgemeld;
	}

	public static boolean isAfgerondeDefinitieveAfmelding(Afmelding<?, ?, ?> afmelding)
	{
		return isAfgerondeAfmelding(afmelding, AfmeldingType.DEFINITIEF);
	}

	public static boolean isAangevraagdeDefinitieveAfmelding(Afmelding<?, ?, ?> afmelding)
	{
		return isAangevraagdeAfmelding(afmelding, AfmeldingType.DEFINITIEF);
	}

	public static boolean isAfgerondeEenmaligeAfmelding(Afmelding<?, ?, ?> afmelding)
	{
		return isAfgerondeAfmelding(afmelding, AfmeldingType.EENMALIG);
	}

	public static boolean isAfgerondeTijdelijkeAfmelding(Afmelding<?, ?, ?> afmelding)
	{
		return isAfgerondeAfmelding(afmelding, AfmeldingType.TIJDELIJK);
	}

	public static boolean isAangevraagdeTijdelijkeAfmelding(Afmelding<?, ?, ?> afmelding)
	{
		return isAangevraagdeAfmelding(afmelding, AfmeldingType.TIJDELIJK);
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

	public static <A extends Afmelding<S, D, ?>, S extends ScreeningRonde<D, ?, A, ?>, D extends Dossier<S, A>> A getLaatsteAfmelding(S screeningRonde, D dossier)
	{
		if (Boolean.FALSE.equals(dossier.getAangemeld()))
		{
			var afmelding = dossier.getLaatsteAfmelding();
			if (afmelding == null)
			{
				afmelding = screeningRonde.getLaatsteAfmelding();
			}
			return afmelding;
		}
		else
		{
			return screeningRonde.getLaatsteAfmelding();
		}
	}

	public static <A extends Afmelding<S, ?, ?>, S extends ScreeningRonde<?, ?, A, ?>> A getHeraanmeldbareAfmelding(Dossier<S, A> dossier)
	{
		if (DossierStatus.INACTIEF.equals(dossier.getStatus()))
		{
			var definitieveAfmeldingen = dossier.getAfmeldingen();
			for (A afmelding : definitieveAfmeldingen)
			{
				if (isAfmeldingVerwerktNietHeraangemeld(afmelding))
				{
					return afmelding;
				}
			}
			var eenmaligOfTijdelijkeAfmeldingen = dossier.getLaatsteScreeningRonde().getAfmeldingen();
			for (A afmelding : eenmaligOfTijdelijkeAfmeldingen)
			{
				if (isAfgerondeAfmeldingNietHeraangemeldEnJuisteType(afmelding, AfmeldingType.TIJDELIJK))
				{
					return afmelding;
				}
			}
		}
		else if (DossierStatus.ACTIEF.equals(dossier.getStatus()) && dossier.getLaatsteScreeningRonde() != null)
		{
			for (A afmelding : dossier.getLaatsteScreeningRonde().getAfmeldingen())
			{
				if (isAfgerondeAfmeldingNietHeraangemeldEnJuisteType(afmelding, AfmeldingType.EENMALIG))
				{
					return afmelding;
				}
				if (isAangevraagdeTijdelijkeAfmelding(afmelding))
				{
					return afmelding;
				}
			}

		}
		throw new IllegalStateException("DossierStatus is onbekend of een ongeldige status.");
	}

	public static <D extends Dossier<S, A>, A extends Afmelding<S, D, ?>, S extends ScreeningRonde<D, ?, A, ?>>
	A getLaatsteDefinitieveOfTijdelijkeHeraangemeldeAfmelding(D dossier)
	{
		var laatsteDefitieveAfmelding = dossier.getLaatsteAfmelding();

		A laatsteTijdelijkeAfmelding = null;
		var laatsteRonde = dossier.getLaatsteScreeningRonde();
		if (laatsteRonde != null)
		{
			var afmeldingen = laatsteRonde.getAfmeldingen();
			for (int i = afmeldingen.size(); i-- > 0; )
			{
				var afmelding = afmeldingen.get(i);
				if (AfmeldingType.TIJDELIJK == afmelding.getType())
				{
					laatsteTijdelijkeAfmelding = afmelding;
					break;
				}
			}
		}

		if (laatsteDefitieveAfmelding != null && laatsteTijdelijkeAfmelding != null)
		{
			return laatsteDefitieveAfmelding.getAfmeldDatum().before(laatsteTijdelijkeAfmelding.getAfmeldDatum()) ? laatsteTijdelijkeAfmelding : laatsteDefitieveAfmelding;
		}
		else if (laatsteTijdelijkeAfmelding != null)
		{
			return laatsteTijdelijkeAfmelding;
		}
		else if (laatsteDefitieveAfmelding != null)
		{
			return laatsteDefitieveAfmelding;
		}
		throw new IllegalStateException("Geen heraangemelde afmelding gevonden.");
	}

	private static boolean isAfgerondeAfmelding(Afmelding<?, ?, ?> afmelding, AfmeldingType type)
	{
		return afmelding != null && isAfgerondeAfmeldingNietHeraangemeldEnJuisteType(afmelding, type)
			&& afmelding.getHeraanmeldStatus() == null;
	}

	private static boolean isAfgerondeAfmeldingNietHeraangemeldEnJuisteType(Afmelding<?, ?, ?> afmelding, AfmeldingType gewensteType)
	{
		return isAfmeldingVerwerktNietHeraangemeld(afmelding) && afmelding.getType() == gewensteType;
	}

	private static boolean isAfmeldingVerwerktNietHeraangemeld(Afmelding<?, ?, ?> afmelding)
	{
		return afmelding.getAfmeldingStatus() == AanvraagBriefStatus.VERWERKT && afmelding.getHeraanmeldStatus() != AanvraagBriefStatus.VERWERKT;
	}

	private static boolean isAangevraagdeAfmelding(Afmelding<?, ?, ?> afmelding, AfmeldingType type)
	{
		return afmelding != null && afmelding.getType() == type && afmelding.getAfmeldingStatus() == AanvraagBriefStatus.BRIEF;
	}
}
