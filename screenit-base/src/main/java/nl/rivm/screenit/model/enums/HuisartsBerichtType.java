package nl.rivm.screenit.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.INaam;

public enum HuisartsBerichtType implements INaam
{
	ONGUNSTIGE_UITSLAG("Ongunstige uitslag en intakeafspraak", Bevolkingsonderzoek.COLON),

	WIJZIGING_INTAKEAFSPRAAK("Wijziging intakeafspraak", Bevolkingsonderzoek.COLON),

	ANNULEREN_INTAKEAFSPRAAK("Annuleren intakeafspraak", Bevolkingsonderzoek.COLON),

	NO_SHOW_INTAKE("No show intake", Bevolkingsonderzoek.COLON),

	INTAKE_NA_OPEN_UITNODIGING("Afspraak intake na open uitnodiging", Bevolkingsonderzoek.COLON),

	CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR("Uitstrijkje niet analyseerbaar", Bevolkingsonderzoek.CERVIX),

	CERVIX_UITSTRIJKJE_HPV_ONBEOORDEELBAAR("Uitstrijkje hpv onbeoordeelbaar", Bevolkingsonderzoek.CERVIX),

	CERVIX_UITSTRIJKJE_HPV_NEGATIEF("Uitstrijkje hpv negatief", Bevolkingsonderzoek.CERVIX),

	CERVIX_TWEEDE_UITSTRIJKJE_HPV_ONBEOORDEELBAAR("Tweede uitstrijkje hpv onbeoordeelbaar", Bevolkingsonderzoek.CERVIX),

	CERVIX_UITSTRIJKJE_CYTOLOGIE_UITSLAG("Uitstrijkje cytologie uitslag", Bevolkingsonderzoek.CERVIX),

	CERVIX_VERVOLGONDERZOEK_CYTOLOGIE_UITSLAG("Controle-uitstrijkje cytologie uitslag", Bevolkingsonderzoek.CERVIX),

	CERVIX_TWEEDE_UITSTRIJKJE_CYTOLOGIE_ONBEOORDEELBAAR("Tweede uitstrijkje cytologie onbeoordeelbaar", Bevolkingsonderzoek.CERVIX),

	CERVIX_UITSLAG_REEDS_BEKEND("Uitslag reeds bekend", Bevolkingsonderzoek.CERVIX),

	CERVIX_UITSTRIJKJE_ONTBREEKT("Uitstrijkje ontbreekt", Bevolkingsonderzoek.CERVIX),

	CERVIX_OMISSIE_NA_UITSTRIJKJE_HPV_POSITIEF("Omissie na uitstrijkje hpv positief", Bevolkingsonderzoek.CERVIX),

	CERVIX_ZORGMAIL_VERIFICATIE("Zorgmail klantnummer verificatiebericht", Bevolkingsonderzoek.CERVIX),

	MAMMA_VERSLAG_UITSLAG_ONGUNSTIG("Borstkanker onderzoek uitslag positief", Bevolkingsonderzoek.MAMMA),

	MAMMA_VERSLAG_UITSLAG_FOTOBESPREKING_ONGUNSTIG("Borstkanker onderzoek uitslag positief na fotobespreking", Bevolkingsonderzoek.MAMMA),

	MAMMA_PROTHESE_MEER_DAN_80_PROCENT("Borstkanker onderzoek beperkt beoordeelbaar prothese > 0.8", Bevolkingsonderzoek.MAMMA),

	MAMMA_UITSLAG_GUNSTIG_MET_NEVENBEVINDINGEN("Borstkanker onderzoek negatief met nevenbevindingen", Bevolkingsonderzoek.MAMMA);

	private String naam;

	private Bevolkingsonderzoek onderzoek;

	private HuisartsBerichtType(String naam, Bevolkingsonderzoek onderzoek)
	{
		this.naam = naam;
		this.onderzoek = onderzoek;
	}

	@Override
	public String getNaam()
	{
		return this.naam;
	}

	public Bevolkingsonderzoek getOnderzoek()
	{
		return onderzoek;
	}

	public void setOnderzoek(Bevolkingsonderzoek onderzoek)
	{
		this.onderzoek = onderzoek;
	}

	public static List<HuisartsBerichtType> getTemplateFromBevolkingsonderzoek(List<Bevolkingsonderzoek> onderzoeken)
	{
		return Arrays.stream(HuisartsBerichtType.values())
			.filter(t -> onderzoeken.contains(t.getOnderzoek()))
			.collect(Collectors.toList());
	}

}
