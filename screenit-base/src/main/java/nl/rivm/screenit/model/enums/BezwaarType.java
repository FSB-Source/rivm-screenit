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

import java.util.List;

public enum BezwaarType
{
	GEEN_WETENSCHAPPELIJK_ONDERZOEK("Ik wil dat jullie mijn gegevens niet gebruiken voor wetenschappelijk onderzoek",
		"Als u deze keuze maakt, dan gebruiken we uw gegevens niet voor wetenschappelijk onderzoek. Dit geldt voor alle drie bevolkingsonderzoeken naar kanker, ook als u daar (nog) niet aan mee doet.",
		false),

	GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK(
		"Ik wil dat jullie mijn lichaamsmateriaal van het bevolkingsonderzoek baarmoederhalskanker niet gebruiken voor wetenschappelijk onderzoek",
		"Als u deze keuze maakt, wordt uw uitstrijkje of zelfafnameset niet gebruikt voor wetenschappelijk onderzoek en wordt het materiaal vernietigd.", false,
		Bevolkingsonderzoek.CERVIX),

	GEEN_KWALITEITSWAARBORGING("Ik wil dat jullie mijn gegevens niet gebruiken om de bevolkingsonderzoeken te verbeteren",
		"Als u deze keuze maakt, dan gebruiken we uw gegevens niet voor de verbetering van onze onderzoeken. Dit geldt voor alle drie bevolkingsonderzoeken naar kanker, ook als u daar (nog) niet aan mee doet.",
		false),

	GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS(
		"Ik wil dat jullie mijn uitslagen en foto’s van het bevolkingsonderzoek borstkanker niet delen met het ziekenhuis",
		"Als u deze keuze maakt, dan krijgt het ziekenhuis uw foto’s van het bevolkingsonderzoek niet. Als er verder onderzoek nodig is, maakt het ziekenhuis nieuwe foto’s. De artsen in het ziekenhuis kunnen die foto’s dan niet vergelijken met onze foto’s. En ze horen niet wat de artsen tijdens het bevolkingsonderzoek hebben gezien.",
		false,
		Bevolkingsonderzoek.MAMMA),

	GEEN_SIGNALERING_VERWIJSADVIES(
		"Ik wil dat het laboratorium niet controleert of ik na een doorverwijzing van het bevolkingsonderzoek baarmoederhalskanker naar de gynaecoloog ben gegaan",
		"Als u deze keuze maakt dan controleert het laboratorium niet of u een afspraak heeft gemaakt met een gynaecoloog (als het nodig was om u verder te onderzoeken).",
		false, Bevolkingsonderzoek.CERVIX),

	@Deprecated
	GEEN_REGISTRATIE_GEBOORTELAND("Geen registratie geboorteland", false),

	GEEN_OPNAME_UIT_BPR("Ik wil dat jullie al mijn contactgegevens en onderzoeksresultaten verwijderen"),

	@Deprecated
	GEEN_UITWISSELING_MET_DE_HUISARTS("Geen uitwisseling met de huisarts", false, Bevolkingsonderzoek.COLON),

	VERZOEK_TOT_VERWIJDERING_DOSSIER("Ik wil dat jullie de onderzoeksresultaten van mijn deelname verwijderen", Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX,
		Bevolkingsonderzoek.MAMMA),

	;

	private final String naam;

	private String subtitel = "";

	private Bevolkingsonderzoek[] bevolkingsonderzoeken = new Bevolkingsonderzoek[] {};

	private Boolean onzichtbaarOpClientPortaal = true;

	BezwaarType(String naam, Bevolkingsonderzoek... bevolkingsOnderzoeken)
	{
		this.naam = naam;
		this.bevolkingsonderzoeken = bevolkingsOnderzoeken;
	}

	BezwaarType(String naam, boolean onzichtbaarOpClientPortaal, Bevolkingsonderzoek... bevolkingsOnderzoeken)
	{
		this.naam = naam;
		this.onzichtbaarOpClientPortaal = onzichtbaarOpClientPortaal;
		this.bevolkingsonderzoeken = bevolkingsOnderzoeken;
	}

	BezwaarType(String naam, String subtitel, boolean onzichtbaarOpClientPortaal, Bevolkingsonderzoek... bevolkingsOnderzoeken)
	{
		this.naam = naam;
		this.subtitel = subtitel;
		this.onzichtbaarOpClientPortaal = onzichtbaarOpClientPortaal;
		this.bevolkingsonderzoeken = bevolkingsOnderzoeken;
	}

	BezwaarType(String naam)
	{
		this.naam = naam;
	}

	public Bevolkingsonderzoek[] getBevolkingsonderzoeken()
	{
		return bevolkingsonderzoeken;
	}

	public Boolean getOnzichtbaarOpClientPortaal()
	{
		return onzichtbaarOpClientPortaal;
	}

	public String getNaam()
	{
		return naam;
	}

	public String getSubtitel()
	{
		return subtitel;
	}

	public static final List<BezwaarType> ALGEMENE_BEZWAAR_TYPES = List.of(GEEN_WETENSCHAPPELIJK_ONDERZOEK, GEEN_KWALITEITSWAARBORGING,
		GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK, GEEN_SIGNALERING_VERWIJSADVIES, GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS);
}
