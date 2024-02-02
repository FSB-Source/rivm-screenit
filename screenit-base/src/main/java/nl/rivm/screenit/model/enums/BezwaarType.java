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

public enum BezwaarType
{
	@Deprecated
	GEEN_REGISTRATIE_GEBOORTELAND("Geen registratie geboorteland", false),

	GEEN_OPNAME_UIT_BPR("Geen opname vanuit het BRP"),

	@Deprecated
	GEEN_UITWISSELING_MET_DE_HUISARTS("Geen uitwisseling met de huisarts", false, Bevolkingsonderzoek.COLON),

	GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS("Geen digitale uitwisseling van uitslag en beelden met ziekenhuis", false, Bevolkingsonderzoek.MAMMA),

	GEEN_KWALITEITSWAARBORGING("Geen kwaliteitswaarborging", false, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	GEEN_WETENSCHAPPELIJK_ONDERZOEK("Geen evaluatie- en wetenschappelijk onderzoek", false, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK("Geen gebruik van mijn lichaamsmateriaal voor wetenschappelijk onderzoek", false, Bevolkingsonderzoek.CERVIX),

	GEEN_SIGNALERING_VERWIJSADVIES("Geen controle vervolg verwijsadvies door laboratorium", false, Bevolkingsonderzoek.CERVIX),

	VERZOEK_TOT_VERWIJDERING_DOSSIER("Verzoek tot verwijdering van dossier", Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	;

	private final String naam;

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
}
