package nl.rivm.screenit.model.berichten.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

public enum BerichtType
{
	MDL_VERSLAG(
		"MDL-bericht",
		Bevolkingsonderzoek.COLON,
		VerslagType.MDL,
		LogGebeurtenis.BERICHT_ONTVANGEN,
		LogGebeurtenis.BERICHT_VERWERKT,
		LogGebeurtenis.BERICHT_VERWERKT_MET_ERROR,
		LogGebeurtenis.BERICHT_VERWERKT_MET_MELDING,
		LogGebeurtenis.BERICHT_MET_ZELFDE_ID,
		LogGebeurtenis.BERICHT_MET_ZELFDE_SETID_VERSIE,
		LogGebeurtenis.BERICHT_ONBEKENDE_BSN,
		LogGebeurtenis.BERICHT_ONDANKS_BEZWAAR),

	PA_LAB_VERSLAG(
		"Pathologie-bericht",
		Bevolkingsonderzoek.COLON,
		VerslagType.PA_LAB,
		LogGebeurtenis.BERICHT_ONTVANGEN,
		LogGebeurtenis.BERICHT_VERWERKT,
		LogGebeurtenis.BERICHT_VERWERKT_MET_ERROR,
		LogGebeurtenis.BERICHT_VERWERKT_MET_MELDING,
		LogGebeurtenis.BERICHT_MET_ZELFDE_ID,
		LogGebeurtenis.BERICHT_MET_ZELFDE_SETID_VERSIE,
		LogGebeurtenis.BERICHT_ONBEKENDE_BSN,
		LogGebeurtenis.BERICHT_ONDANKS_BEZWAAR),

	CERVIX_CYTOLOGIE_VERSLAG(
		"BMHK cytologie-bericht",
		Bevolkingsonderzoek.CERVIX,
		VerslagType.CERVIX_CYTOLOGIE,
		LogGebeurtenis.CERVIX_BERICHT_ONTVANGEN,
		LogGebeurtenis.CERVIX_VERSLAG_CYTOLOGIE_VERWERKT,
		LogGebeurtenis.CERVIX_VERSLAG_CYTOLOGIE_VERWERKT_MET_ERROR,
		LogGebeurtenis.CERVIX_VERSLAG_CYTOLOGIE_VERWERKT_MET_MELDINGEN,
		LogGebeurtenis.CERVIX_BERICHT_MET_ZELFDE_ID,
		LogGebeurtenis.CERVIX_BERICHT_MET_ZELFDE_SETID_VERSIE,
		LogGebeurtenis.CERVIX_BERICHT_ONBEKENDE_BSN,
		null),

	MAMMA_PA_FOLLOW_UP_VERSLAG(
		"BK follow-up-bericht",
		Bevolkingsonderzoek.MAMMA,
		VerslagType.MAMMA_PA_FOLLOW_UP,
		LogGebeurtenis.MAMMA_BERICHT_ONTVANGEN,
		LogGebeurtenis.MAMMA_VERSLAG_FOLLOW_UP_VERWERKT,
		LogGebeurtenis.MAMMA_VERSLAG_FOLLOW_UP_VERWERKT_MET_ERROR,
		LogGebeurtenis.MAMMA_VERSLAG_FOLLOW_UP_VERWERKT_MET_MELDING,
		LogGebeurtenis.MAMMA_BERICHT_MET_ZELFDE_ID,
		LogGebeurtenis.MAMMA_BERICHT_MET_ZELFDE_SETID_VERSIE,
		LogGebeurtenis.MAMMA_BERICHT_ONBEKENDE_BSN,
		null);

	private final String naam;

	private final Bevolkingsonderzoek bevolkingsonderzoek;

	private final VerslagType verslagType;

	private final LogGebeurtenis lbBerichtOntvangen;

	private final LogGebeurtenis lbBerichtVerwerkt;

	private final LogGebeurtenis lbBerichtVerwerktMetError;

	private final LogGebeurtenis lbBerichtVerwerktMetMelding;

	private final LogGebeurtenis lbBerichtZelfdeId;

	private final LogGebeurtenis lbBerichtZelfdeSetIdEnVersie;

	private final LogGebeurtenis lbOnbekendeBsn;

	private final LogGebeurtenis lbOndanksBezwaar;

	private BerichtType(String naam, Bevolkingsonderzoek bevolkingsonderzoek, VerslagType verslagType, LogGebeurtenis lbBerichtOntvangen, LogGebeurtenis lbBerichtVerwerkt,
		LogGebeurtenis lbBerichtVerwerktMetError, LogGebeurtenis lbBerichtVerwerktMetMelding, LogGebeurtenis lbBerichtZelfdeId, LogGebeurtenis lbBerichtZelfdeSetIdEnVersie,
		LogGebeurtenis lbOnbekendeBsn, LogGebeurtenis lbOndanksBezwaar)
	{
		this.naam = naam;
		this.bevolkingsonderzoek = bevolkingsonderzoek;
		this.verslagType = verslagType;
		this.lbBerichtOntvangen = lbBerichtOntvangen;
		this.lbBerichtVerwerkt = lbBerichtVerwerkt;
		this.lbBerichtVerwerktMetError = lbBerichtVerwerktMetError;
		this.lbBerichtVerwerktMetMelding = lbBerichtVerwerktMetMelding;
		this.lbBerichtZelfdeId = lbBerichtZelfdeId;
		this.lbBerichtZelfdeSetIdEnVersie = lbBerichtZelfdeSetIdEnVersie;
		this.lbOnbekendeBsn = lbOnbekendeBsn;
		this.lbOndanksBezwaar = lbOndanksBezwaar;
	}

	public String getNaam()
	{
		return this.naam;
	}

	public Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return bevolkingsonderzoek;
	}

	public VerslagType getVerslagType()
	{
		return verslagType;
	}

	public LogGebeurtenis getLbBerichtOntvangen()
	{
		return lbBerichtOntvangen;
	}

	public LogGebeurtenis getLbBerichtVerwerkt()
	{
		return lbBerichtVerwerkt;
	}

	public LogGebeurtenis getLbBerichtVerwerktMetError()
	{
		return lbBerichtVerwerktMetError;
	}

	public LogGebeurtenis getLbBerichtVerwerktMetMelding()
	{
		return lbBerichtVerwerktMetMelding;
	}

	public LogGebeurtenis getLbBerichtZelfdeId()
	{
		return lbBerichtZelfdeId;
	}

	public LogGebeurtenis getLbBerichtZelfdeSetIdEnVersie()
	{
		return lbBerichtZelfdeSetIdEnVersie;
	}

	public LogGebeurtenis getLbOnbekendeBsn()
	{
		return lbOnbekendeBsn;
	}

	public LogGebeurtenis getLbOndanksBezwaar()
	{
		return lbOndanksBezwaar;
	}

	@Override
	public String toString()
	{
		return this.naam;
	}

}
