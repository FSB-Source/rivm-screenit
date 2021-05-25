package nl.rivm.screenit.service.cervix.enums;

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

public enum CervixVervolgTekst
{

	UITSTRIJKJE_REGISTREER_ONTVANGST(""),

	UITSTRIJKJE_ONTVANGEN_NAAR_HPV("vervolgstap-naar-hpv"),

	UITSTRIJKJE_GEANALYSEERD_OP_HPV_POGING_1_ONGELDIG_NAAR_HPV("vervolgstap-naar-hpv"),

	UITSTRIJKJE_GEANALYSEERD_OP_HPV_POGING_2_ONGELDIG_VERNIETIG("vervolgstap-vernietig"),

	UITSTRIJKJE_HPV_POSITIEF_NAAR_CYTOLOGIE("vervolgstap-naar-cytologie"),

	UITSTRIJKJE_REEDS_HPV_UITSLAG_NAAR_CYTOLOGIE("vervolgstap-naar-cytologie"),

	UITSTRIJKJE_VERVOLGONDERZOEK_NAAR_CYTOLOGIE("vervolgstap-naar-cytologie"),

	UITSTRIJKJE_NIET_ANALYSEERBAAR("vervolgstap-vernietig"),

	UITSTRIJKJE_REEDS_HPV_UITSLAG_VERNIETIG("vervolgstap-vernietig"),

	UITSTRIJKJE_REEDS_CYTOLOGIE_UITSLAG_VERNIETIG("vervolgstap-vernietig"),

	UITSTRIJKJE_HPV_NEGATIEF_BEWAAR("vervolgstap-bewaar"),

	UITSTRIJKJE_CYTOLOGIE_UITSLAG_BEWAAR("vervolgstap-bewaar"),

	UITSTRIJKJE_HPV_POSITIEF_CYTOLOGIE_ONBEOORDEELBAAR_BEWAAR("vervolgstap-bewaar"),

	UITSTRIJKJE_CYTOLOGIE_ONBEOORDEELBAAR_VERNIETIG("vervolgstap-vernietig"),

	UITSTRIJKJE_CLIENT_REEDS_GEINFORMEERD_BEWAAR("vervolgstap-bewaar"),

	UITSTRIJKJE_CLIENT_REEDS_GEINFORMEERD_VERNIETIG("vervolgstap-vernietig"),

	ZAS_REGISTREER_ONTVANGST(""),

	ZAS_ONTVANGEN_NAAR_HPV("vervolgstap-naar-hpv"),

	ZAS_GEANALYSEERD_OP_HPV_POGING_1_ONGELDIG_NAAR_HPV("vervolgstap-naar-hpv"),

	ZAS_GEANALYSEERD_OP_HPV_POGING_2_ONGELDIG_VERNIETIG("vervolgstap-vernietig"),

	ZAS_NIET_ANALYSEERBAAR_VERNIETIG("vervolgstap-vernietig"),

	ZAS_NIET_HOUDBAAR_VERNIETIG("vervolgstap-vernietig"),

	ZAS_REEDS_HPV_UITSLAG_VERNIETIG("vervolgstap-vernietig"),

	ZAS_HPV_POSITIEF_BEWAAR("vervolgstap-bewaar-zas-positief"),

	ZAS_HPV_NEGATIEF_BEWAAR("vervolgstap-bewaar-zas-negatief"),

	ZAS_HPV_POSITIEF_CLIENT_REEDS_GEINFORMEERD_BEWAAR("vervolgstap-bewaar-zas-positief"),

	ZAS_HPV_NEGATIEF_CLIENT_REEDS_GEINFORMEERD_BEWAAR("vervolgstap-bewaar-zas-negatief"),

	ZAS_CLIENT_REEDS_GEINFORMEERD_VERNIETIG("vervolgstap-vernietig");

	private String cssClass;

	CervixVervolgTekst(String cssClass)
	{
		this.cssClass = cssClass;
	}

	public String getCssClass()
	{
		return cssClass;
	}
}
