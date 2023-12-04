package nl.rivm.screenit.model.dashboard;

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

import java.util.List;

import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

public enum DashboardType
{
	GBA_LANDELIJK("GBA & inactieve client", List.of(OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	COLON_BATCH_NA_GBA_VERWERKING("Darmkanker specifieke verwerking nav. GBA", Bevolkingsonderzoek.COLON),

	CLIENT_SELECTIE("Client selectie", List.of(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.COLON),

	UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM("Uitnodiging versturen naar inpakcentrum", Bevolkingsonderzoek.COLON),

	CONTROLE_TERUGKOPPELING_CLIENT_SELECTIE("Controle terugkoppeling van Inpakcentrum", Bevolkingsonderzoek.COLON),

	TERUGKOPPELING_CLIENT_SELECTIE("Terugkoppeling van Inpakcentrum", Bevolkingsonderzoek.COLON),

	RETOURZENDINGEN("Retourzendingen", Bevolkingsonderzoek.COLON),

	COLON_IFOBT_KOPPELING("FIT-berichten koppeling", List.of(OrganisatieType.RIVM, OrganisatieType.LABORATORIUM), Bevolkingsonderzoek.COLON),

	IFOBT_INLEZEN("FIT uitslagen ontvangen", Bevolkingsonderzoek.COLON),

	IFOBT_VERWERKING("FIT verwerking", Bevolkingsonderzoek.COLON),

	IFOBT_HERINNERING("FIT herinnering", Bevolkingsonderzoek.COLON),

	GUNSTIGE_UITSLAG("Gunstige uitslag versturen", Bevolkingsonderzoek.COLON),

	INTAKE_AFSPRAAK_MAKEN("Intake afspraken inplannen", List.of(OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.COLON),

	COLON_VERVOLG_INTAKE_CONCLUSIE("Vervolg intakeconclusie", Bevolkingsonderzoek.COLON),

	COLON_HUISARTSBERICHTEN("Huisartsberichten", List.of(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.COLON),

	BRIEVEN_GENEREREN("Brieven genereren", Bevolkingsonderzoek.COLON),

	VERSLAGEN("CDA Berichten", Bevolkingsonderzoek.COLON),

	COLON_ILM("ILM DK", Bevolkingsonderzoek.COLON),

	COLON_CONTROLE_MISSENDE_UITSLAGEN("Controle missende uitslagen", Bevolkingsonderzoek.COLON),

	CERVIX_BATCH_NA_GBA_VERWERKING("Baarmoederhalskanker specifieke verwerking nav. GBA", Bevolkingsonderzoek.CERVIX),

	CERVIX_SELECTIE("Client selectie", Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM("Verstuur zas uitnodigingen", Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_CONTROLE_TERUGKOPPELING("Controle zas terugkoppeling van inpakcentrum", Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_TERUGKOPPELING("Terugkoppeling zas van Inpakcentrum", Bevolkingsonderzoek.CERVIX),

	CERVIX_VERVOLGONDERZOEK("Controle-uitstrijkje", Bevolkingsonderzoek.CERVIX),

	CERVIX_HERINNEREN("Herinneren", Bevolkingsonderzoek.CERVIX),

	CERVIX_UITSTEL("Uitstel", Bevolkingsonderzoek.CERVIX),

	CERVIX_GEVOLGEN_LABPROCES_VERWERKEN("Gevolgen labproces verwerken", Bevolkingsonderzoek.CERVIX),

	CERVIX_BRIEVEN_GENEREREN("Brieven genereren", Bevolkingsonderzoek.CERVIX),

	REGIO_BRIEVEN_GENEREREN("BMHK huisarts brieven genereren", List.of(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.CERVIX),

	CERVIX_HUISARTSBERICHTEN("Huisartsberichten", List.of(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.CERVIX),

	CERVIX_DIGITALE_LABFORMULIER_FOUT_BERICHTEN("Digitaal labformulier fout berichten", List.of(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE),
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HPV_KOPPELING("HPV koppeling (alleen analyseresultaten)", List.of(OrganisatieType.RIVM, OrganisatieType.BMHK_LABORATORIUM), Bevolkingsonderzoek.CERVIX),

	CERVIX_ORDER("Cytologie order verwerking", List.of(OrganisatieType.RIVM, OrganisatieType.BMHK_LABORATORIUM), Bevolkingsonderzoek.CERVIX),

	CERVIX_HL7V2_KOPPELINGEN("HL7v2 koppelingen (alleen HPV order)", Bevolkingsonderzoek.CERVIX),

	CERVIX_VERSLAG_CYTOLOGIE("Verslag cytologie", List.of(OrganisatieType.RIVM, OrganisatieType.BMHK_LABORATORIUM), Bevolkingsonderzoek.CERVIX),

	CERVIX_BEPALEN_VERRICHTINGEN("Bepalen verrichtingen", List.of(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.CERVIX),

	CERVIX_HPV_ORU("HPV-uitslag ORU berichten verwerking", List.of(OrganisatieType.RIVM, OrganisatieType.BMHK_LABORATORIUM), Bevolkingsonderzoek.CERVIX),

	CERVIX_HEROVERWEGERS("Heroverwegers brief aanmaken", List.of(OrganisatieType.RIVM), Bevolkingsonderzoek.CERVIX),

	CERVIX_HPVMIN_UITSLAGEN_VERWIJDEREN("HPV(-) labformuliergegevens wissen + scans verwijderen", Bevolkingsonderzoek.CERVIX),

	CERVIX_VERLATE_DEELNAME_COVID19("Verlate deelname COVID-19", Bevolkingsonderzoek.CERVIX),

	CERVIX_OUDE_NIET_INGESTUURDE_ZAS("Herinneren oude ZAS (Z) insturen", Bevolkingsonderzoek.CERVIX),

	CERVIX_ILM("ILM BMHK", Bevolkingsonderzoek.CERVIX),

	CERVIX_CONTROLE_MISSENDE_UITSLAGEN("Controle missende uitslagen", Bevolkingsonderzoek.CERVIX),

	MAMMA_HERINNEREN("Herinneren", Bevolkingsonderzoek.MAMMA),

	MAMMA_BATCH_NA_GBA_VERWERKING("Borstkanker specifieke verwerking nav. GBA", Bevolkingsonderzoek.MAMMA),

	MAMMA_BRIEVEN_GENEREREN("Brieven genereren", Bevolkingsonderzoek.MAMMA),

	MAMMA_IMS_UITGAAND("Uitgaande HL7-berichten naar IMS", Bevolkingsonderzoek.MAMMA),

	MAMMA_IMS_INKOMEND("Inkomende HL7-berichten IMS", Bevolkingsonderzoek.MAMMA),

	MAMMA_UITNODIGINGEN("Uitnodigingen", List.of(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.MAMMA),

	MAMMA_HUISARTS_BERICHTEN("Huisartsberichten", List.of(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.MAMMA),

	MAMMA_VERSLAG_FOLLOW_UP("Verslag follow-up", List.of(OrganisatieType.RIVM), Bevolkingsonderzoek.MAMMA),

	MAMMA_KANSBEREKENING("Kansberekening", Bevolkingsonderzoek.MAMMA),

	MAMMA_VERVOLG_ONDERZOEKEN("Nabewerking onderzoeken", List.of(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.MAMMA),

	MAMMA_UITWISSELPORTAAL("Uitwisselportaal", List.of(OrganisatieType.RIVM), Bevolkingsonderzoek.MAMMA),

	MAMMA_PALGA_CSV_EXPORT("Palga csv exporteren", List.of(OrganisatieType.RIVM), Bevolkingsonderzoek.MAMMA),

	MAMMA_PALGA_CSV_IMPORT("Palga csv importeren", List.of(OrganisatieType.RIVM), Bevolkingsonderzoek.MAMMA),

	MAMMA_SE_BERICHTEN("SE Berichten", List.of(OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM), Bevolkingsonderzoek.MAMMA),

	MAMMA_ILM("ILM BK", Bevolkingsonderzoek.MAMMA),

	MAMMA_CONTROLE_MISSENDE_UITSLAGEN("Controle missende uitslagen", Bevolkingsonderzoek.MAMMA),

	ALGEMENE_BRIEVEN_GENEREREN("Algemene brieven genereren", Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	BEZWAAR_BRIEVEN_GENEREREN("Bezwaar brieven genereren", Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	PROJECT_BRIEVEN_GENEREREN("Project brieven genereren", Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	ILM_ALGEMENE_GEGEVENS_VERWIJDEREN("ILM algemene gegevens verwijderen", Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	WACHTWOORD_VERLOOPT_HERINNERING("Wachtwoord verloopt herinnering", Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	ENOVATION_HUISARTSEN("Enovation huisartsen import", Bevolkingsonderzoek.MAMMA, Bevolkingsonderzoek.COLON),

	MAIL_LANDELIJK("Mail versturen", List.of(OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	SIGNALERING_GENDER("Signalering gender", Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	;

	private final String naam;

	private final Bevolkingsonderzoek[] bevolkingsOnderzoek;

	private final List<OrganisatieType> organisatieTypes;

	DashboardType(String naam, Bevolkingsonderzoek... bevolkingsOnderzoek)
	{
		this(naam, null, bevolkingsOnderzoek);
	}

	DashboardType(String naam, List<OrganisatieType> organisatieTypes, Bevolkingsonderzoek... bevolkingsOnderzoek)
	{
		this.naam = naam;
		this.organisatieTypes = organisatieTypes;
		this.bevolkingsOnderzoek = bevolkingsOnderzoek;
	}

	public String getNaam()
	{
		return naam;
	}

	public Bevolkingsonderzoek[] getBevolkingsOnderzoek()
	{
		return bevolkingsOnderzoek;
	}

	public List<OrganisatieType> getOrganisatieTypes()
	{
		return organisatieTypes;
	}
}
