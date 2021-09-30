package nl.rivm.screenit.model.dashboard;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobType;

public enum DashboardType
{
	GBA_LANDELIJK("GBA", JobType.GBA, Arrays.asList(OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	CLIENT_SELECTIE("Client selectie", JobType.CLIENT_SELECTIE, Arrays.asList(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.COLON),

	UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM("Uitnodiging versturen naar inpakcentrum", JobType.UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM_JOB_DK, Bevolkingsonderzoek.COLON),

	CONTROLE_TERUGKOPPELING_CLIENT_SELECTIE("Controle terugkoppeling van Inpakcentrum", Bevolkingsonderzoek.COLON),

	TERUGKOPPELING_CLIENT_SELECTIE("Terugkoppeling van Inpakcentrum", JobType.KOPPELDATA_VERWERKING, Bevolkingsonderzoek.COLON),

	RETOURZENDINGEN("Retourzendingen", Bevolkingsonderzoek.COLON),

	IFOBT_INLEZEN("FIT uitslagen ontvangen", Bevolkingsonderzoek.COLON),

	IFOBT_VERWERKING("FIT verwerking", JobType.IFOBT_VERWERKING, Bevolkingsonderzoek.COLON),

	IFOBT_HERINNERING("FIT herinnering", JobType.IFOBT_HERINNERING, Bevolkingsonderzoek.COLON),

	GUNSTIGE_UITSLAG("Gunstige uitslag versturen", JobType.GUNSTIGE_UITSLAG, Bevolkingsonderzoek.COLON),

	INTAKE_AFSPRAAK_MAKEN("Intake afspraken inplannen", JobType.INTAKE_AFSPRAKEN_MAKEN, Arrays.asList(OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.COLON),

	COLON_VERVOLG_INTAKE_CONCLUSIE("Vervolg intakeconclusie", JobType.VERVOLG_INTAKE_CONCLUSIE_BATCH, Bevolkingsonderzoek.COLON),

	VERSLAGEN("CDA Berichten", Bevolkingsonderzoek.COLON),

	BRIEVEN_GENEREREN("Brieven genereren", JobType.BRIEVEN_GENEREREN, Bevolkingsonderzoek.COLON),

	ENOVATION_HUISARTSEN("Enovation huisartsen import", JobType.ENOVATION_HUISARTSEN_BATCH, Bevolkingsonderzoek.MAMMA, Bevolkingsonderzoek.COLON),

	CERVIX_ILM("ILM BMHK", JobType.CERVIX_ILM, Bevolkingsonderzoek.CERVIX),

	CERVIX_SELECTIE("Client selectie", JobType.CERVIX_SELECTIE, Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM("Verstuur zas uitnodigingen", JobType.UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM_JOB_DK, Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_CONTROLE_TERUGKOPPELING("Controle zas terugkoppeling van inpakcentrum", Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_TERUGKOPPELING("Terugkoppeling zas van Inpakcentrum", JobType.CERVIX_KOPPELDATA_VERWERKING, Bevolkingsonderzoek.CERVIX),

	CERVIX_VERVOLGONDERZOEK("Controle-uitstrijkje", JobType.CERVIX_VERVOLGONDERZOEK, Bevolkingsonderzoek.CERVIX),

	CERVIX_HERINNEREN("Herinneren", JobType.CERVIX_HERINNEREN, Bevolkingsonderzoek.CERVIX),

	CERVIX_UITSTEL("Uitstel", JobType.CERVIX_UITSTEL, Bevolkingsonderzoek.CERVIX),

	CERVIX_GEVOLGEN_LABPROCES_VERWERKEN("Gevolgen labproces verwerken", JobType.CERVIX_GEVOLGEN_LABPROCES_VERWERKEN, Bevolkingsonderzoek.CERVIX),

	CERVIX_BRIEVEN_GENEREREN("Brieven genereren", JobType.CERVIX_BRIEVEN, Bevolkingsonderzoek.CERVIX),

	REGIO_BRIEVEN_GENEREREN(
		"BMHK huisarts brieven genereren",
		JobType.REGIO_BRIEVEN,
		Arrays.asList(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE),
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HUISARTSBERICHTEN(
		"Huisartsberichten",
		JobType.CERVIX_HUISARTSBERICHTEN,
		Arrays.asList(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE),
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HPV_KOPPELING("HPV koppeling", Arrays.asList(OrganisatieType.RIVM, OrganisatieType.BMHK_LABORATORIUM), Bevolkingsonderzoek.CERVIX),

	CERVIX_ORDER("Order verwerking", JobType.CERVIX_ORDER, Arrays.asList(OrganisatieType.RIVM, OrganisatieType.BMHK_LABORATORIUM), Bevolkingsonderzoek.CERVIX),

	CERVIX_VERSLAG_CYTOLOGIE("Verslag cytologie", Arrays.asList(OrganisatieType.RIVM, OrganisatieType.BMHK_LABORATORIUM), Bevolkingsonderzoek.CERVIX),

	BEZWAAR_BRIEVEN_GENEREREN("Bezwaar brieven genereren", JobType.BEZWAAR_BRIEVEN, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	ALGEMENE_BRIEVEN_GENEREREN("Algemene brieven genereren", JobType.ALGEMENE_BRIEVEN, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	PROJECT_BRIEVEN_GENEREREN("Project brieven genereren", JobType.PROJECT_BRIEVEN, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	ILM_ALGEMENE_GEGEVENS_VERWIJDEREN(
		"ILM algemene gegevens verwijderen",
		JobType.ILM_ALGEMENE_GEGEVENS_VERWIJDEREN,
		Bevolkingsonderzoek.COLON,
		Bevolkingsonderzoek.CERVIX,
		Bevolkingsonderzoek.MAMMA),

	COLON_ILM("ILM DK", JobType.COLON_ILM, Bevolkingsonderzoek.COLON),

	COLON_IFOBT_KOPPELING("FIT-berichten koppeling", Arrays.asList(OrganisatieType.RIVM, OrganisatieType.LABORATORIUM), Bevolkingsonderzoek.COLON),

	COLON_BATCH_NA_GBA_VERWERKING("Darmkanker specifieke verwerking nav. GBA", JobType.COLON_NA_GBA, Bevolkingsonderzoek.COLON),

	COLON_HUISARTSBERICHTEN(
		"Huisartsberichten",
		JobType.COLON_HUISARTSBERICHTEN_OPNIEUW_VERSTUREN,
		Arrays.asList(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE),
		Bevolkingsonderzoek.COLON),

	CERVIX_BATCH_NA_GBA_VERWERKING("Baarmoederhalskanker specifieke verwerking nav. GBA", JobType.CERVIX_NA_GBA, Bevolkingsonderzoek.CERVIX),

	CERVIX_BEPALEN_VERRICHTINGEN(
		"Bepalen verrichtingen",
		JobType.CERVIX_BEPALEN_VERRICHTINGEN,
		Arrays.asList(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE),
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HPV_ORU(
		"HPV-uitslag ORU berichten verwerking",
		JobType.CERVIX_HPV_ORU,
		Arrays.asList(OrganisatieType.RIVM, OrganisatieType.BMHK_LABORATORIUM),
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HEROVERWEGERS(
		"Heroverwegers brief aanmaken",
		JobType.CERVIX_HEROVERWEGERS,
		Arrays.asList(OrganisatieType.RIVM),
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HPVMIN_UITSLAGEN_VERWIJDEREN("HPV(-) labformuliergegevens wissen", JobType.CERVIX_HPV_ORU, Bevolkingsonderzoek.CERVIX),

	MAMMA_HERINNEREN("Herinneren", JobType.MAMMA_HERINNEREN, Bevolkingsonderzoek.MAMMA),

	MAMMA_BATCH_NA_GBA_VERWERKING("Borstkanker specifieke verwerking nav. GBA", JobType.MAMMA_NA_GBA, Bevolkingsonderzoek.MAMMA),

	MAMMA_BRIEVEN_GENEREREN("Brieven genereren", JobType.MAMMA_BRIEVEN, Bevolkingsonderzoek.MAMMA),

	MAMMA_IMS_UITGAAND("Uitgaande HL7-berichten naar IMS", Bevolkingsonderzoek.MAMMA),

	MAMMA_IMS_INKOMEND("Inkomende HL7-berichten IMS", Bevolkingsonderzoek.MAMMA),

	MAMMA_UITNODIGINGEN("Uitnodigingen", JobType.MAMMA_UITNODIGEN, Arrays.asList(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE), Bevolkingsonderzoek.MAMMA),

	MAMMA_HUISARTS_BERICHTEN(
		"Huisartsberichten",
		JobType.MAMMA_HUISARTS_BERICHTEN_OPNIEUW_VERSTUREN,
		Arrays.asList(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE),
		Bevolkingsonderzoek.MAMMA),

	MAMMA_VERSLAG_FOLLOW_UP("Verslag follow-up", Arrays.asList(OrganisatieType.RIVM), Bevolkingsonderzoek.MAMMA),

	MAMMA_KANSBEREKENING("Kansberekening", JobType.MAMMA_KANSBEREKENING, Bevolkingsonderzoek.MAMMA),

	MAMMA_VERVOLG_ONDERZOEKEN(
		"Nabewerking onderzoeken",
		JobType.MAMMA_VERVOLG_ONDERZOEKEN,
		Arrays.asList(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE),
		Bevolkingsonderzoek.MAMMA),

	MAMMA_UITWISSELPORTAAL(
		"Uitwisselportaal",
		JobType.MAMMA_UITWISSELPORTAAL,
		Arrays.asList(OrganisatieType.RIVM),
		Bevolkingsonderzoek.MAMMA),

	MAMMA_PALGA_CSV_EXPORT(
		"Palga csv exporteren",
		JobType.MAMMA_PALGA_CSV_EXPORT,
		Arrays.asList(OrganisatieType.RIVM),
		Bevolkingsonderzoek.MAMMA),

	MAMMA_PALGA_CSV_IMPORT(
		"Palga csv importeren",
		JobType.MAMMA_PALGA_CSV_IMPORT,
		Arrays.asList(OrganisatieType.RIVM),
		Bevolkingsonderzoek.MAMMA),

	MAMMA_SE_BERICHTEN(
		"SE Berichten",
		Arrays.asList(OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM),
		Bevolkingsonderzoek.MAMMA),

	MAMMA_ILM("ILM BK", JobType.MAMMA_ILM, Bevolkingsonderzoek.MAMMA),

	;

	private final String naam;

	private final JobType type;

	private final Bevolkingsonderzoek[] bevolkingsOnderzoek;

	private final List<OrganisatieType> organisatieTypes;

	DashboardType(String naam, List<OrganisatieType> organisatieTypes, Bevolkingsonderzoek... bevolkingsOnderzoek)
	{
		this(naam, null, organisatieTypes, bevolkingsOnderzoek);
	}

	DashboardType(String naam, Bevolkingsonderzoek... bevolkingsOnderzoek)
	{
		this(naam, null, null, bevolkingsOnderzoek);
	}

	DashboardType(String naam, JobType type, Bevolkingsonderzoek... bevolkingsOnderzoek)
	{
		this(naam, type, null, bevolkingsOnderzoek);
	}

	DashboardType(String naam, JobType type, List<OrganisatieType> organisatieTypes, Bevolkingsonderzoek... bevolkingsOnderzoek)
	{
		this.naam = naam;
		this.type = type;
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
