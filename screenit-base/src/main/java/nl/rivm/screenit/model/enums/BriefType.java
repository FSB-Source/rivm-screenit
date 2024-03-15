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

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lombok.Getter;

import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.topicuszorg.util.collections.CollectionUtils;

public enum BriefType
{
	COLON_VOORAANKONDIGING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_10_101 - Vooraankondiging",
		Bevolkingsonderzoek.COLON),

	COLON_VOORAANKONDIGING_NA_VERVOLGONDERZOEK(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_10_102 - Vooraankondiging na vervolgonderzoek",
		Bevolkingsonderzoek.COLON),

	COLON_UITNODIGING(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_101 - Uitnodiging Colon",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_NA_SURVEILLANCE_OF_BEHANDELING(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_103 - Uitnodiging na surveillance of behandeling",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_ZONDER_VOORAANKONDIGING(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_102 - Uitnodiging zonder VA",
		Bevolkingsonderzoek.COLON),

	COLON_UITNODIGING_ZONDER_FIT(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_104 - Uitnodiging Colon zonder FIT",
		Bevolkingsonderzoek.COLON),

	COLON_UITNODIGING_WANT_GEEN_MONSTER(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_201 - Uitnodiging na uitslag onbekend",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_WANT_BRIEF_ONLEESBAAR(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_202 - Uitnodiging na uitslag onbekend, barcode onleesbaar",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_WANT_BUIS_KAPOT(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_203 - Uitnodiging na uitslag onbekend, testbuisje kapot",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_WANT_GEEN_VLOEISTOF_IN_BUIS(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_204 - Uitnodiging na uitslag onbekend, testbuisje geen vloeistof",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_WANT_TECHNISCH_ONMOGELIJK(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_205 - Uitnodiging na uitslag onbekend, overige redenen",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_WANT_TE_WEINIG_ONTLASTING(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_206 - Uitnodiging na uitslag onbekend, te weinig ontlasting",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_WANT_TE_VEEL_ONTLASTING(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_207 - Uitnodiging na uitslag onbekend, te veel ontlasting",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_WANT_VERLOREN(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_301 - Uitnodiging na FIT verloren",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_HERAANMELDING(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_302 - Uitnodiging na heraanmelding",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_WANT_TE_LAAT_TERUGGESTUURD(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_401 - Uitnodiging na uitslag onbekend, vervaldatum verstreken",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_WANT_HOUDBAARHEID_VERLOPEN(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_501 - Uitnodiging na uitslag onbekend, THT-datum verstreken",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_WANT_GEEN_TEST(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_601 - Uitnodiging na uitslag onbekend, geen testbuisje ontvangen",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_UITNODIGING_WANT_GEEN_BRIEF(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_20_701- Uitnodiging geen antwoordformulier ontvangen",
		Bevolkingsonderzoek.COLON),

	COLON_GUNSTIGE_UITSLAG(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_40_101 - Uitslagbrief gunstig",
		Bevolkingsonderzoek.COLON),

	COLON_UITNODIGING_INTAKE(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_40_102 - Uitslagbrief ongunstig",
		Bevolkingsonderzoek.COLON),

	COLON_INTAKE_NO_SHOW(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_40_103 - Uitslagbrief ongunstig, no show",
		Bevolkingsonderzoek.COLON)
		{
			@Override
			public List<BriefType> getMagNietOpZelfdeDagAfgedruktTypes()
			{
				return List.of(COLON_BEVESTIGING_TERUG_NAAR_SCREENING, COLON_BEVESTIGING_DEFINITIEVE_EXCLUSIE, COLON_INTAKE_NO_SHOW, COLON_INTAKE_GEWIJZIGD);
			}
		},

	COLON_INTAKE_GEWIJZIGD(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_50_101 - Brief gewijzigde intake",
		Bevolkingsonderzoek.COLON)
		{
			@Override
			public List<BriefType> getMagNietOpZelfdeDagAfgedruktTypes()
			{
				return List.of(COLON_BEVESTIGING_TERUG_NAAR_SCREENING, COLON_BEVESTIGING_DEFINITIEVE_EXCLUSIE, COLON_INTAKE_NO_SHOW, COLON_INTAKE_GEWIJZIGD,
					COLON_INTAKE_AFMELDING);
			}
		},

	COLON_INTAKE_AFMELDING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_50_102 - Brief bevestiging afmelden intake",
		Bevolkingsonderzoek.COLON)
		{
			@Override
			public List<BriefType> getMagNietOpZelfdeDagAfgedruktTypes()
			{
				return List.of(COLON_INTAKE_AFMELDING, COLON_INTAKE_GEWIJZIGD);
			}
		},

	COLON_UITSLAGBRIEF_EXTRA_MONSTER(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "1_40_201 - Uitslagbrief extra monster",
		Bevolkingsonderzoek.COLON),

	COLON_UITSLAGBRIEF_ONBEOORDEELBAAR_BUITEN_DOELGROEP(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "1_40_202 - Uitslagbrief FIT onbeoordeelbaar, buiten doelgroep",
		Bevolkingsonderzoek.COLON),

	COLON_UITSLAGBRIEF_ONGUNSTIGE_BUITEN_DOELGROEP(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "1_40_203 - Uitslagbrief ongunstig, buiten doelgroep",
		Bevolkingsonderzoek.COLON),

	COLON_HERINNERING_ANDERE_INTAKELOCATIE(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_50_104 - Brief herinnering client wil andere intakelocatie",
		Bevolkingsonderzoek.COLON),

	COLON_AFMELDING_AANVRAAG(OrganisatieType.SCREENINGSORGANISATIE, true, "1_70_101 - Brief aanvraag definitief of tijdelijk afmelden", Bevolkingsonderzoek.COLON)
		{
			@Override
			public List<BriefType> getMagNietOpZelfdeDagAfgedruktTypes()
			{
				return new ArrayList<>(Arrays.asList(COLON_AFMELDING_AANVRAAG, COLON_AFMELDING_BEVESTIGING, COLON_AFMELDING_HANDTEKENING, COLON_HERAANMELDING_AANVRAAG,
					COLON_HERAANMELDING_BEVESTIGING, COLON_HERAANMELDING_HANDTEKENING));
			}
		},

	COLON_AFMELDING_BEVESTIGING(OrganisatieType.SCREENINGSORGANISATIE, true, "1_70_103 - Brief bevestiging definitief of tijdelijk afmelden", Bevolkingsonderzoek.COLON)
		{
			@Override
			public List<BriefType> getMagNietOpZelfdeDagAfgedruktTypes()
			{
				return new ArrayList<>(Arrays.asList(COLON_AFMELDING_AANVRAAG, COLON_AFMELDING_BEVESTIGING, COLON_AFMELDING_HANDTEKENING, COLON_HERAANMELDING_AANVRAAG,
					COLON_HERAANMELDING_BEVESTIGING, COLON_HERAANMELDING_HANDTEKENING));
			}
		},

	COLON_AFMELDING_HANDTEKENING(OrganisatieType.SCREENINGSORGANISATIE, true, "1_70_102 - Brief definitief of tijdelijk afmelden zonder handtekening", Bevolkingsonderzoek.COLON)
		{
			@Override
			public List<BriefType> getMagNietOpZelfdeDagAfgedruktTypes()
			{
				return new ArrayList<>(Arrays.asList(COLON_AFMELDING_AANVRAAG, COLON_AFMELDING_BEVESTIGING, COLON_AFMELDING_HANDTEKENING, COLON_HERAANMELDING_AANVRAAG,
					COLON_HERAANMELDING_BEVESTIGING, COLON_HERAANMELDING_HANDTEKENING));
			}
		},

	COLON_HERINNERING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "1_30_101 - Brief FIT herinnering",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_CONTROLE_COLOSCOPIE(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "19 - Brief controle coloscopie",
		Bevolkingsonderzoek.COLON),

	@Deprecated
	COLON_HERAANMELDING_AANVRAAG(OrganisatieType.SCREENINGSORGANISATIE, true, "1_70_201 - Brief aanvraag heraanmelding", Bevolkingsonderzoek.COLON)
		{
			@Override
			public List<BriefType> getMagNietOpZelfdeDagAfgedruktTypes()
			{
				return new ArrayList<>(Arrays.asList(COLON_AFMELDING_AANVRAAG, COLON_AFMELDING_BEVESTIGING, COLON_AFMELDING_HANDTEKENING, COLON_HERAANMELDING_AANVRAAG,
					COLON_HERAANMELDING_BEVESTIGING, COLON_HERAANMELDING_HANDTEKENING));
			}
		},

	COLON_HERAANMELDING_BEVESTIGING(OrganisatieType.SCREENINGSORGANISATIE, true, "1_70_203 - Brief bevestiging heraanmelding", Bevolkingsonderzoek.COLON)
		{
			@Override
			public List<BriefType> getMagNietOpZelfdeDagAfgedruktTypes()
			{
				return new ArrayList<>(Arrays.asList(COLON_AFMELDING_AANVRAAG, COLON_AFMELDING_BEVESTIGING, COLON_AFMELDING_HANDTEKENING, COLON_HERAANMELDING_AANVRAAG,
					COLON_HERAANMELDING_BEVESTIGING, COLON_HERAANMELDING_HANDTEKENING));
			}
		},

	@Deprecated
	COLON_HERAANMELDING_HANDTEKENING(OrganisatieType.SCREENINGSORGANISATIE, true, "1_70_202 - Brief heraanmelding zonder handtekening", Bevolkingsonderzoek.COLON)
		{
			@Override
			public List<BriefType> getMagNietOpZelfdeDagAfgedruktTypes()
			{
				return new ArrayList<>(Arrays.asList(COLON_AFMELDING_AANVRAAG, COLON_AFMELDING_BEVESTIGING, COLON_AFMELDING_HANDTEKENING, COLON_HERAANMELDING_AANVRAAG,
					COLON_HERAANMELDING_BEVESTIGING, COLON_HERAANMELDING_HANDTEKENING));
			}
		},

	COLON_OPEN_UITNODIGING(OrganisatieType.SCREENINGSORGANISATIE, true, "1_20_801 - Brief open uitnodiging", Bevolkingsonderzoek.COLON),

	COLON_BEVESTIGING_INTAKE_AFSRPAAK_NA_OPEN_UITNODIGING(OrganisatieType.SCREENINGSORGANISATIE, true, "1_50_103 - Brief bevestiging afspraak na open uitnodiging",
		Bevolkingsonderzoek.COLON),

	COLON_BEVESTIGING_DEFINITIEVE_EXCLUSIE(OrganisatieType.SCREENINGSORGANISATIE, true, "1_50_201 Brief bevestiging definitieve exclusie", Bevolkingsonderzoek.COLON)
		{
			@Override
			public List<BriefType> getMagNietOpZelfdeDagAfgedruktTypes()
			{
				return new ArrayList<>(Arrays.asList(COLON_BEVESTIGING_TERUG_NAAR_SCREENING, COLON_BEVESTIGING_DEFINITIEVE_EXCLUSIE, COLON_INTAKE_NO_SHOW, COLON_INTAKE_GEWIJZIGD));
			}
		},

	COLON_BEVESTIGING_TERUG_NAAR_SCREENING(OrganisatieType.SCREENINGSORGANISATIE, true, "1_50_202 Brief bevestiging terug naar screening na x- jaar", Bevolkingsonderzoek.COLON)
		{
			@Override
			public List<BriefType> getMagNietOpZelfdeDagAfgedruktTypes()
			{
				return new ArrayList<>(Arrays.asList(COLON_BEVESTIGING_TERUG_NAAR_SCREENING, COLON_BEVESTIGING_DEFINITIEVE_EXCLUSIE, COLON_INTAKE_NO_SHOW, COLON_INTAKE_GEWIJZIGD));
			}
		},

	CLIENT_BEZWAAR_AANVRAAG(OrganisatieType.SCREENINGSORGANISATIE, true, "0_80_101 - Brief aanvraag bezwaar", Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX),

	CLIENT_BEZWAAR_BEVESTIGING(OrganisatieType.SCREENINGSORGANISATIE, true, "0_80_103 - Brief bevestiging bezwaar", Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX),

	CLIENT_BEZWAAR_HANDTEKENING(OrganisatieType.SCREENINGSORGANISATIE, true, "0_80_102 - Brief bezwaar zonder handtekening", Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX),

	CLIENT_INZAGE_PERSOONSGEGEVENS_AANVRAAG(OrganisatieType.SCREENINGSORGANISATIE, true, "0_80_201 - Brief aanvraag inzage of overdracht persoonsgegevens",
		Bevolkingsonderzoek.COLON,
		Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	CLIENT_INZAGE_PERSOONSGEGEVENS_HANDTEKENING(OrganisatieType.SCREENINGSORGANISATIE, true, "0_80_202 - Brief aanvraag inzage of overdracht persoonsgegevens zonder handtekening",
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	CLIENT_SIGNALERING_GENDER(OrganisatieType.SCREENINGSORGANISATIE, true, "0_10_101 - Brief signalering gender", Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA),

	CERVIX_VOORAANKONDIGING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_10_101 - Cervix vooraankondiging",
		Bevolkingsonderzoek.CERVIX),

	@Deprecated
	CERVIX_UITNODIGING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_20_101 - Cervix uitnodiging",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_UITNODIGING_30(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_20_102 - Cervix uitnodiging leeftijdscategorie 30",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_UITNODIGING_35(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_20_103 - Cervix uitnodiging leeftijdscategorie 35",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_UITNODIGING_40(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_20_104 - Cervix uitnodiging leeftijdscategorie 40",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_UITNODIGING_45(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_20_105 - Cervix uitnodiging leeftijdscategorie 45",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_UITNODIGING_50(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_20_106 - Cervix uitnodiging leeftijdscategorie 50",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_UITNODIGING_55(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_20_107 - Cervix uitnodiging leeftijdscategorie 55",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_UITNODIGING_60(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_20_108 - Cervix uitnodiging leeftijdscategorie 60",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_UITNODIGING_65(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_20_109 - Cervix uitnodiging leeftijdscategorie 65",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HERINNERING_UITNODIGING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_30_101 - Herinnering uitnodiging cervix",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HERINNERING_ZAS_UITNODIGING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_30_102 - Herinnering na aangevraagde ZAS",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_LAATSTE_HERINNERING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_30_103 - Extra herinnering 35-60",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HERINNERING_30_JARIGEN(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_30_104 - Herinnering 30 jarigen",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_UITNODIGING(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_20_301 - Verzendbrief ZAS",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_COMBI_UITNODIGING_30(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_20_112 - Cervix uitnodiging leeftijdscategorie 30 met ZAS",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_NON_RESPONDER(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_30_302 - Herinnering non-responders met ZAS",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_UITNODIGING_CONTROLEUITSTRIJKJE(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_20_201 - Cervix uitnodiging controle-uitstrijkje",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_AFMELDING_AANVRAAG(OrganisatieType.SCREENINGSORGANISATIE, true, "2_70_101 - Brief aanvraag definitieve afmelding", Bevolkingsonderzoek.CERVIX),

	CERVIX_AFMELDING_HANDTEKENING(OrganisatieType.SCREENINGSORGANISATIE, true, "2_70_102 - Brief afmelden zonder handtekening", Bevolkingsonderzoek.CERVIX),

	CERVIX_BEVESTIGING_DEFINITIEVE_AFMELDING(OrganisatieType.SCREENINGSORGANISATIE, true, "2_70_103 - Brief bevestiging definitieve afmelding", Bevolkingsonderzoek.CERVIX),

	@Deprecated
	CERVIX_HERAANMELDING_AANVRAAG(OrganisatieType.SCREENINGSORGANISATIE, false, "2_70_201 - Brief aanvraag heraanmelding", Bevolkingsonderzoek.CERVIX),

	@Deprecated
	CERVIX_HERAANMELDING_HANDTEKENING(OrganisatieType.SCREENINGSORGANISATIE, false, "2_70_202 - Brief heraanmelding zonder handtekening", Bevolkingsonderzoek.CERVIX),

	CERVIX_HERAANMELDING_BEVESTIGING(OrganisatieType.SCREENINGSORGANISATIE, true, "2_70_203 - Brief bevestiging heraanmelding", Bevolkingsonderzoek.CERVIX),

	CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_HPV_ONBEOORDEELBAAR(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_101 - Uitslag uitstrijkje niet analyseerbaar of HPV onbeoordeelbaar",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_CYTOLOGIE_ONBEOORDEELBAAR(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_102 - Uitslag cytologie onbeoordeelbaar",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_CYTOLOGIE_ONBEOORDEELBAAR(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_103 - Uitslag uitstrijkje niet analyseerbaar of cytologie onbeoordeelbaar",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_NIET_ANALYSEERBAAR_OF_ONBEOORDEELBAAR(
		OrganisatieType.INPAKCENTRUM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_104 - Uitslag ZAS niet analyseerbaar",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_UITSTRIJKJE_TWEEDE_KEER_ONBEOORDEELBAAR(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_105 - Uitslag uitstrijkje tweede keer onbeoordeelbaar",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_TWEEDE_KEER_ONBEOORDEELBAAR(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_106 - Uitslag ZAS tweede keer onbeoordeelbaar",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HPV_NEGATIEF_VOLGENDE_RONDE_5_JAAR(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_201 - HPV negatief, volgende ronde 5 jaar",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HPV_NEGATIEF_LAATSTE_RONDE(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_202 - HPV negatief, laatste ronde",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HPV_NEGATIEF_VOLGENDE_RONDE_10_JAAR(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_203 - HPV negatief, volgende ronde 10 jaar",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_HPV_POSITIEF(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_301 - Uitslag ZAS HPV positief",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_CYTOLOGIE_NEGATIEF(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_302 - Uitslag cytologie negatief",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_CYTOLOGIE_LICHTE_AFWIJKING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_303 - Uitslag cytologie lichte afwijking",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_CYTOLOGIE_LICHTE_AFWIJKING_HPVOTHER(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_312 - Uitslag cytologie lichte afwijking, vervolgonderzoek",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_CYTOLOGIE_AFWIJKING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_304 - Uitslag cytologie afwijking",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_VOLGEND_MONSTER_CYTOLOGIE_NEGATIEF(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_305 - Uitslag volgend monster cytologie negatief",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_VOLGEND_MONSTER_CYTOLOGIE_LICHTE_AFWIJKING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_306 - Uitslag volgend monster cytologie lichte afwijking",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_VOLGEND_MONSTER_CYTOLOGIE_LICHTE_AFWIJKING_HPVOTHER(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_313 - Uitslag volgend monster cytologie lichte afwijking, vervolgonderzoek",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_VOLGEND_MONSTER_CYTOLOGIE_AFWIJKING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_307 - Uitslag volgend monster cytologie afwijking",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HUISARTS_ONBEKEND(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_308 - Uitslag huisarts onbekend",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_CONTROLEUITSTRIJKJE_NEGATIEF(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_309 - Uitslag controle-uitstrijkje negatief",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_CONTROLEUITSTRIJKJE_LICHTE_AFWIJKING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_310 - Uitslag controle-uitstrijkje lichte afwijking",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_CONTROLEUITSTRIJKJE_AFWIJKING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_311 - Uitslag controle-uitstrijkje afwijking",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_MONSTER_NA_HPV_NEGATIEF(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_401 - Uitslag extra monster na HPV negatief",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_NA_HPV_POSITIEF(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_402 - Uitslag extra ZAS na HPV positief",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_NA_CYTOLOGIE_ONBEOORDEELBAAR(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_403 - Uitslag extra ZAS na cytologie onbeoordeelbaar",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_NA_CYTOLOGIE_NEGATIEF(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_404 - Uitslag extra monster na cytologie negatief",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_NA_CYTOLOGIE_POSITIEF(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_405 - Uitslag extra monster na cytologie positief",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_ZAS_NA_CYTOLOGIE_POSITIEF_HPVOTHER(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		false, "2_40_406 - Uitslag extra monster na cytologie positief, vervolgonderzoek",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_HEROVERWEGERS(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY },
		true, "2_70_301 - Brief heroverwegers",
		Bevolkingsonderzoek.CERVIX),

	CERVIX_VERWIJDER_UITSLAG(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF },
		true, "2_80_301 - Brief aanvraag verwijderen uitslag",
		Bevolkingsonderzoek.CERVIX),

	COLON_VERWIJDER_UITSLAG(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF },
		true, "1_80_301 - Brief aanvraag verwijderen uitslag",
		Bevolkingsonderzoek.COLON),

	REGIO_REGISTRATIE_UITSTRIJKEND_HUISARTS(OrganisatieType.SCREENINGSORGANISATIE, true, "2_60_101 - Registratiebrief huisarts", Bevolkingsonderzoek.CERVIX),

	REGIO_UITSTRIJKEND_ARTS_VOORBLAD_LABFORMULIER(OrganisatieType.SCREENINGSORGANISATIE, false, "2_60_102 - Oplegbrief laboratoriumformulier", Bevolkingsonderzoek.CERVIX),

	REGIO_UITSTRIJKEND_ARTS_LABFORMULIER(OrganisatieType.SCREENINGSORGANISATIE, false, "2_60_103 - Laboratoriumformulier", Bevolkingsonderzoek.CERVIX),

	COLON_ZENDING_GEWEIGERD(OrganisatieType.SCREENINGSORGANISATIE, false, "1_100_101 - Zending FIT geweigerd", Bevolkingsonderzoek.COLON),

	CERVIX_ZENDING_GEWEIGERD(OrganisatieType.SCREENINGSORGANISATIE, false, "2_100_101 - Zending ZAS geweigerd", Bevolkingsonderzoek.CERVIX),

	MAMMA_AFSPRAAK_UITNODIGING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_20_101 - Uitnodiging met datum",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_OPEN_UITNODIGING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_20_102 - Uitnodiging zonder datum",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_UITNODIGING_MINDER_VALIDE(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_20_105 - Uitnodiging mindervaliden zonder datum",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_UITNODIGING_TEHUIS_ZONDER_DATUM(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_20_106 - Uitnodiging tehuis zonder datum",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_UITNODIGING_SUSPECT(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_20_107 - Uitnodiging suspecte cliënten",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_HERAANMELDING_BEVESTIGING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_70_203 - Brief bevestiging heraanmelding",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_AFMELDING_AANVRAAG(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_70_101 - Brief aanvraag definitieve afmelding",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_AFMELDING_HANDTEKENING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_70_102 - Brief afmelden zonder handtekening",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_BEVESTIGING_DEFINITIEVE_AFMELDING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_70_103 - Brief bevestiging definitieve afmelding",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_UITSTEL(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_20_110 - Bevestiging uitstel opnieuw uitnodigen",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_AFSPRAAK_VERZET(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_20_104 - Afspraak bevestiging",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_OPROEP_OPNEMEN_CONTACT(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_20_114 - Oproep na onderbroken onderzoek",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_GUNSTIGE_UITSLAG(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_101 - Uitslag cliënt negatief",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_0(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_102 - Uitslag cliënt positief BIRADS 0",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_4_5(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_103 - Uitslag cliënt positief BIRADS 4-5",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_0_ZONDER_HUISARTS(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_107 - Uitslag cliënt positief zonder huisarts BI-RADS 0",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_4_5_ZONDER_HUISARTS(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_110 - Uitslag cliënt positief zonder huisarts BI-RADS 4-5",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_0(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_108 - Uitslag cliënt positief BI-RADS 0 na eerder negatief",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_4_5(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_109 - Uitslag cliënt positief BIRADS 4-5 na eerder negatief",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_0_ZONDER_HUISARTS(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_114 - Uitslag cliënt positief zonder huisarts BI-RADS 0 na eerder negatief",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_4_5_ZONDER_HUISARTS(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_115 - Uitslag cliënt positief zonder huisarts BI-RADS 4-5 na eerder negatief",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_HERINNERING(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_30_101 - Uitnodiging herinnering",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_GEEN_ONDERZOEK(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_104 - Geen uitslag cliënt niet beoordeeld",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_20_109 - Mindervalide onderzoek in ziekenhuis",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_BEPERKT_BEOORDEELBAAR(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_105 - Uitslag cliënt negatief beperkt beoordeelbaar",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_BEPERKT_BEOORDEELBAAR_PROTHESE(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_106 - Uitslag cliënt negatief beperkt beoordeelbaar prothese > 0.8",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_VERWIJSVERSLAG(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_111 - Verwijsverslag",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_VERSLAG_UITSLAG_NEGATIEF(
		OrganisatieType.RIVM,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_113 - Verslag negatieve screening",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_GEEN_BEOORDELING_MOGELIJK(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		false, "3_40_112 - Uitslag cliënt geen beoordeling mogelijk",
		Bevolkingsonderzoek.MAMMA),

	MAMMA_INFOBRIEF_PROTHESEN(
		OrganisatieType.SCREENINGSORGANISATIE,
		new ProjectBriefActieType[] { ProjectBriefActieType.VERVANGENDEBRIEF, ProjectBriefActieType.XDAGENNAY, ProjectBriefActieType.XMETY },
		true, "3_20_111 - Infobrief prothesen",
		Bevolkingsonderzoek.MAMMA),

	;

	private static final List<BriefType> MAMMA_UITNODIGINGEN = Arrays.asList(
		BriefType.MAMMA_AFSPRAAK_UITNODIGING, BriefType.MAMMA_AFSPRAAK_VERZET,
		BriefType.MAMMA_OPEN_UITNODIGING,
		BriefType.MAMMA_UITNODIGING_MINDER_VALIDE,
		BriefType.MAMMA_UITNODIGING_SUSPECT,
		BriefType.MAMMA_UITNODIGING_TEHUIS_ZONDER_DATUM,
		BriefType.MAMMA_UITSTEL);

	public static final List<BriefType> MAMMA_OPEN_UITNODIGINGEN = Arrays.asList(
		BriefType.MAMMA_OPEN_UITNODIGING,
		BriefType.MAMMA_UITNODIGING_MINDER_VALIDE,
		BriefType.MAMMA_UITNODIGING_SUSPECT,
		BriefType.MAMMA_UITNODIGING_TEHUIS_ZONDER_DATUM);

	private static final List<BriefType> MAMMA_OVERIGE_UITSLAGEN = Arrays.asList(MAMMA_GUNSTIGE_UITSLAG, MAMMA_GEEN_ONDERZOEK, MAMMA_BEPERKT_BEOORDEELBAAR,
		MAMMA_BEPERKT_BEOORDEELBAAR_PROTHESE, MAMMA_GEEN_BEOORDELING_MOGELIJK);

	private static final List<BriefType> MAMMA_ONGUNSTIGE_UITSLAGEN = Arrays.asList(MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_0, MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_4_5,
		MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_4_5_ZONDER_HUISARTS, MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_0_ZONDER_HUISARTS, MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_0,
		MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_4_5, MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_4_5_ZONDER_HUISARTS,
		MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_0_ZONDER_HUISARTS);

	private static final List<BriefType> MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAGEN = Arrays.asList(MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_0,
		MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_4_5, MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_4_5_ZONDER_HUISARTS,
		MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_0_ZONDER_HUISARTS);

	private static final List<BriefType> MAMMA_ONGUNSTIGE_UITSLAGEN_ZONDER_HUISARTS = Arrays.asList(MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_4_5_ZONDER_HUISARTS,
		MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_0_ZONDER_HUISARTS, MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_4_5_ZONDER_HUISARTS,
		MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_0_ZONDER_HUISARTS);

	public static final List<BriefType> COLON_UITSLAG_BRIEVEN = Arrays.asList(COLON_GUNSTIGE_UITSLAG, COLON_UITSLAGBRIEF_ONBEOORDEELBAAR_BUITEN_DOELGROEP,
		COLON_UITSLAGBRIEF_ONGUNSTIGE_BUITEN_DOELGROEP, COLON_UITNODIGING_INTAKE, COLON_UITSLAGBRIEF_EXTRA_MONSTER);

	public static final List<BriefType> COLON_BRIEVEN_GEEN_INTAKE_NODIG = Arrays.asList(COLON_UITNODIGING_INTAKE, COLON_UITSLAGBRIEF_ONGUNSTIGE_BUITEN_DOELGROEP);

	private static final List<BriefType> COLON_CONCLUSIE_BRIEVEN = Arrays.asList(COLON_BEVESTIGING_DEFINITIEVE_EXCLUSIE, COLON_BEVESTIGING_TERUG_NAAR_SCREENING,
		COLON_INTAKE_NO_SHOW);

	private static final List<BriefType> CERVIX_UITNODIGINGEN = Arrays.asList(CERVIX_UITNODIGING, CERVIX_UITNODIGING_30, CERVIX_UITNODIGING_35, CERVIX_UITNODIGING_40,
		CERVIX_UITNODIGING_45, CERVIX_UITNODIGING_50, CERVIX_UITNODIGING_55, CERVIX_UITNODIGING_60, CERVIX_UITNODIGING_65);

	private static final List<BriefType> CERVIX_UITSTRIJKJE_OVERIG = Arrays.asList(BriefType.CERVIX_UITNODIGING_CONTROLEUITSTRIJKJE,
		BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_HPV_ONBEOORDEELBAAR, BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_CYTOLOGIE_ONBEOORDEELBAAR,
		BriefType.CERVIX_UITSTRIJKJE_TWEEDE_KEER_ONBEOORDEELBAAR, BriefType.CERVIX_CYTOLOGIE_ONBEOORDEELBAAR, BriefType.CERVIX_ZAS_HPV_POSITIEF,
		BriefType.CERVIX_ZAS_TWEEDE_KEER_ONBEOORDEELBAAR, BriefType.CERVIX_ZAS_NA_HPV_POSITIEF, BriefType.CERVIX_ZAS_NA_CYTOLOGIE_ONBEOORDEELBAAR);

	private static final List<BriefType> CERVIX_ZAS_BRIEVEN = Arrays.asList(BriefType.CERVIX_ZAS_UITNODIGING, BriefType.CERVIX_ZAS_NIET_ANALYSEERBAAR_OF_ONBEOORDEELBAAR,
		BriefType.CERVIX_ZAS_COMBI_UITNODIGING_30, BriefType.CERVIX_ZAS_NON_RESPONDER);

	private static final List<BriefType> CERVIX_ZAS_NIET_DIRECT_HERZENDBAAR = List.of(BriefType.CERVIX_ZAS_NON_RESPONDER, BriefType.CERVIX_ZAS_COMBI_UITNODIGING_30);

	public static final List<BriefType> CERVIX_UITSLAG_BRIEVEN = Arrays.asList(
		BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_HPV_ONBEOORDEELBAAR,
		BriefType.CERVIX_CYTOLOGIE_ONBEOORDEELBAAR,
		BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_CYTOLOGIE_ONBEOORDEELBAAR,
		BriefType.CERVIX_ZAS_NIET_ANALYSEERBAAR_OF_ONBEOORDEELBAAR,
		BriefType.CERVIX_ZAS_TWEEDE_KEER_ONBEOORDEELBAAR,
		BriefType.CERVIX_ZAS_TWEEDE_KEER_ONBEOORDEELBAAR,
		BriefType.CERVIX_UITSTRIJKJE_TWEEDE_KEER_ONBEOORDEELBAAR,
		BriefType.CERVIX_HPV_NEGATIEF_VOLGENDE_RONDE_5_JAAR,
		BriefType.CERVIX_HPV_NEGATIEF_LAATSTE_RONDE,
		BriefType.CERVIX_HPV_NEGATIEF_VOLGENDE_RONDE_10_JAAR,
		BriefType.CERVIX_ZAS_HPV_POSITIEF,
		BriefType.CERVIX_CYTOLOGIE_NEGATIEF,
		BriefType.CERVIX_CYTOLOGIE_LICHTE_AFWIJKING,
		BriefType.CERVIX_CYTOLOGIE_AFWIJKING,
		BriefType.CERVIX_VOLGEND_MONSTER_CYTOLOGIE_NEGATIEF,
		BriefType.CERVIX_VOLGEND_MONSTER_CYTOLOGIE_LICHTE_AFWIJKING,
		BriefType.CERVIX_VOLGEND_MONSTER_CYTOLOGIE_AFWIJKING,
		BriefType.CERVIX_HUISARTS_ONBEKEND,
		BriefType.CERVIX_CONTROLEUITSTRIJKJE_NEGATIEF,
		BriefType.CERVIX_CONTROLEUITSTRIJKJE_LICHTE_AFWIJKING,
		BriefType.CERVIX_CONTROLEUITSTRIJKJE_AFWIJKING,
		BriefType.CERVIX_CYTOLOGIE_LICHTE_AFWIJKING_HPVOTHER,
		BriefType.CERVIX_VOLGEND_MONSTER_CYTOLOGIE_LICHTE_AFWIJKING_HPVOTHER,
		BriefType.CERVIX_MONSTER_NA_HPV_NEGATIEF,
		BriefType.CERVIX_ZAS_NA_HPV_POSITIEF,
		BriefType.CERVIX_ZAS_NA_CYTOLOGIE_ONBEOORDEELBAAR,
		BriefType.CERVIX_ZAS_NA_CYTOLOGIE_NEGATIEF,
		BriefType.CERVIX_ZAS_NA_CYTOLOGIE_POSITIEF,
		BriefType.CERVIX_ZAS_NA_CYTOLOGIE_POSITIEF_HPVOTHER);

	private final Bevolkingsonderzoek[] onderzoeken;

	private final OrganisatieType verzendendeOrganisatieType;

	private final List<ProjectBriefActieType> briefActieTypes = new ArrayList<>();

	@Getter
	private final String codeEnNaam;

	private final boolean vervangen;

	BriefType(OrganisatieType verzendendeOrganisatieType, ProjectBriefActieType[] actieTypes, boolean vervangen, String codeEnNaam, Bevolkingsonderzoek... onderzoeken)
	{
		this.verzendendeOrganisatieType = verzendendeOrganisatieType;
		this.vervangen = vervangen;
		this.codeEnNaam = codeEnNaam;
		this.onderzoeken = onderzoeken;
		if (actieTypes != null)
		{
			this.briefActieTypes.addAll(Arrays.asList(actieTypes));
		}
	}

	BriefType(OrganisatieType verzendendeOrganisatieType, boolean vervangen, String codeEnNaam, Bevolkingsonderzoek... onderzoeken)
	{
		this(verzendendeOrganisatieType, null, vervangen, codeEnNaam, onderzoeken);
	}

	public OrganisatieType getVerzendendeOrganisatieType()
	{
		return verzendendeOrganisatieType;
	}

	public Bevolkingsonderzoek[] getOnderzoeken()
	{
		return onderzoeken;
	}

	public List<ProjectBriefActieType> getBriefActieTypes()
	{
		return briefActieTypes;
	}

	public String getBriefCode()
	{
		return codeEnNaam.substring(0, codeEnNaam.indexOf(" "));
	}

	public String getWeergaveNaam()
	{
		if (!isActief())
		{
			return codeEnNaam + " (niet meer in gebruik)";
		}
		return codeEnNaam;
	}

	public List<BriefType> getMagNietOpZelfdeDagAfgedruktTypes()
	{
		return vervangen ? Collections.singletonList(this) : Collections.emptyList();
	}

	public static List<BriefType> getBriefTypes(Bevolkingsonderzoek... onderzoeken)
	{
		return getBriefTypes(false, onderzoeken);
	}

	public static List<BriefType> getBriefTypes(boolean exactMatch, Bevolkingsonderzoek... bvoFilter)
	{
		List<BriefType> briefTypes = new ArrayList<>();

		List<Bevolkingsonderzoek> filterBvos = new ArrayList<>(Arrays.asList(bvoFilter));
		Collections.sort(filterBvos);
		for (BriefType type : values())
		{
			List<Bevolkingsonderzoek> briefTypeBvos = new ArrayList<>(Arrays.asList(type.getOnderzoeken()));
			Collections.sort(briefTypeBvos);
			if (!exactMatch && CollectionUtils.intersection(briefTypeBvos, filterBvos).size() > 0 || 
				exactMatch && briefTypeBvos.equals(filterBvos))
			{
				briefTypes.add(type);
			}
		}
		return briefTypes;
	}

	public static List<BriefType> getBriefTypesMetOrganisatieType(boolean exactMatch, OrganisatieType organisatieType, Bevolkingsonderzoek... bvoFilter)
	{
		List<BriefType> briefTypes = new ArrayList<>();

		List<Bevolkingsonderzoek> filterBvos = new ArrayList<>(Arrays.asList(bvoFilter));
		Collections.sort(filterBvos);
		for (BriefType type : values())
		{
			List<Bevolkingsonderzoek> briefTypeBvos = new ArrayList<>(Arrays.asList(type.getOnderzoeken()));
			Collections.sort(briefTypeBvos);
			boolean isOrganisatieType = organisatieType.equals(type.getVerzendendeOrganisatieType());
			if (isOrganisatieType && !exactMatch && CollectionUtils.intersection(briefTypeBvos, filterBvos).size() > 0 || 
				isOrganisatieType && exactMatch && briefTypeBvos.equals(filterBvos))
			{
				briefTypes.add(type);
			}
		}
		return briefTypes;
	}

	public static List<BriefType> getBriefTypesMetOrganisatieType(OrganisatieType organisatieType)
	{
		List<BriefType> briefTypes = new ArrayList<>();
		for (BriefType type : values())
		{
			List<Bevolkingsonderzoek> briefTypeBvos = new ArrayList<>(Arrays.asList(type.getOnderzoeken()));
			Collections.sort(briefTypeBvos);
			if (organisatieType.equals(type.getVerzendendeOrganisatieType()))
			{
				briefTypes.add(type);
			}
		}
		return briefTypes;
	}

	public static List<BriefType> getMammaUitslagBriefTypen()
	{
		return Stream.concat(MAMMA_OVERIGE_UITSLAGEN.stream(), MAMMA_ONGUNSTIGE_UITSLAGEN.stream()).collect(Collectors.toList());
	}

	public static List<BriefType> getMammaOngunstigeUitslagBriefTypen()
	{
		return MAMMA_ONGUNSTIGE_UITSLAGEN;
	}

	public static List<BriefType> getMammaFotobesprekingOngunstigeUitslagen()
	{
		return MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAGEN;
	}

	public static List<BriefType> getMammaOngunstigeUitslagenZonderHuisarts()
	{
		return MAMMA_ONGUNSTIGE_UITSLAGEN_ZONDER_HUISARTS;
	}

	public static List<BriefType> getMammaGunstigeUitslagBriefTypen()
	{
		List<BriefType> uitslagBriefTypes = new ArrayList<>(MAMMA_OVERIGE_UITSLAGEN);
		uitslagBriefTypes.remove(BriefType.MAMMA_GEEN_ONDERZOEK);
		return uitslagBriefTypes;
	}

	public static List<BriefType> getMammaEersteRondeBrieftype()
	{
		return Arrays.asList(MAMMA_AFSPRAAK_UITNODIGING, MAMMA_OPEN_UITNODIGING, MAMMA_UITNODIGING_MINDER_VALIDE, MAMMA_UITNODIGING_TEHUIS_ZONDER_DATUM);
	}

	public static List<BriefType> getMammaBriefApart()
	{
		return Arrays.asList(MAMMA_OPEN_UITNODIGING, MAMMA_UITNODIGING_MINDER_VALIDE, MAMMA_UITNODIGING_TEHUIS_ZONDER_DATUM, MAMMA_AFSPRAAK_UITNODIGING, MAMMA_UITNODIGING_SUSPECT,
			MAMMA_AFSPRAAK_VERZET);
	}

	public static boolean isMammaUitslagBrief(BriefType briefType)
	{
		List<BriefType> uitslagBriefTypes = new ArrayList<>(getMammaOngunstigeUitslagBriefTypen());
		uitslagBriefTypes.addAll(MAMMA_OVERIGE_UITSLAGEN);
		return uitslagBriefTypes.contains(briefType);
	}

	public static List<BriefType> getMammaUitnodigingenBriefTypen()
	{
		return MAMMA_UITNODIGINGEN;
	}

	public static boolean isMammaUitnodigingBrief(BriefType briefType)
	{
		return getMammaUitnodigingenBriefTypen().contains(briefType);
	}

	public static List<BriefType> getColonConclusieBrieven()
	{
		return COLON_CONCLUSIE_BRIEVEN;
	}

	public static List<BriefType> getCervixUitnodigingen()
	{
		return CERVIX_UITNODIGINGEN;
	}

	public static List<BriefType> getCervixUitstrijkjeBrieven()
	{
		List<BriefType> uitstrijkjeBriefTypes = new ArrayList<>(CERVIX_UITNODIGINGEN);
		uitstrijkjeBriefTypes.addAll(CERVIX_UITSTRIJKJE_OVERIG);
		return uitstrijkjeBriefTypes;
	}

	public static List<BriefType> getCervixZasBrieven()
	{
		return CERVIX_ZAS_BRIEVEN;
	}

	public static List<BriefType> getCervixZasUitnodigingNietDirectHerzendbaarBrieven()
	{
		return CERVIX_ZAS_NIET_DIRECT_HERZENDBAAR;
	}

	public boolean isActief()
	{
		boolean isActief = true;
		try
		{
			Field f = getClass().getField(name());
			Deprecated d = f.getAnnotation(Deprecated.class);
			isActief = d == null;
		}
		catch (NoSuchFieldException | SecurityException e)
		{
		}
		return isActief;
	}

}
