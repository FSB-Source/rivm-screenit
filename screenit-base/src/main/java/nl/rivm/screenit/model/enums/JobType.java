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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.OrganisatieParameterKey;

import static nl.rivm.screenit.model.enums.Bevolkingsonderzoek.CERVIX;
import static nl.rivm.screenit.model.enums.Bevolkingsonderzoek.COLON;
import static nl.rivm.screenit.model.enums.Bevolkingsonderzoek.MAMMA;
import static nl.rivm.screenit.model.enums.JobFlag.BLOCK_CRON_TRIGGER;
import static nl.rivm.screenit.model.enums.JobFlag.BLOCK_MANUAL_START;

public enum JobType
{
	GBA(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	GBA_ZONDER_VO105(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	COLON_NA_GBA(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	CLIENT_SELECTIE(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM_JOB_DK(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	IFOBT_VERWERKING(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	IFOBT_HERINNERING(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	KOPPELDATA_VERWERKING(new JobFlag[] { BLOCK_MANUAL_START, BLOCK_CRON_TRIGGER }, BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	GUNSTIGE_UITSLAG(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	INTAKE_AFSPRAKEN_MAKEN(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	COLON_HUISARTSBERICHTEN_OPNIEUW_VERSTUREN(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	VERVOLG_INTAKE_CONCLUSIE_BATCH(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	HUISARTS_ONTKOPPELEN_JOB_DK(new JobFlag[] { BLOCK_MANUAL_START, BLOCK_CRON_TRIGGER }, BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	ONEINDIGE_ROOSTERITEMS_UITROLLEN(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	BRIEVEN_GENEREREN(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	COLON_ILM(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	COLON_ROOSTER_GEEN_CAPACITEIT_SIGNALERING(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	COLON_CONTROLE_MISSENDE_UITSLAGEN(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON },
		new OrganisatieParameterKey[] { OrganisatieParameterKey.COLON_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN }),

	CERVIX_NA_GBA(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_SELECTIE(
		BatchApplicationType.CERVIX,
		new Bevolkingsonderzoek[] { CERVIX },
		new OrganisatieParameterKey[] { OrganisatieParameterKey.CERVIX_MAX_AANTAL_CLIENTEN_SELECTIE }),

	CERVIX_KOPPELDATA_VERWERKING(new JobFlag[] { BLOCK_MANUAL_START, BLOCK_CRON_TRIGGER }, BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_ILM(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_ZAS_UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM(
		BatchApplicationType.CERVIX,
		new Bevolkingsonderzoek[] { CERVIX },
		new OrganisatieParameterKey[] { OrganisatieParameterKey.CERVIX_MAX_AANTAL_ZAS_NAAR_INPAKCENTRUM }),

	CERVIX_VERVOLGONDERZOEK(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_UITSTEL(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_HERINNEREN(
		BatchApplicationType.CERVIX,
		new Bevolkingsonderzoek[] { CERVIX },
		new OrganisatieParameterKey[] { OrganisatieParameterKey.CERVIX_MAX_AANTAL_HERINNERINGEN_UITSTRIJKJE, OrganisatieParameterKey.CERVIX_MAX_AANTAL_HERINNERINGEN_ZAS,
			OrganisatieParameterKey.CERVIX_MAX_AANTAL_HERINNERINGEN_ZAS_NON_RESPONDER, OrganisatieParameterKey.CERVIX_MAX_AANTAL_HERINNERINGEN_ZAS_PU }),

	CERVIX_HUISARTSBERICHTEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_GEVOLGEN_LABPROCES_VERWERKEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_BRIEVEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	REGIO_BRIEVEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_ORDER(BatchApplicationType.CERVIX,
		new Bevolkingsonderzoek[] { CERVIX },
		new OrganisatieParameterKey[] { OrganisatieParameterKey.CERVIX_ORDER_NIEUWE_STIJL }),

	CERVIX_HPV_ORU(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_BEPALEN_VERRICHTINGEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_HPVMIN_VERWIJDEREN_UITSLAG(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_HEROVERWEGERS(
		BatchApplicationType.CERVIX,
		new Bevolkingsonderzoek[] { CERVIX },
		new OrganisatieParameterKey[] { OrganisatieParameterKey.CERVIX_MAX_AANTAL_HEROVERWEGERS }),

	CERVIX_CISMIGRANTEN_UITNODIGEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_VERLATE_DEELNAME_COVID19(
		BatchApplicationType.CERVIX,
		new Bevolkingsonderzoek[] { CERVIX },
		new OrganisatieParameterKey[] { OrganisatieParameterKey.CERVIX_MAX_AANTAL_CLIENTEN_VERLATE_DEELNAME, OrganisatieParameterKey.CERVIX_PROJECT_VERLATE_DEELNAME }),

	CERVIX_OUDE_NIET_INGESTUURDE_ZAS(
		BatchApplicationType.CERVIX,
		new Bevolkingsonderzoek[] { CERVIX },
		new OrganisatieParameterKey[] { OrganisatieParameterKey.CERVIX_MAX_AANTAL_CLIENTEN_OUDE_ZAS, OrganisatieParameterKey.CERVIX_PROJECT_OUDE_ZAS }),

	CERVIX_CONTROLE_MISSENDE_UITSLAGEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX },
		new OrganisatieParameterKey[] { OrganisatieParameterKey.CERVIX_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN }),

	MAMMA_NA_GBA(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_KANSBEREKENING(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_UITNODIGEN(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_HERINNEREN(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_BRIEVEN(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_BEOORDELINGEN_ACCORDEREN(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_BEOORDELINGEN_DOORZETTEN_ARBITRAGE(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_HUISARTS_BERICHTEN_OPNIEUW_VERSTUREN(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_VERVOLG_ONDERZOEKEN(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_CONCEPT_MODEL(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_UITWISSELPORTAAL(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_ILM(
		BatchApplicationType.MAMMA,
		new Bevolkingsonderzoek[] { MAMMA },
		new OrganisatieParameterKey[] { OrganisatieParameterKey.MAMMA_ILM_BEELDEN_STATUS_SIGNALEREN_UITVOEREN,
			OrganisatieParameterKey.MAMMA_ILM_GUNSTIGE_BEELDEN_VERWIJDEREN_UITVOEREN,
			OrganisatieParameterKey.MAMMA_ILM_OVERIGE_BEELDEN_VERWIJDEREN_UITVOEREN,
			OrganisatieParameterKey.MAMMA_ILM_PALGA_IMPORT_VERSLAGEN_VERWIJDEREN_UITVOEREN,
			OrganisatieParameterKey.MAMMA_ILM_APPLICATIE_LOGGING_VERWIJDEREN_UITVOEREN,
			OrganisatieParameterKey.MAMMA_ILM_RONDES_VERWIJDEREN_UITVOEREN,
			OrganisatieParameterKey.MAMMA_ILM_MAX_TIJD_MINUTEN }),

	MAMMA_XDS(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_PALGA_CSV_EXPORT(new JobFlag[] { BLOCK_CRON_TRIGGER }, BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_PALGA_CSV_IMPORT(new JobFlag[] { BLOCK_CRON_TRIGGER }, BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_CONTROLE_MISSENDE_UITSLAGEN(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA },
		new OrganisatieParameterKey[] { OrganisatieParameterKey.MAMMA_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN }),

	COORDINATEN(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	BEZWAAR_BRIEVEN(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	ALGEMENE_BRIEVEN(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	PROJECT_BRIEVEN(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	ILM_ALGEMENE_GEGEVENS_VERWIJDEREN(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	WACHTWOORD_VERLOOPT_HERINNERING(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	ENOVATION_HUISARTSEN_BATCH(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, MAMMA }),

	SIGNALERING_GENDER(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { CERVIX, MAMMA }),

	;

	private static Set<JobType> getJobTypes(BatchApplicationType batchApplicationType)
	{
		return Arrays.stream(JobType.values())
			.filter(jobType -> jobType.batchApplicationType == batchApplicationType)
			.collect(Collectors.toSet());
	}

	static
	{
		Set<JobType> colon = getJobTypes(BatchApplicationType.COLON);
		Set<JobType> cervix = getJobTypes(BatchApplicationType.CERVIX);
		Set<JobType> mamma = getJobTypes(BatchApplicationType.MAMMA);
		Set<JobType> generalis = getJobTypes(BatchApplicationType.GENERALIS);

		Set<JobType> colonCervix = new HashSet<>();
		colonCervix.addAll(colon);
		colonCervix.addAll(cervix);

		Set<JobType> colonMamma = new HashSet<>();
		colonMamma.addAll(colon);
		colonMamma.addAll(mamma);

		Set<JobType> cervixMamma = new HashSet<>();
		cervixMamma.addAll(cervix);
		cervixMamma.addAll(mamma);

		Set<JobType> inpakcentrum = EnumSet.of(UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM_JOB_DK, CERVIX_ZAS_UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM);

		for (JobType jobType : colon)
		{
			jobType.parallelleJobs.addAll(cervixMamma);
			if (jobType == JobType.UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM_JOB_DK)
			{
				jobType.parallelleJobs.removeAll(inpakcentrum);
			}
		}

		for (JobType jobType : cervix)
		{
			jobType.parallelleJobs.addAll(colonMamma);
			if (jobType == JobType.CERVIX_ZAS_UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM)
			{
				jobType.parallelleJobs.removeAll(inpakcentrum);
			}
		}

		for (JobType jobType : mamma)
		{
			jobType.parallelleJobs.addAll(colonCervix);
			if (jobType == JobType.MAMMA_KANSBEREKENING)
			{
				jobType.parallelleJobs.addAll(generalis);
			}
		}

		for (JobType jobType : generalis)
		{
			jobType.parallelleJobs.add(MAMMA_KANSBEREKENING);
		}
	}

	private final BatchApplicationType batchApplicationType;

	private final Bevolkingsonderzoek[] bevolkingsOnderzoeken;

	private final JobFlag[] jobFlags;

	private final OrganisatieParameterKey[] jobParameters;

	private final Set<JobType> parallelleJobs = new HashSet<>();

	JobType(BatchApplicationType batchApplicationType, Bevolkingsonderzoek[] bevolkingsOnderzoeken)
	{
		this(batchApplicationType, bevolkingsOnderzoeken, null);
	}

	JobType(BatchApplicationType batchApplicationType, Bevolkingsonderzoek[] bevolkingsOnderzoeken, OrganisatieParameterKey[] jobParameters)
	{
		this(null, batchApplicationType, bevolkingsOnderzoeken, jobParameters);
	}

	JobType(JobFlag[] jobFlags, BatchApplicationType batchApplicationType, Bevolkingsonderzoek[] bevolkingsOnderzoeken)
	{
		this(jobFlags, batchApplicationType, bevolkingsOnderzoeken, null);
	}

	JobType(JobFlag[] jobFlags, BatchApplicationType batchApplicationType, Bevolkingsonderzoek[] bevolkingsOnderzoeken, OrganisatieParameterKey[] jobParameters)
	{
		if (jobParameters != null)
		{
			this.jobParameters = jobParameters.clone();
		}
		else
		{
			this.jobParameters = null;
		}
		if (jobFlags != null)
		{
			this.jobFlags = jobFlags.clone();
		}
		else
		{
			this.jobFlags = null;
		}
		this.batchApplicationType = batchApplicationType;
		if (bevolkingsOnderzoeken != null)
		{
			this.bevolkingsOnderzoeken = bevolkingsOnderzoeken.clone();
		}
		else
		{
			this.bevolkingsOnderzoeken = null;
		}
	}

	public List<JobType> getParallelleJobs()
	{
		List<JobType> jobs = new ArrayList<>();
		if (!parallelleJobs.isEmpty())
		{
			jobs = new ArrayList<>(parallelleJobs);
		}
		return jobs;
	}

	public Bevolkingsonderzoek[] getBevolkingsOnderzoeken()
	{
		return bevolkingsOnderzoeken;
	}

	public BatchApplicationType getBatchApplicationType()
	{
		return batchApplicationType;
	}

	public boolean hasJobFlag(JobFlag searchJobFlag)
	{
		return this.jobFlags != null && Arrays.asList(this.jobFlags).contains(searchJobFlag);
	}

	public OrganisatieParameterKey[] getJobParameters()
	{
		return jobParameters;
	}

}
