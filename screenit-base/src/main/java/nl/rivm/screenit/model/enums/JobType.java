
package nl.rivm.screenit.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import static nl.rivm.screenit.model.enums.Bevolkingsonderzoek.CERVIX;
import static nl.rivm.screenit.model.enums.Bevolkingsonderzoek.COLON;
import static nl.rivm.screenit.model.enums.Bevolkingsonderzoek.MAMMA;
import static nl.rivm.screenit.model.enums.JobFlag.BLOCK_MANUAL_START;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.OrganisatieParameterKey;

public enum JobType
{

	GBA(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	GBA_ZONDER_VO105(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	COLON_NA_GBA(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	CERVIX_NA_GBA(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CLIENT_SELECTIE(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM_JOB_DK(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	IFOBT_INLEZEN(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	IFOBT_VERWERKING(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	IFOBT_HERINNERING(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	COLON_ILM(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	KOPPELDATA_VERWERKING(new JobFlag[] { BLOCK_MANUAL_START }, BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	GUNSTIGE_UITSLAG(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	INTAKE_AFSPRAKEN_MAKEN(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	VERVOLG_INTAKE_CONCLUSIE_BATCH(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	HUISARTS_ONTKOPPELEN_JOB_DK(new JobFlag[] { BLOCK_MANUAL_START }, BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	ENOVATION_HUISARTSEN_BATCH(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, MAMMA }),

	BRIEVEN_GENEREREN(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	CERVIX_SELECTIE(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }, new OrganisatieParameterKey[] { OrganisatieParameterKey.CERVIX_MAX_AANTAL_CLIENTEN_SELECTIE }),

	CERVIX_KOPPELDATA_VERWERKING(new JobFlag[] { BLOCK_MANUAL_START }, BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_ILM(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_ZAS_UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }, new OrganisatieParameterKey[] { OrganisatieParameterKey.CERVIX_MAX_AANTAL_ZAS_NAAR_INPAKCENTRUM }),

	CERVIX_VERVOLGONDERZOEK(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_UITSTEL(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_HERINNEREN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }, new OrganisatieParameterKey[] { OrganisatieParameterKey.CERVIX_MAX_AANTAL_HERINNERINGEN_UITSTRIJKJE, OrganisatieParameterKey.CERVIX_MAX_AANTAL_HERINNERINGEN_ZAS }),

	CERVIX_HUISARTSBERICHTEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_GEVOLGEN_LABPROCES_VERWERKEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_BRIEVEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	PROJECT_BRIEVEN(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	ILM_ALGEMENE_GEGEVENS_VERWIJDEREN(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	ONEINDIGE_ROOSTERITEMS_UITROLLEN(BatchApplicationType.COLON, new Bevolkingsonderzoek[] { COLON }),

	COORDINATEN(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	BEZWAAR_BRIEVEN(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	ALGEMENE_BRIEVEN(BatchApplicationType.GENERALIS, new Bevolkingsonderzoek[] { COLON, CERVIX, MAMMA }),

	REGIO_BRIEVEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_ORDER(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_HPV_ORU(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_BEPALEN_VERRICHTINGEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_HPVMIN_VERWIJDEREN_UITSLAG(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_HEROVERWEGERS(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

	CERVIX_CISMIGRANTEN_UITNODIGEN(BatchApplicationType.CERVIX, new Bevolkingsonderzoek[] { CERVIX }),

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

	MAMMA_ILM(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_XDS(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_PALGA_CSV_EXPORT(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA }),

	MAMMA_PALGA_CSV_IMPORT(BatchApplicationType.MAMMA, new Bevolkingsonderzoek[] { MAMMA });

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
			switch (jobType)
			{
			case UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM_JOB_DK:
				jobType.parallelleJobs.removeAll(inpakcentrum);
				break;
			}
		}

		for (JobType jobType : cervix)
		{
			jobType.parallelleJobs.addAll(colonMamma);
			switch (jobType)
			{
			case CERVIX_ZAS_UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM:
				jobType.parallelleJobs.removeAll(inpakcentrum);
				break;
			}
		}

		for (JobType jobType : mamma)
		{
			jobType.parallelleJobs.addAll(colonCervix);
			switch (jobType)
			{
			case MAMMA_KANSBEREKENING:
				jobType.parallelleJobs.addAll(generalis);
				break;
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

	private Set<JobType> parallelleJobs = new HashSet<>();

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
		if (parallelleJobs != null)
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
		return this.jobFlags != null && Arrays.stream(this.jobFlags).filter(f -> f.equals(searchJobFlag)).findFirst().isPresent();
	}

	public OrganisatieParameterKey[] getJobParameters()
	{
		return jobParameters;
	}

}
