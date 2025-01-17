package nl.rivm.screenit.batch.jobs.cervix.gevolgenlabprocesverwerken;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.CervixGevolgenLabprocesVerwerkenBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixGevolgenLabprocesVerwerkenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixGevolgenLabprocesVerwerkenRapportageBriefType;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixGevolgenLabprocesVerwerkenRapportageHuisartsberichtType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixGevolgenLabprocesVerwerkenJobListener extends BaseLogListener
{
	private final ICurrentDateSupplier dateSupplier;

	private final HibernateService hibernateService;

	private final SimplePreferenceService preferenceService;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		super.beforeStarting(jobExecution);

		String startdatumAanleveringGenotyperingString = preferenceService.getString(PreferenceKey.CERVIX_START_AANLEVERING_GENOTYPERING_EN_INVOERING_TRIAGE.name());
		getJobExecution().getExecutionContext().putString(PreferenceKey.CERVIX_START_AANLEVERING_GENOTYPERING_EN_INVOERING_TRIAGE.name(), startdatumAanleveringGenotyperingString);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_GEVOLGEN_LABPROCES_VERWERKEN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_GEVOLGEN_LABPROCES_VERWERKEN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new CervixGevolgenLabprocesVerwerkenBeeindigdLogEvent();
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.CERVIX;
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var context = jobExecution.getExecutionContext();
		var rapportage = new CervixGevolgenLabprocesVerwerkenRapportage();

		rapportage.setDatumVerwerking(dateSupplier.getDate());
		rapportage.setAantalInLabproces(context.getLong(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_IN_LABPROCES_KEY, 0));
		rapportage.setAantalDefinitiefHeraangemeld(context.getLong(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_DEFINITIEF_HERAANGEMELD_KEY, 0));
		rapportage.setAantalEenmaligHeraangemeld(context.getLong(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_EENMALIG_HERAANGEMELD_KEY, 0));
		rapportage.setAantalUitnodigingenUitstrijkje(context.getLong(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_UITNODIGINGEN_UITSTRIJKJE_KEY, 0));
		rapportage.setAantalUitnodigingenZas(context.getLong(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_UITNODIGINGEN_ZAS_KEY, 0));
		rapportage.setAantalInVervolgonderzoek(context.getLong(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_IN_VERVOLGONDERZOEK_KEY, 0));
		rapportage.setAantalRondenGesloten(context.getLong(CervixGevolgenLabprocesVerwerkenConstants.AANTAL_RONDEN_GESLOTEN_KEY, 0));
		rapportage.setTotaalAantalBrieven(context.getLong(CervixGevolgenLabprocesVerwerkenConstants.TOTAAL_AANTAL_BRIEVEN_KEY, 0));
		rapportage.setTotaalAantalHuisartsberichten(context.getLong(CervixGevolgenLabprocesVerwerkenConstants.TOTAAL_AANTAL_HUISARTSBERICHTEN_KEY, 0));

		aantallenContextVerwerken(CervixGevolgenLabprocesVerwerkenConstants.BRIEF_TYPE_KEY, new AantalVerwerker<BriefType>()
		{
			@Override
			public void verwerk(BriefType briefType, long aantal)
			{
				rapportage.getBriefTypen().add(new CervixGevolgenLabprocesVerwerkenRapportageBriefType(rapportage, briefType, aantal));
			}
		});

		aantallenContextVerwerken(CervixGevolgenLabprocesVerwerkenConstants.HUISARTSBERICHT_TYPE_KEY, new AantalVerwerker<HuisartsBerichtType>()
		{
			@Override
			protected void verwerk(HuisartsBerichtType huisartsberichtType, long aantal)
			{
				rapportage.getHuisartsberichtTypen().add(new CervixGevolgenLabprocesVerwerkenRapportageHuisartsberichtType(rapportage, huisartsberichtType, aantal));
			}
		});

		var gevolgenLabprocesVerwerkenBeeindigdLogEvent = (CervixGevolgenLabprocesVerwerkenBeeindigdLogEvent) super.eindLogging(jobExecution);
		gevolgenLabprocesVerwerkenBeeindigdLogEvent.setRapportage(rapportage);

		hibernateService.saveOrUpdate(rapportage);
		return gevolgenLabprocesVerwerkenBeeindigdLogEvent;
	}
}
