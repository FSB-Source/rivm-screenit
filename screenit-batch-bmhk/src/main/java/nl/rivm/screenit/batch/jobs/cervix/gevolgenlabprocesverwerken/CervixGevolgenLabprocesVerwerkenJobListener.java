package nl.rivm.screenit.batch.jobs.cervix.gevolgenlabprocesverwerken;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class CervixGevolgenLabprocesVerwerkenJobListener extends BaseLogListener
{

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private HibernateService hibernateService;

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
		ExecutionContext context = jobExecution.getExecutionContext();
		CervixGevolgenLabprocesVerwerkenRapportage rapportage = new CervixGevolgenLabprocesVerwerkenRapportage();

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

		CervixGevolgenLabprocesVerwerkenBeeindigdLogEvent gevolgenLabprocesVerwerkenBeeindigdLogEvent = (CervixGevolgenLabprocesVerwerkenBeeindigdLogEvent) super.eindLogging(
			jobExecution);
		gevolgenLabprocesVerwerkenBeeindigdLogEvent.setRapportage(rapportage);

		hibernateService.saveOrUpdate(rapportage);
		return gevolgenLabprocesVerwerkenBeeindigdLogEvent;
	}
}
