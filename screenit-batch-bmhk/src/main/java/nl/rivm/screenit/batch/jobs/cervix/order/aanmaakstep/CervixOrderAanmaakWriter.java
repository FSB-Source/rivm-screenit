package nl.rivm.screenit.batch.jobs.cervix.order.aanmaakstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.cervix.order.CervixOrderConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.service.CervixOrderBerichtService;
import nl.rivm.screenit.dao.cervix.CervixBepaalVervolgDao;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieReden;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.cervix.impl.CervixBepaalVervolgContext;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
@Slf4j
public class CervixOrderAanmaakWriter extends BaseWriter<CervixUitstrijkje>
{

	private LogService logService;

	private CervixOrderBerichtService orderBerichtService;

	private CervixFactory factory;

	private ICurrentDateSupplier dateSupplier;

	private CervixBepaalVervolgDao bepaalVervolgDao;

	private CervixBaseMonsterService monsterService;

	private SimplePreferenceService preferenceService;

	@Override
	protected void write(CervixUitstrijkje uitstrijkje) throws Exception
	{
		try
		{
			var cytologieReden = getCytologieReden(uitstrijkje);
			factory.maakCytologieOrder(uitstrijkje, cytologieReden, maakHL7v2Bericht(uitstrijkje, cytologieReden));

			aantalContextOphogen(CervixOrderConstants.KEY_ORDER_AANGEMAAKT);
		}
		catch (IllegalStateException e)
		{
			logging(uitstrijkje, e.getMessage());
		}
	}

	private String maakHL7v2Bericht(CervixUitstrijkje uitstrijkje, CervixCytologieReden cytologieReden)
	{
		return orderBerichtService.maakCytologieOrderTextBericht(uitstrijkje, cytologieReden);
	}

	private CervixCytologieReden getCytologieReden(CervixUitstrijkje uitstrijkje) throws IllegalStateException
	{
		try
		{
			String startdatumAanleveringGenotyperingString = (String) getJobExecution().getExecutionContext()
				.get(PreferenceKey.CERVIX_START_AANLEVERING_GENOTYPERING_EN_INVOERING_TRIAGE.name());

			var vervolgContext = new CervixBepaalVervolgContext(uitstrijkje, false, dateSupplier.getLocalDateTime(),
				DateUtil.parseLocalDateForPattern(startdatumAanleveringGenotyperingString, Constants.DATE_FORMAT_YYYYMMDD), bepaalVervolgDao, monsterService,
				preferenceService.getInteger(PreferenceKey.CERVIX_INTERVAL_CONTROLE_UITSTRIJKJE.name()));

			if (vervolgContext.inVervolgonderzoekDatum != null)
			{
				if (bepaalVervolgDao.anderUitstrijkjeOnbeoordeelbaarCytologie(vervolgContext.huidigUitstrijkje))
				{
					return CervixCytologieReden.HERHALING_VERVOLGONDERZOEK;
				}
				return CervixCytologieReden.VERVOLGONDERZOEK;
			}

			if (bepaalVervolgDao.anderUitstrijkjeOnbeoordeelbaarCytologie(vervolgContext.huidigUitstrijkje))
			{
				return CervixCytologieReden.HERHALING_INITIEEL_NA_ONBEOORDEELBAARHEID;
			}

			if (vervolgContext.monsterHpvUitslag instanceof CervixUitstrijkje)
			{
				return CervixCytologieReden.INITIEEL_ZONDER_ZAS;
			}
			else
			{
				return CervixCytologieReden.INITIEEL_NA_ZAS;
			}
		}
		catch (Exception e)
		{
			LOG.error("Er is een probleem opgetreden met het bepalen van de cytologie reden.", e);
			throw new IllegalStateException("Er kon voor deze client geen cytologiereden worden bepaald.");
		}
	}

	private void logging(CervixUitstrijkje uitstrijkje, String melding)
	{
		List<Instelling> instellingen = new ArrayList<>();
		instellingen.add(uitstrijkje.getLaboratorium());
		var client = uitstrijkje.getOntvangstScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ORDER_AANMAKEN_MISLUKT, instellingen, client, melding, Bevolkingsonderzoek.CERVIX);
	}
}
