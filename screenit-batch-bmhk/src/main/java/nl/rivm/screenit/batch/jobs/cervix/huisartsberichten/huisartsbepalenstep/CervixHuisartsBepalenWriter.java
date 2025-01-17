package nl.rivm.screenit.batch.jobs.cervix.huisartsberichten.huisartsbepalenstep;

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

import java.time.temporal.ChronoUnit;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.cervix.huisartsberichten.CervixHuisartsberichtenConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixHuisartsBepalenWriter extends BaseWriter<CervixHuisartsBericht>
{
	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier dateSupplier;

	private final HibernateService hibernateService;

	@Override
	protected void write(CervixHuisartsBericht huisartsBericht) throws Exception
	{
		var dagenWachtOpHuisartsBekend = preferenceService.getInteger(PreferenceKey.PERIODE_UITSLAG_NAAR_HUISARTS.toString());
		var dagenGeledenBerichtAangemaakt = ChronoUnit.DAYS.between(DateUtil.toLocalDate(huisartsBericht.getAanmaakDatum()), dateSupplier.getLocalDate());
		if (dagenGeledenBerichtAangemaakt < dagenWachtOpHuisartsBekend)
		{
			var labformulier = huisartsBericht.getUitstrijkje().getLabformulier();
			if (labformulier != null && labformulier.getHuisartsLocatie() != null
				&& (labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD || labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE))
			{
				huisartsBericht.setHuisartsLocatie(labformulier.getHuisartsLocatie());
				aantalContextOphogen(CervixHuisartsberichtenConstants.HUISARTS_BEPAALD);
				hibernateService.saveOrUpdate(huisartsBericht);
				return;
			}
			aantalContextOphogen(CervixHuisartsberichtenConstants.HUISARTS_KON_NIET_WORDEN_BEPAALD);
		}
		else
		{
			huisartsBericht.setStatus(CervixHuisartsBerichtStatus.HUISARTS_ONBEKEND);
			huisartsBericht.setStatusDatum(dateSupplier.getDate());
			hibernateService.saveOrUpdate(huisartsBericht);
			aantalContextOphogen(CervixHuisartsberichtenConstants.HUISARTS_ONBEKEND);
		}
	}
}
