package nl.rivm.screenit.batch.jobs.ilm.applicatielogging;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.logging.RetourzendingLogEvent;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.springframework.stereotype.Component;

@Component
public class IlmApplicatieLoggingVerwijderenWriter extends BaseWriter<LogRegel>
{
	@Override
	protected void write(LogRegel logRegel)
	{
		var logEvent = logRegel.getLogEvent();
		var sanddBestand = logEvent != null && RetourzendingLogEvent.class.equals(HibernateHelper.getDeproxiedClass(logEvent))
			? ((RetourzendingLogEvent) HibernateHelper.deproxy(logEvent)).getSanddBestand()
			: null;
		getHibernateService().delete(logRegel);
		deactiveerSanddBestand(sanddBestand);
	}

	private void deactiveerSanddBestand(UploadDocument sanddBestand)
	{
		if (sanddBestand != null)
		{
			sanddBestand.setActief(false);
		}
	}
}
