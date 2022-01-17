package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.MammaIlmJobListener;
import nl.rivm.screenit.batch.model.dto.MammaIlmRetryDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class MammaAbstractBeeldenStatusSignalerenWriter<E extends HibernateObject> extends BaseWriter<E>
{
	private final Logger LOG = LoggerFactory.getLogger(this.getClass());

	protected void registreerSignalering(boolean isBezwaar, boolean isUploaded, long accessionNumber, Date statusDatum, Client client)
	{
		LOG.info("Gesignaleerd, accessionNumber: {}, statusDatum: {}, isBezwaar: {}, isUploaded: {}", accessionNumber, statusDatum, isBezwaar, isUploaded);
		List<MammaIlmRetryDto> entries = (List<MammaIlmRetryDto>) getJobExecution().getExecutionContext().get(MammaIlmJobListener.KEY_BEELDEN_STATUS_ENTRIES);
		if (entries == null)
		{
			entries = new ArrayList<>();
			getExecutionContext().put(MammaIlmJobListener.KEY_BEELDEN_STATUS_ENTRIES, entries);
		}
		MammaIlmRetryDto dto = new MammaIlmRetryDto(accessionNumber,
			DateUtil.toUtilDate(DateUtil.toLocalDateTime(statusDatum)),
			client.getId(),
			DateUtil.toUtilDate(DateUtil.toLocalDate(client.getPersoon().getGeboortedatum())),
			isUploaded,
			isBezwaar,
			false);
		entries.add(dto);
	}
}
