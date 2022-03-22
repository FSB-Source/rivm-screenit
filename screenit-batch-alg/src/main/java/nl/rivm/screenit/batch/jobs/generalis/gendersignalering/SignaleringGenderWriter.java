package nl.rivm.screenit.batch.jobs.generalis.gendersignalering;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;

import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public class SignaleringGenderWriter extends BaseWriter<Client>
{
	@Autowired
	private BaseBriefService briefService;

	@Override
	protected void write(Client client) throws Exception
	{
		LOG.info("Brief signalering gender voor client {}", client.getId());
		briefService.maakAlgemeneBrief(client, BriefType.CLIENT_SIGNALERING_GENDER);
		aantalContextOphogen(SignaleringGenderListener.TOTAAL_AANTAL_BRIEVEN_KEY);
	}
}
