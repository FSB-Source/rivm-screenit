package nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.onafgerondeverwijzing;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class OnbevestigdeIntakeVerwijzingenWriter extends BaseWriter<ColonIntakeAfspraak>
{

	private final HibernateService hibernateService;

	private final AfspraakService afspraakService;

	private final LogService logService;

	@Override
	protected void write(ColonIntakeAfspraak afspraak) throws Exception
	{
		afspraakService.setAfspraakStatus(afspraak, AfspraakStatus.VERPLAATST);
		var conclusie = afspraak.getConclusie();
		afspraak.setConclusie(null);
		hibernateService.saveOrUpdate(afspraak);
		hibernateService.delete(conclusie);

		String melding = "Conclusie: " + conclusie.getType().getOmschrijving()
			+ " verwijderd en afspraak van verwijzende intakelocatie wordt verplaatst, omdat mogelijke verwijzing niet bevestigd is door verwijzende intakelocatie.";
		logService.logGebeurtenis(LogGebeurtenis.CONCLUSIE_VERWIJDEREN, afspraak.getClient(), melding, Bevolkingsonderzoek.COLON);
	}
}
