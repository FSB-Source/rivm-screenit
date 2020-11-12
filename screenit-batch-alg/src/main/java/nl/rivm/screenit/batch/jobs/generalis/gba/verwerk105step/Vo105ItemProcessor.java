
package nl.rivm.screenit.batch.jobs.generalis.gba.verwerk105step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.batch.jobs.generalis.gba.wrappers.Vo105BerichtWrapper;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.GbaVraagType;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo105Bericht;
import nl.topicuszorg.gba.vertrouwdverbonden.model.enums.BerichtType;
import nl.topicuszorg.gba.vertrouwdverbonden.model.enums.Vo105_ArecordVeld;
import nl.topicuszorg.gba.vertrouwdverbonden.model.utils.VoxHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;

public class Vo105ItemProcessor implements ItemProcessor<Long, List<Vo105BerichtWrapper>>
{
	
	private static final Logger LOG = LoggerFactory.getLogger(Vo105ItemProcessor.class);

	@Autowired
	private InstellingService instellingService;

	@Autowired
	private HibernateService hibernateService;

	private StepExecution stepExecution;

	@Override
	public List<Vo105BerichtWrapper> process(Long item)
	{
		GbaVraag vraag = hibernateService.load(GbaVraag.class, item);
		String bsn = null;
		if (vraag.getClient() != null)
		{
			bsn = vraag.getClient().getPersoon().getBsn();
		}
		else
		{
			bsn = vraag.getBsn();
		}

		List<Vo105BerichtWrapper> berichtWrappers = new ArrayList<>();

		if (vraag.getVraagType() == GbaVraagType.PLAATS_INDICATIE)
		{
			ScreeningOrganisatie screeningOrganisatie = vraag.getScreeningOrganisatie();
			if (screeningOrganisatie != null)
			{
				Vo105BerichtWrapper berichtWrapper = getBerichtWrapper(vraag, BerichtType.AP01, bsn, screeningOrganisatie);
				berichtWrappers.add(berichtWrapper);
			}
			else
			{
				LOG.error("Geen screeningsorganisatie bekend voor plaatsindicatie (AP01) bericht. Vraag niet verstuurd: " + vraag.getId());
			}
		}
		else
		{
			List<ScreeningOrganisatie> screeningOrganisaties = instellingService.getActieveInstellingen(ScreeningOrganisatie.class);
			for (ScreeningOrganisatie screeningOrganisatie : screeningOrganisaties)
			{
				Vo105BerichtWrapper berichtWrapper = getBerichtWrapper(vraag, BerichtType.AV01, bsn, screeningOrganisatie);
				berichtWrappers.add(berichtWrapper);
			}
		}

		vraag.setUniqueBatchId(stepExecution.getJobExecution().getId().toString());
		hibernateService.saveOrUpdate(vraag);

		return berichtWrappers;
	}

	protected Vo105BerichtWrapper getBerichtWrapper(GbaVraag vraag, BerichtType berichtType, String bsn, ScreeningOrganisatie screeningOrganisatie)
	{
		Map<Vo105_ArecordVeld, String> stuurgegevens = new HashMap<>();
		stuurgegevens.put(Vo105_ArecordVeld.INTAFN, "COLON");
		stuurgegevens.put(Vo105_ArecordVeld.SOFINR, bsn);
		stuurgegevens.put(Vo105_ArecordVeld.COMM, "BVO" + screeningOrganisatie.getRegioCode() + vraag.getId());
		Vo105Bericht vo105Bericht = VoxHelper.createVo105GerichtWithAnrOrBSN(null, bsn, berichtType, null, stuurgegevens);

		Vo105BerichtWrapper berichtWrapper = new Vo105BerichtWrapper();
		berichtWrapper.setScreeningOrganisatie(screeningOrganisatie);
		berichtWrapper.setVo105Bericht(vo105Bericht);
		return berichtWrapper;
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}
}
