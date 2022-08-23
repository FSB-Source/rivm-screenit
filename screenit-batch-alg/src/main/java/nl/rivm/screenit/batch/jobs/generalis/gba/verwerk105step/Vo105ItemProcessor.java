package nl.rivm.screenit.batch.jobs.generalis.gba.verwerk105step;

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

import java.util.EnumMap;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.enums.GbaVraagType;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo105Bericht;
import nl.topicuszorg.gba.vertrouwdverbonden.model.enums.BerichtType;
import nl.topicuszorg.gba.vertrouwdverbonden.model.enums.Vo105_ArecordVeld;
import nl.topicuszorg.gba.vertrouwdverbonden.model.utils.VoxHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class Vo105ItemProcessor implements ItemProcessor<Long, Vo105Bericht>
{
	@Autowired
	private HibernateService hibernateService;

	private StepExecution stepExecution;

	@Override
	public Vo105Bericht process(Long item)
	{
		var gbaVraag = hibernateService.load(GbaVraag.class, item);
		Vo105Bericht bericht = berichtVoorGbaVraag(gbaVraag);
		gbaVraag.setUniqueBatchId(stepExecution.getJobExecution().getId().toString());
		hibernateService.saveOrUpdate(gbaVraag);
		return bericht;
	}

	private Vo105Bericht berichtVoorGbaVraag(GbaVraag vraag)
	{
		String bsn = vraag.getClient() != null ? vraag.getClient().getPersoon().getBsn() : vraag.getBsn();
		BerichtType berichtType = vraag.getVraagType() == GbaVraagType.PLAATS_INDICATIE ? BerichtType.AP01 : BerichtType.AV01;
		return maakBericht(berichtType, bsn);
	}

	protected Vo105Bericht maakBericht(BerichtType berichtType, String bsn)
	{
		Map<Vo105_ArecordVeld, String> stuurgegevens = new EnumMap<>(Vo105_ArecordVeld.class);
		stuurgegevens.put(Vo105_ArecordVeld.SOFINR, bsn);
		stuurgegevens.put(Vo105_ArecordVeld.INTAFN, ""); 
		return VoxHelper.createVo105GerichtWithAnrOrBSN(null, bsn, berichtType, null, stuurgegevens);
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}
}
