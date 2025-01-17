package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.MammaIlmJobListener;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public abstract class MammaBeeldenVerwijderenWriter<E extends HibernateObject> extends BaseWriter<E>
{

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	protected void verwijderenBeeldenVanScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		LOG.info("Ronde gevonden om beelden te verwijderen: {}", screeningRonde.getId());
		aantalContextOphogen(MammaIlmJobListener.KEY_BEELDEN_VERWIJDERD_AANTAL);
		berichtToBatchService.queueMammaHL7v24BerichtUitgaand(screeningRonde, MammaHL7v24ORMBerichtStatus.DELETE);
		screeningRonde.getUitnodigingen()
			.forEach(uitnodiging -> uitnodiging.getAfspraken()
				.forEach(afspraak ->
				{
					if (afspraak.getOnderzoek() != null && afspraak.getOnderzoek().getMammografie() != null)
					{
						var mammografie = afspraak.getOnderzoek().getMammografie();
						mammografie.setIlmStatus(MammaMammografieIlmStatus.TE_VERWIJDEREN);
						mammografie.setIlmStatusDatum(currentDateSupplier.getDate());
						hibernateService.saveOrUpdate(mammografie);
					}
				}));
	}

}
