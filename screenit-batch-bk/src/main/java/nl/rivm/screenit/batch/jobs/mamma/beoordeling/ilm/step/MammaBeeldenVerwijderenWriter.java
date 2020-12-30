package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.MammaIlmJobListener;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaHL7BerichtType;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaBeeldenVerwijderenWriter extends BaseWriter<MammaOnderzoek>
{

	private final Logger LOG = LoggerFactory.getLogger(MammaBeeldenVerwijderenWriter.class);

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void write(MammaOnderzoek onderzoek)
	{
		MammaScreeningRonde screeningRonde = onderzoek.getAfspraak().getUitnodiging().getScreeningRonde();
		LOG.info("gevonden ronde: {}", screeningRonde.getId());
		aantalContextOphogen(MammaIlmJobListener.KEY_BEELDEN_VERWIJDERD_AANTAL);
		berichtToBatchService.queueMammaIlmHL7v24BerichtUitgaand(screeningRonde, MammaHL7v24ORMBerichtStatus.DELETE, MammaHL7BerichtType.IMS_ORM_ILM);
		screeningRonde.getUitnodigingen()
			.forEach(uitnodiging -> uitnodiging.getAfspraken()
				.forEach(afspraak -> {
					if (afspraak.getOnderzoek() != null && afspraak.getOnderzoek().getMammografie() != null)
					{
						MammaMammografie mammografie = afspraak.getOnderzoek().getMammografie();
						mammografie.setIlmStatus(MammaMammografieIlmStatus.TE_VERWIJDEREN);
						mammografie.setIlmStatusDatum(currentDateSupplier.getDate());
						hibernateService.saveOrUpdate(mammografie);
					}
				}));
	}
}
