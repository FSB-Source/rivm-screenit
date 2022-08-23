package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.gunstig;

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

import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.MammaBeeldenVerwijderenWriter;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaBeeldenGunstigWriter extends MammaBeeldenVerwijderenWriter<MammaDossier>
{
	private final HibernateService hibernateService;

	public static final Long MINIMUM_AANTAL_GUNSTIGE_AFGESLOTEN_RONDES_MET_BEELDEN = 3L;

	@Override
	protected void write(MammaDossier mammaDossier)
	{
		var criteria = hibernateService.getHibernateSession().createCriteria(MammaScreeningRonde.class, "rondes");
		criteria.createAlias("laatsteOnderzoek", "onderzoek");
		criteria.createAlias("onderzoek.laatsteBeoordeling", "beoordeling");
		criteria.createAlias("onderzoek.mammografie", "mammografie");
		criteria.add(
			Restrictions.and(
				Restrictions.in("status", ScreeningRondeStatus.AFGEROND),
				Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.UITSLAG_GUNSTIG),
				Restrictions.eq("mammografie.ilmStatus", MammaMammografieIlmStatus.BESCHIKBAAR),
				Restrictions.eq("dossier.id", mammaDossier.getId())));
		criteria.addOrder(Order.desc("creatieDatum"));
		criteria.setFirstResult(MINIMUM_AANTAL_GUNSTIGE_AFGESLOTEN_RONDES_MET_BEELDEN.intValue());

		List<MammaScreeningRonde> rondes = criteria.list();
		for (var screeningRonde : rondes)
		{
			verwijderenBeeldenVanScreeningRonde(screeningRonde);
		}
	}
}
