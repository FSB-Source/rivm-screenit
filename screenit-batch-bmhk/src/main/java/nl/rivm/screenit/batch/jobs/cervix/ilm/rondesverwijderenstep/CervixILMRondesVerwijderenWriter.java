package nl.rivm.screenit.batch.jobs.cervix.ilm.rondesverwijderenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixILMRondesVerwijderenWriter extends BaseWriter<CervixScreeningRonde>
{

	private final CervixBaseScreeningrondeService cervixBaseScreeningrondeService;

	private final HibernateService hibernateService;

	@Override
	protected void write(CervixScreeningRonde ronde) throws Exception
	{
		var dossier = ronde.getDossier();
		if (dossier == null)
		{
			return;
		}

		var cisHistorie = dossier.getCisHistorie();
		if (cisHistorie != null)
		{
			cisHistorie.setScreeningRonde(null);
			cisHistorie.getCisHistorieRegels().forEach(regel ->
			{
				hibernateService.delete(regel);
			});
			dossier.setCisHistorie(null);
			hibernateService.delete(cisHistorie);
			hibernateService.saveOrUpdate(dossier);
		}

		List<CervixUitnodiging> verplaatsteUitnodigingen = new ArrayList<>();
		for (CervixUitnodiging uitnodiging : ronde.getUitnodigingen())
		{
			if (uitnodiging.getMonster() != null)
			{
				CervixMonster monster = uitnodiging.getMonster();
				CervixScreeningRonde ontvangstRonde = monster.getOntvangstScreeningRonde();
				if (ontvangstRonde != null && !ontvangstRonde.equals(ronde))
				{
					verplaatsUitnodigingNaarRonde(uitnodiging, ronde, ontvangstRonde);
					verplaatsteUitnodigingen.add(uitnodiging);
				}
				else
				{
					Optional<CervixScreeningRonde> cytologieRonde = dossier.getScreeningRondes().stream().filter(r -> !r.equals(ronde)
							&& r.getStatusDatum().after(ronde.getStatusDatum()) && r.getUitstrijkjeCytologieUitslag() != null && r.getUitstrijkjeCytologieUitslag().equals(monster))
						.findFirst();

					if (cytologieRonde.isPresent())
					{
						verplaatsUitnodigingNaarRonde(uitnodiging, ronde, cytologieRonde.get());
						monster.setOntvangstScreeningRonde(cytologieRonde.get());
						verplaatsteUitnodigingen.add(uitnodiging);
						hibernateService.saveOrUpdate(monster);
					}
				}
			}
		}
		verplaatsteUitnodigingen.forEach(u -> ronde.getUitnodigingen().remove(u));
		hibernateService.saveOrUpdateAll(ronde);
		cervixBaseScreeningrondeService.verwijderScreeningRonde(ronde);
	}

	private void verplaatsUitnodigingNaarRonde(CervixUitnodiging uitnodiging, CervixScreeningRonde origineleRonde, CervixScreeningRonde nieuweRonde)
	{
		uitnodiging.setScreeningRonde(nieuweRonde);
		var uitnodigingBrief = uitnodiging.getBrief();
		uitnodigingBrief.setScreeningRonde(nieuweRonde);
		if (origineleRonde.getEersteUitnodiging().equals(uitnodiging))
		{
			origineleRonde.setEersteUitnodiging(null);
		}
		if (origineleRonde.getLaatsteUitnodiging().equals(uitnodiging))
		{
			origineleRonde.setLaatsteUitnodiging(null);
		}
		if (origineleRonde.getLaatsteBrief().equals(uitnodigingBrief))
		{
			origineleRonde.setLaatsteBrief(null);
		}
		origineleRonde.getBrieven().remove(uitnodigingBrief);
		nieuweRonde.getBrieven().add(uitnodigingBrief);
		hibernateService.saveOrUpdateAll(uitnodiging, uitnodigingBrief, nieuweRonde);
	}

}
