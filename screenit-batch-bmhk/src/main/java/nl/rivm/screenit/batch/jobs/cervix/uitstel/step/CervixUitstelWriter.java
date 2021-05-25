package nl.rivm.screenit.batch.jobs.cervix.uitstel.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.cervix.uitstel.CervixUitstelConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;

public class CervixUitstelWriter extends BaseWriter<CervixUitstel>
{

	@Autowired
	private CervixFactory factory;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private BaseBriefService briefService;

	@Override
	protected void write(CervixUitstel uitstel) throws Exception
	{
		CervixDossier dossier = uitstel.getScreeningRonde().getDossier();
		CervixScreeningRonde ronde = uitstel.getScreeningRonde();
		ronde.setUitstel(null);
		hibernateService.saveOrUpdate(ronde);

		if (!(ronde.getUitstrijkjeCytologieUitslag() != null && ronde.getUitnodigingVervolgonderzoek() == null))
		{
			CervixUitnodiging uitnodiging = null;
			CervixCISHistorie cisHistorie = dossier.getCisHistorie();
			if (cisHistorie != null && uitstel.equals(cisHistorie.getUitstel()))
			{

				cisHistorie.setUitstel(null);
				hibernateService.saveOrUpdate(cisHistorie);
				uitnodiging = factory.maakUitnodiging(ronde, BriefType.CERVIX_UITNODIGING, true, true);
			}
			else
			{
				CervixBrief brief;
				boolean herinneren = true;

				CervixUitnodiging laatsteUitnodiging = clientService.getLaatstVerstuurdeUitnodiging(ronde, false);
				if (laatsteUitnodiging != null)
				{
					CervixBrief laatsteUitnodigingBrief = laatsteUitnodiging.getBrief();
					brief = briefService.maakCervixBrief(ronde, laatsteUitnodigingBrief.getBriefType());
					brief.setHerdruk(laatsteUitnodigingBrief);
					herinneren = laatsteUitnodiging.getHerinneren();
				}
				else
				{
					brief = briefService.maakCervixBrief(ronde, BriefType.CERVIX_UITNODIGING);
				}

				hibernateService.saveOrUpdate(brief);

				uitnodiging = factory.maakUitnodiging(ronde, brief, herinneren, true);
			}

			uitnodiging.setUitgesteld(true);
			hibernateService.saveOrUpdate(uitnodiging);
		}

		hibernateService.delete(uitstel);

		aantalContextOphogen(CervixUitstelConstants.UITSTEL_AANTAL_KEY);
	}
}
