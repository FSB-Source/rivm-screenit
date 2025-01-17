package nl.rivm.screenit.batch.jobs.cervix.uitstel.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.cervix.uitstel.CervixUitstelConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.cervix.impl.CervixBaseScreeningrondeServiceImpl;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixUitstelWriter extends BaseWriter<CervixUitstel>
{
	private final CervixFactory factory;

	private final HibernateService hibernateService;

	private final ClientService clientService;

	private final BaseBriefService briefService;

	private final ICurrentDateSupplier dateSupplier;

	private final CervixBaseScreeningrondeServiceImpl screeningrondeService;

	@Override
	protected void write(CervixUitstel uitstel) throws Exception
	{
		var dossier = uitstel.getScreeningRonde().getDossier();
		var ronde = uitstel.getScreeningRonde();
		ronde.setUitstel(null);
		hibernateService.saveOrUpdate(ronde);

		if (!(ronde.getUitstrijkjeCytologieUitslag() != null && ronde.getUitnodigingVervolgonderzoek() == null))
		{
			CervixUitnodiging uitnodiging;
			var cisHistorie = dossier.getCisHistorie();
			if (cisHistorie != null && uitstel.equals(cisHistorie.getUitstel()))
			{

				cisHistorie.setUitstel(null);
				hibernateService.saveOrUpdate(cisHistorie);
				uitnodiging = factory.maakUitnodiging(ronde, ronde.getLeeftijdcategorie().getUitnodigingsBrief(), true, true);
			}
			else
			{
				boolean herinneren = true;
				if (ronde.getLaatsteBrief() != null && ronde.getLaatsteBrief().getBriefType() == BriefType.CERVIX_VOORAANKONDIGING)
				{
					factory.updateDossierMetVolgendeRondeDatum(dossier, dateSupplier.getLocalDateTime());
				}
				CervixBrief brief;
				if (ronde.getLaatsteUitnodiging() != null && ronde.getLaatsteUitnodiging().getBrief() != null && !screeningrondeService.nieuweUitnodigingVoorClientMoetPUZijn(
					ronde))
				{
					brief = briefService.maakBvoBrief(ronde, ronde.getLaatsteUitnodiging().getBrief().getBriefType());
				}
				else
				{
					brief = briefService.maakBvoBrief(ronde, ronde.getLeeftijdcategorie().getUitnodigingsBrief());
				}
				var laatsteUitnodiging = clientService.getLaatstVerstuurdeUitnodiging(ronde, true);
				if (laatsteUitnodiging != null)
				{
					var laatsteUitnodigingBrief = laatsteUitnodiging.getBrief();
					brief.setHerdruk(laatsteUitnodigingBrief);
					herinneren = laatsteUitnodiging.getHerinneren();
					hibernateService.saveOrUpdate(brief);
				}

				uitnodiging = factory.maakUitnodigingMetVoorEnNaBmhk2023HerinnerenCheck(ronde, brief, herinneren);
			}

			uitnodiging.setUitgesteld(true);
			hibernateService.saveOrUpdate(uitnodiging);
		}

		hibernateService.delete(uitstel);

		aantalContextOphogen(CervixUitstelConstants.UITSTEL_AANTAL_KEY);
	}
}
