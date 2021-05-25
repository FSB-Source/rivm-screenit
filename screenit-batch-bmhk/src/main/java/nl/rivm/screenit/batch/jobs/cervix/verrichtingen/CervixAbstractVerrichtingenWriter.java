package nl.rivm.screenit.batch.jobs.cervix.verrichtingen;

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

import java.util.Date;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.service.cervix.CervixVerrichtingFactory;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.springframework.beans.factory.annotation.Autowired;

public abstract class CervixAbstractVerrichtingenWriter<T extends CervixMonster> extends BaseWriter<T>
{

	@Autowired
	protected CervixVerrichtingFactory verrichtingenFactory;

	protected void bepaalCytologieVerrichtingen(CervixUitstrijkje uitstrijkje)
	{
		CervixScreeningRonde ronde = uitstrijkje.getOntvangstScreeningRonde();
		Date cytologieVerrichtingsDatum = uitstrijkje.getCytologieVerslag().getVerslagContent().getVerrichting().getEindeVerrichting();

		if (ronde.getInVervolgonderzoekDatum() == null || uitstrijkje.getOntvangstdatum().before(ronde.getInVervolgonderzoekDatum()))
		{
			if (HibernateHelper.deproxy(ronde.getMonsterHpvUitslag()) instanceof CervixUitstrijkje)
			{

				verrichtingenFactory.maakLabVerrichting(uitstrijkje, CervixTariefType.LAB_CYTOLOGIE_NA_HPV_UITSTRIJKJE, cytologieVerrichtingsDatum);
				aantalContextOphogen(CervixBepalenVerrichtingenConstants.VERRICHTINGEN_LAB_CYTOLOGIE_UITSTRIJKJE_AANTAL_KEY);
			}
			else
			{

				verrichtingenFactory.maakLabVerrichting(uitstrijkje, CervixTariefType.LAB_CYTOLOGIE_NA_HPV_ZAS, cytologieVerrichtingsDatum);
				aantalContextOphogen(CervixBepalenVerrichtingenConstants.VERRICHTINGEN_LAB_CYTOLOGIE_ZAS_AANTAL_KEY);
			}
		}
		else
		{

			verrichtingenFactory.maakLabVerrichting(uitstrijkje, CervixTariefType.LAB_CYTOLOGIE_VERVOLGUITSTRIJKJE, cytologieVerrichtingsDatum);
			aantalContextOphogen(CervixBepalenVerrichtingenConstants.VERRICHTINGEN_LAB_CYTOLOGIE_VERVOLGUITSTRIJKJE_AANTAL_KEY);
		}
	}
}
