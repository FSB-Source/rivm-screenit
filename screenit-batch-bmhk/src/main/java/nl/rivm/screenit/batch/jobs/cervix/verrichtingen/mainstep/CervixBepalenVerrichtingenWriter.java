package nl.rivm.screenit.batch.jobs.cervix.verrichtingen.mainstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.Date;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.cervix.verrichtingen.CervixAbstractVerrichtingenWriter;
import nl.rivm.screenit.batch.jobs.cervix.verrichtingen.CervixBepalenVerrichtingenConstants;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;

import org.springframework.stereotype.Component;

@Component
@Slf4j
public class CervixBepalenVerrichtingenWriter extends CervixAbstractVerrichtingenWriter<CervixMonster>
{

	@Override
	protected void write(CervixMonster monster) throws Exception
	{
		try
		{
			Date hpvAnalyseDatum = null;
			if (monster.getHpvBeoordelingen().size() > 0)
			{
				hpvAnalyseDatum = monster.getHpvBeoordelingen().get(monster.getHpvBeoordelingen().size() - 1).getAutorisatieDatum();
			}
			if (CervixMonsterType.UITSTRIJKJE.equals(monster.getUitnodiging().getMonsterType()))
			{
				var uitstrijkje = (CervixUitstrijkje) monster;
				if (!CervixHuisartsBerichtStatus.HUISARTS_ONBEKEND.equals(uitstrijkje.getHuisartsBericht().getStatus()))
				{
					verrichtingenFactory.maakHuisartsVerrichting(monster, CervixTariefType.HUISARTS_UITSTRIJKJE, uitstrijkje.getOntvangstdatum(),
						uitstrijkje.getHuisartsBericht().getHuisartsLocatie());
					aantalContextOphogen(CervixBepalenVerrichtingenConstants.VERRICHTINGEN_HUISARTS_UITSTRIJKJE_AANTAL_KEY);
				}
				if (hpvAnalyseDatum != null)
				{

					verrichtingenFactory.maakLabVerrichting(monster, CervixTariefType.LAB_HPV_ANALYSE_UITSTRIJKJE, hpvAnalyseDatum);
					aantalContextOphogen(CervixBepalenVerrichtingenConstants.VERRICHTINGEN_LAB_HPV_UITSTRIJKJE_AANTAL_KEY);
				}

				if (uitstrijkje.getCytologieVerslag() != null)
				{
					bepaalCytologieVerrichtingen(uitstrijkje);
				}
			}
			else
			{
				if (hpvAnalyseDatum != null)
				{

					verrichtingenFactory.maakLabVerrichting(monster, CervixTariefType.LAB_HPV_ANALYSE_ZAS, hpvAnalyseDatum);
					aantalContextOphogen(CervixBepalenVerrichtingenConstants.VERRICHTINGEN_LAB_HPV_ZAS_AANTAL_KEY);
				}
			}
		}
		catch (Exception e)
		{
			LOG.error("Fout bij bepalen van verrichting, monsterId: " + monster.getMonsterId() + ", clientId "
				+ monster.getUitnodiging().getScreeningRonde().getDossier().getClient().getId());
			throw e;
		}

	}

}
