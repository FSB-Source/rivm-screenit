package nl.rivm.screenit.main.service.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.model.enums.GebeurtenisBron;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixMonster;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CervixOmissieGebeurtenisFactory
{
	private static final Logger LOG = LoggerFactory.getLogger(CervixOmissieGebeurtenisFactory.class);

	public static void createOmissieGebeurtenisIndienVanToepassing(CervixBrief brief, ScreeningRondeGebeurtenissen rondeDossier)
	{
		if (brief.getOmissieType() == null)
		{
			return;
		}

		ScreeningRondeGebeurtenis gebeurtenis = new ScreeningRondeGebeurtenis();

		CervixMonster monster = brief.getMonster();
		if (monster != null)
		{
			gebeurtenis.setUitnodiging(monster.getUitnodiging());
			gebeurtenis.setExtraOmschrijving("Monster-id: ", monster.getMonsterId());
		}
		gebeurtenis.setClickable(false);
		gebeurtenis.setDatum(new Date(brief.getCreatieDatum().getTime() - 1000)); 

		switch (brief.getOmissieType())
		{
		case WACHT_OP_UITSTRIJKJE_ONTVANGEN:
			gebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_OMISSIE_WACHT_OP_UITSTRIJKJE_ONTVANGEN);
			break;
		case WACHT_OP_HPV_UITSLAG:
			gebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_OMISSIE_WACHT_OP_HPV_UITSLAG);
			break;
		case WACHT_OP_GECONTROLEERD:
			gebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_OMISSIE_WACHT_OP_GECONTROLEERD);
			break;
		case WACHT_OP_GECONTROLEERD_VOOR_CYTOLOGIE:
			gebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_OMISSIE_WACHT_OP_GECONTROLEERD_VOOR_CYTOLOGIE);
			break;
		case WACHT_OP_CYTOLOGIE_UITSLAG:
			gebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_OMISSIE_WACHT_OP_CYTOLOGIE_UITSLAG);
			break;
		default:
			LOG.error("Onbekend BMHK gebeurtenis omissie type: " + brief.getOmissieType());
		}
		gebeurtenis.setBron(GebeurtenisBron.AUTOMATISCH);

		rondeDossier.addGebeurtenis(gebeurtenis);
	}
}
