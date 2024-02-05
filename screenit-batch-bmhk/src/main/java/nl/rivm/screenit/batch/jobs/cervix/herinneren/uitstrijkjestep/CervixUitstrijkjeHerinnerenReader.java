package nl.rivm.screenit.batch.jobs.cervix.herinneren.uitstrijkjestep;

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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.cervix.herinneren.allsteps.CervixHerinnerenReader;
import nl.rivm.screenit.model.OrganisatieParameterKey;

import org.hibernate.Criteria;
import org.springframework.stereotype.Component;

@Component
public class CervixUitstrijkjeHerinnerenReader extends CervixHerinnerenReader
{

	private static final int CERVIX_UITSTRIJKJE_HERINNEREN_READER_FETCH_SIZE = 50;

	public CervixUitstrijkjeHerinnerenReader()
	{
		super(CERVIX_UITSTRIJKJE_HERINNEREN_READER_FETCH_SIZE, PreferenceKey.CERVIX_HERINNERINGS_PERIODE_NON_RESPONDER,
			OrganisatieParameterKey.CERVIX_MAX_AANTAL_HERINNERINGEN_UITSTRIJKJE,
			"mergedBrieven.printDatum");
	}

	@Override
	protected void voegStepSpecifiekeCriteriaToe(Criteria crit)
	{
		voegAliasEnCriteriaToeMonstertypeUitstrijkje(crit);
	}

}
