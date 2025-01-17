package nl.rivm.screenit.batch.jobs.cervix.aftergba.retourzendingstep;

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

import javax.persistence.criteria.From;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.aftergba.retourzendingstep.BaseRetourzendingReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;

import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Component
public class RetourzendingReader extends BaseRetourzendingReader
{
	@Override
	protected String getRetourzendingMarker()
	{
		return Constants.CERVIX_RETOURZENDING_MARKER;
	}

	@Override
	protected From<?, ? extends InpakbareUitnodiging<?>> getUitnodigingJoin(Root<Client> r)
	{
		var dossierJoin = join(r, Client_.cervixDossier);
		var screeningRondeJoin = join(dossierJoin, CervixDossier_.screeningRondes);
		return join(screeningRondeJoin, CervixScreeningRonde_.uitnodigingen);
	}
}
