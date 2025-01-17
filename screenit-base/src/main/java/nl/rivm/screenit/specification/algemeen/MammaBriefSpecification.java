package nl.rivm.screenit.specification.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import javax.persistence.criteria.Join;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaBrief_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.specification.ExtendedSpecification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaUitnodigingSpecification.heeftLaatsteAfspraak;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaBriefSpecification
{
	public static ExtendedSpecification<MammaBrief> heeftGeenMergedBrieven()
	{
		return (r, q, cb) -> cb.isNull(r.get(MammaBrief_.mergedBrieven));
	}

	public static ExtendedSpecification<MammaBrief> clientHeeftAfspraak()
	{
		return heeftLaatsteAfspraak().with(r -> laatsteUitnodigingJoin(r));
	}

	public static Join<MammaScreeningRonde, MammaUitnodiging> laatsteUitnodigingJoin(From<?, ? extends MammaBrief> briefRoot)
	{
		var dossierJoin = dossierJoin(briefRoot);
		var laatsteScreeningRondeJoin = join(dossierJoin, MammaDossier_.laatsteScreeningRonde);
		return join(laatsteScreeningRondeJoin, MammaScreeningRonde_.laatsteUitnodiging);
	}

	private static Join<Client, MammaDossier> dossierJoin(From<?, ? extends MammaBrief> briefRoot)
	{
		var clientJoin = join(briefRoot, ClientBrief_.client);
		return join(clientJoin, Client_.mammaDossier);
	}

}
