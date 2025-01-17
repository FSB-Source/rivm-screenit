package nl.rivm.screenit.batch.jobs.cervix.huisartsberichten.versturenstep;

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

import java.util.List;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.HuisartsBericht_;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht_;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.specification.algemeen.GemeenteSpecification;
import nl.rivm.screenit.specification.cervix.CervixBriefSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.cervix.huisartsberichten.CervixHuisartsberichtenJobConfiguration.CERVIX_HUISARTSENBERICHTEN_JOB_READERS_FETCH_SIZE;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsBerichtSpecification.heeftGeenUitstrijkje;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsBerichtSpecification.heeftHuisartsLocatie;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsBerichtSpecification.heeftStatusIn;

@Component
public class CervixHuisartsberichtVersturenReader extends BaseSpecificationScrollableResultReader<CervixHuisartsBericht>
{

	public CervixHuisartsberichtVersturenReader()
	{
		super.setFetchSize(CERVIX_HUISARTSENBERICHTEN_JOB_READERS_FETCH_SIZE);
	}

	@Override
	protected Specification<CervixHuisartsBericht> createSpecification()
	{
		return heeftStatusIn(
			List.of(CervixHuisartsBerichtStatus.AANGEMAAKT, CervixHuisartsBerichtStatus.VERSTUREN_MISLUKT, CervixHuisartsBerichtStatus.KLANTNUMMER_NIET_GEVERIFIEERD,
				CervixHuisartsBerichtStatus.OPNIEUW_VERSTUREN_MISLUKT))
			.and(heeftHuisartsLocatie())
			.and(heeftGeenUitstrijkje()

				.or(CervixBriefSpecification.heeftGeenUitnodiging().with(r -> getBriefJoin(r)))
				.or(GemeenteSpecification.heeftBmhkLaboratorium().with(r -> getGemeenteJoin(r)))
			);
	}

	private static Join<CervixUitstrijkje, CervixBrief> getBriefJoin(From<?, ? extends CervixHuisartsBericht> r)
	{
		var uitstrijkjeJoin = join(r, CervixHuisartsBericht_.uitstrijkje, JoinType.LEFT);
		return join(uitstrijkjeJoin, CervixMonster_.brief, JoinType.LEFT);
	}

	private static Join<BagAdres, Gemeente> getGemeenteJoin(From<?, ? extends CervixHuisartsBericht> r)
	{
		var clientJoin = join(r, HuisartsBericht_.client);
		var persoonJoin = join(clientJoin, Client_.persoon);
		var adresJoin = join(persoonJoin, GbaPersoon_.gbaAdres);
		return join(adresJoin, BagAdres_.gbaGemeente);
	}
}
