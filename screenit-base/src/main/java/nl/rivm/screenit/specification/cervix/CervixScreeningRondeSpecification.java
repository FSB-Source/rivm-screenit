package nl.rivm.screenit.specification.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.ScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixBrief_;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixUitnodiging_;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixScreeningRondeSpecification
{
	public static Specification<CervixScreeningRonde> heeftPersoon(String bsn)
	{
		return (r, q, cb) ->
		{
			var dossier = SpecificationUtil.join(r, CervixScreeningRonde_.dossier);
			var client = SpecificationUtil.join(dossier, CervixDossier_.client);
			var persoon = SpecificationUtil.join(client, Client_.persoon);

			return cb.equal(persoon.get(GbaPersoon_.bsn), bsn);
		};
	}

	public static Specification<CervixScreeningRonde> heeftMonsterInDossier(CervixMonster monster)
	{
		return (r, q, cb) ->
		{
			var dossier = SpecificationUtil.join(r, CervixScreeningRonde_.dossier);
			var rondesVanDossier = dossier.join(CervixDossier_.screeningRondes);
			var uitnodiging = rondesVanDossier.join(CervixScreeningRonde_.uitnodigingen);
			var monsterJoin = SpecificationUtil.join(uitnodiging, CervixUitnodiging_.monster);
			return cb.equal(monsterJoin, monster);
		};
	}

	public static Specification<CervixScreeningRonde> heeftCreatieDatumNa(Date peilDatum)
	{
		return (r, q, cb) -> cb.lessThan(r.get(ScreeningRonde_.creatieDatum), peilDatum);
	}

	public static Specification<CervixScreeningRonde> getZASsenHandmatigAangevraagdSpecification(CervixScreeningRonde ronde, boolean aangevraagdDoorClient)
	{
		return ((r, cq, cb) ->
		{
			var uitnodigingJoin = r.join(CervixScreeningRonde_.uitnodigingen);
			var uitnodiging = uitnodigingJoin.on(
				cb.equal(uitnodigingJoin.get(CervixUitnodiging_.screeningRonde), r));

			var briefJoin = uitnodiging.join(CervixUitnodiging_.brief);
			var brief = briefJoin.on(cb.equal(briefJoin.get(CervixBrief_.uitnodiging), uitnodiging.get(CervixUitnodiging_.brief)));

			var juisteRonde = cb.equal(r, ronde);
			var monsterTypeZAS = cb.equal(uitnodiging.get(CervixUitnodiging_.monsterType), CervixMonsterType.ZAS);
			var zasAangevraagdDoorClient = cb.equal(uitnodiging.get(CervixUitnodiging_.zasAangevraagdDoorClient), aangevraagdDoorClient);
			var filterOpBriefType = cb.not(brief.get(Brief_.briefType).in(BriefType.getCervixZasUitnodigingNietDirectHerzendbaarBrieven()));

			return cb.and(juisteRonde, monsterTypeZAS, zasAangevraagdDoorClient, filterOpBriefType);
		});
	}

}
