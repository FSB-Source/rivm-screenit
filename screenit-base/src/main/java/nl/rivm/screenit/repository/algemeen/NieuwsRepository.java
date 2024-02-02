package nl.rivm.screenit.repository.algemeen;

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

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.nieuws.GebruikerNieuwsItem;
import nl.rivm.screenit.model.nieuws.GebruikerNieuwsItem_;
import nl.rivm.screenit.model.nieuws.NieuwsItem;
import nl.rivm.screenit.model.nieuws.NieuwsItem_;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.domain.Specification;

public interface NieuwsRepository extends BaseJpaRepository<NieuwsItem>
{

	static Specification<NieuwsItem> baseSpecification()
	{
		return ((r, q, cb) -> cb.isNotNull(r));
	}

	static Specification<NieuwsItem> publicerenTot(Date date)
	{
		return ((r, q, cb) -> cb.or(cb.isNull(r.get(NieuwsItem_.publicerenTot)), cb.greaterThan(r.get(NieuwsItem_.publicerenTot), date)));
	}

	static Specification<NieuwsItem> publicerenVanaf(Date date)
	{
		return ((r, q, cb) -> cb.or(cb.isNull(r.get(NieuwsItem_.publicerenVanaf)), cb.lessThanOrEqualTo(r.get(NieuwsItem_.publicerenVanaf), date)));
	}

	static Specification<NieuwsItem> isOngelezen(Gebruiker gebruiker)
	{
		return ((nieuwsItem, q, cb) ->
		{
			Join<GebruikerNieuwsItem, NieuwsItem> join = nieuwsItem.join(NieuwsItem_.GEBRUIKER_NIEUWS_ITEMS, JoinType.LEFT);
			Join<GebruikerNieuwsItem, NieuwsItem> gebruikerNieuwsItem = join.on(cb.equal(join.get(GebruikerNieuwsItem_.GEBRUIKER), gebruiker));

			return cb.or(gebruikerNieuwsItem.isNull(),
				cb.and(cb.equal(gebruikerNieuwsItem.get(GebruikerNieuwsItem_.GEBRUIKER), gebruiker),
					cb.or(cb.isNull(gebruikerNieuwsItem.get(GebruikerNieuwsItem_.NIET_ZICHTBAAR_VANAF)),
						cb.or(
							cb.and(cb.isNull(nieuwsItem.get(NieuwsItem_.GEWIJZIGD)),
								cb.gt(nieuwsItem.get(NieuwsItem_.GEMAAKT), gebruikerNieuwsItem.get(GebruikerNieuwsItem_.NIET_ZICHTBAAR_VANAF))),
							cb.gt(nieuwsItem.get(NieuwsItem_.GEWIJZIGD), gebruikerNieuwsItem.get(GebruikerNieuwsItem_.NIET_ZICHTBAAR_VANAF))))));
		});
	}

}
