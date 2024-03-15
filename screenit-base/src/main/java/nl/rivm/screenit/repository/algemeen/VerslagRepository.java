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

import java.util.Optional;

import javax.persistence.criteria.Join;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht_;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.hibernate.SessionFactory;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.query.QueryUtils;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class VerslagRepository
{
	private final SessionFactory sessionFactory;

	public Optional<? extends Verslag> getVerslagVoorBerichtId(String berichtId, Class<? extends Verslag> verslagType)
	{
		var em = sessionFactory.getCurrentSession();
		var cb = em.getCriteriaBuilder();
		var q = cb.createQuery(verslagType);
		var r = q.from(em.getMetamodel().entity(verslagType));

		Join<?, OntvangenCdaBericht> ontvangenCdaBericht = r.join("ontvangenCdaBericht");
		q.where(cb.equal(ontvangenCdaBericht.get(OntvangenCdaBericht_.berichtId), berichtId))
			.orderBy(QueryUtils.toOrders(Sort.by(Sort.Order.asc(AbstractHibernateObject_.ID)), r, cb));

		var query = em.createQuery(q);
		query.setMaxResults(1); 

		return query.getResultList().stream().findFirst();
	}
}
