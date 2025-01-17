package nl.rivm.screenit.repository.algemeen;

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

import java.util.List;
import java.util.Optional;

import javax.persistence.criteria.Join;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht_;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
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

	private static final String ONTVANGEN_CDA_BERICHT_ATTR = "ontvangenCdaBericht";

	public Optional<? extends Verslag> getVerslagVoorBerichtId(String berichtId, Class<? extends Verslag> verslagType)
	{
		var em = sessionFactory.getCurrentSession();
		var cb = em.getCriteriaBuilder();
		var q = cb.createQuery(verslagType);
		var r = q.from(em.getMetamodel().entity(verslagType));

		Join<?, OntvangenCdaBericht> ontvangenCdaBericht = r.join(ONTVANGEN_CDA_BERICHT_ATTR);
		q.where(cb.equal(ontvangenCdaBericht.get(OntvangenCdaBericht_.berichtId), berichtId))
			.orderBy(QueryUtils.toOrders(Sort.by(Sort.Order.asc(AbstractHibernateObject_.ID)), r, cb));

		var query = em.createQuery(q);
		query.setMaxResults(1); 

		return Optional.ofNullable(query.uniqueResult());
	}

	public Optional<? extends Verslag> getVerslagVoorSetId(String setId, Class<? extends Verslag> verslagType)
	{
		var em = sessionFactory.getCurrentSession();
		var cb = em.getCriteriaBuilder();
		var q = cb.createQuery(verslagType);
		var r = q.from(em.getMetamodel().entity(verslagType));

		Join<?, OntvangenCdaBericht> ontvangenCdaBericht = r.join(ONTVANGEN_CDA_BERICHT_ATTR);
		q.where(cb.and(cb.equal(ontvangenCdaBericht.get(OntvangenCdaBericht_.setId), setId),
				ontvangenCdaBericht.get(OntvangenCdaBericht_.status).in(List.of(BerichtStatus.VERWERKT, BerichtStatus.VERWERKING))))
			.orderBy(QueryUtils.toOrders(Sort.by(Sort.Order.asc(ONTVANGEN_CDA_BERICHT_ATTR + "." + OntvangenCdaBericht_.VERSIE)), r, cb));

		var query = em.createQuery(q);
		query.setMaxResults(1);

		return Optional.ofNullable(query.uniqueResult());
	}
}
