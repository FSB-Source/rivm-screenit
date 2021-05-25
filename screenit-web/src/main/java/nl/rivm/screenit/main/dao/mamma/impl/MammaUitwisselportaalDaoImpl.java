package nl.rivm.screenit.main.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.dao.mamma.MammaUitwisselportaalDao;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.primitives.Ints;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaUitwisselportaalDaoImpl extends AbstractAutowiredDao implements MammaUitwisselportaalDao
{

	@Override
	public List<MammaDownloadOnderzoekenVerzoek> searchVerzoeken(MammaDownloadOnderzoekenVerzoek searchObject, long first, long count, String sortProperty, boolean asc)
	{
		BaseCriteria<MammaDownloadOnderzoekenVerzoek> criteria = createCriteria(searchObject);
		if (asc)
		{
			criteria.addOrder(Order.asc(sortProperty));
		}
		else
		{
			criteria.addOrder(Order.desc(sortProperty));
		}
		if (sortProperty.startsWith("gedownloadOp"))
		{
			criteria.addOrder(Order.desc("aangemaaktOp"));
		}

		return criteria.list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count)));
	}

	private BaseCriteria<MammaDownloadOnderzoekenVerzoek> createCriteria(MammaDownloadOnderzoekenVerzoek searchObject)
	{
		BaseCriteria<MammaDownloadOnderzoekenVerzoek> criteria = new BaseCriteria<>(MammaDownloadOnderzoekenVerzoek.class);
		if (searchObject.getAangemaaktDoor() != null)
		{
			criteria.alias("aangemaaktDoor");
			criteria.add(Restrictions.eq("aangemaaktDoor.organisatie", searchObject.getAangemaaktDoor().getOrganisatie()));
		}
		if (searchObject.getStatus() != null)
		{
			criteria.add(Restrictions.ne("status", BestandStatus.CRASH));
			criteria.add(Restrictions.ne("status", BestandStatus.VERWERKT));
		}
		criteria.add(Restrictions.ne("status", BestandStatus.VERWIJDERD));

		return criteria;
	}

	@Override
	public long countVerzoeken(MammaDownloadOnderzoekenVerzoek searchObject)
	{
		BaseCriteria<MammaDownloadOnderzoekenVerzoek> criteria = createCriteria(searchObject);
		return criteria.countLong(getSession());
	}

	@Override
	public List<MammaDownloadOnderzoekenVerzoek> getDownloadVerzoekenGedownload(MammaOnderzoek onderzoek)
	{
		BaseCriteria<MammaDownloadOnderzoekenVerzoek> criteria = new BaseCriteria<>(MammaDownloadOnderzoekenVerzoek.class);

		criteria.alias("onderzoeken", "downloadOnderzoek");
		criteria.add(Restrictions.eq("downloadOnderzoek.onderzoek", onderzoek));
		criteria.add(Restrictions.isNotNull("gedownloadOp"));
		criteria.addOrder(Order.desc("gedownloadOp"));

		return criteria.list(getSession());
	}

	@Override
	public Instelling getLaatstGedownloadDoorInstelling(MammaDossier dossier)
	{
		Criteria criteria = createLaatstGedownloadDoorInstellingCriteria(dossier);
		return (Instelling) criteria.uniqueResult();
	}

	private Criteria createLaatstGedownloadDoorInstellingCriteria(MammaDossier dossier)
	{
		Criteria criteria = getSession().createCriteria(MammaDownloadOnderzoek.class);
		criteria.createAlias("verzoek", "verzoek");
		criteria.createAlias("verzoek.aangemaaktDoor", "aangemaaktDoor");
		criteria.createAlias("aangemaaktDoor.organisatie", "organisatie");
		criteria.createAlias("onderzoek", "onderzoek");
		criteria.createAlias("onderzoek.afspraak", "afspraak");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.screeningRonde", "screeningRonde");
		criteria.createAlias("screeningRonde.dossier", "dossier");

		criteria.add(Restrictions.isNotNull("verzoek.gedownloadOp"));
		criteria.add(Restrictions.eq("dossier.id", dossier.getId()));
		criteria.add(Restrictions.eq("organisatie.actief", true));

		criteria.addOrder(Order.desc("verzoek.gedownloadOp"));
		criteria.setMaxResults(1);
		criteria.setProjection(Projections.property("aangemaaktDoor.organisatie"));

		return criteria;
	}
}
