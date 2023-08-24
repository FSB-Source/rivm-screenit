package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.VerslagDao;
import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.cervix.verslag.CervixVerslag;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlTnummerPathologieVerslag;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.mamma.verslag.MammaVerslag;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Property;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.primitives.Ints;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class VerslagDaoImpl extends AbstractAutowiredDao implements VerslagDao
{

	@Override
	public MdlVerslag getMdlVerslagMetTNummer(PaVerslagContent verslagContent)
	{
		Criteria criteria = getSession().createCriteria(MdlVerslag.class);
		criteria.createAlias("verslagContent", "verslagContent");
		criteria.createAlias("verslagContent.coloscopieMedischeObservatie", "coloscopieMedischeObservatie");
		criteria.createAlias("coloscopieMedischeObservatie.tnummerPathologieVerslag", "tnummerPathologieVerslag");

		criteria.add(Restrictions.eq("screeningRonde", verslagContent.getVerslag().getScreeningRonde()));
		criteria.add(Restrictions.eq("tnummerPathologieVerslag.tnummerPathologieVerslag", verslagContent.getPathologieMedischeObservatie().getTnummerLaboratorium()));

		List<?> list = criteria.list();
		if (list == null || list.isEmpty())
		{
			return null;
		}
		return (MdlVerslag) list.get(0);
	}

	@Override
	public List<PaVerslag> getPaVerslagMetTNummer(MdlVerslagContent verslagContent)
	{
		Criteria criteria = getSession().createCriteria(PaVerslag.class);
		criteria.createAlias("verslagContent", "verslagContent");
		criteria.createAlias("verslagContent.pathologieMedischeObservatie", "pathologieMedischeObservatie");

		criteria.add(Restrictions.eq("screeningRonde", verslagContent.getVerslag().getScreeningRonde()));
		List<MdlTnummerPathologieVerslag> tnummerPathologieVerslag = verslagContent.getColoscopieMedischeObservatie().getTnummerPathologieVerslag();
		List<String> tnummers = new ArrayList<>();
		for (MdlTnummerPathologieVerslag tnummer : tnummerPathologieVerslag)
		{
			if (StringUtils.isNotBlank(tnummer.getTnummerPathologieVerslag()))
			{
				tnummers.add(tnummer.getTnummerPathologieVerslag());
			}
		}
		if (!tnummers.isEmpty())
		{
			criteria.add(Restrictions.in("pathologieMedischeObservatie.tnummerLaboratorium", tnummers));
			List<?> list = criteria.list();
			return (List<PaVerslag>) list;
		}

		return new ArrayList<>();
	}

	@Override
	public DSValue getDsValue(String code, String codeSystem, String valueSetName)
	{
		return getDsValue(code, codeSystem, valueSetName, false);
	}

	@Override
	public DSValue getDsValue(String code, String codeSystem, String valueSetName, boolean ignoreCase)
	{
		Criteria criteria = getSession().createCriteria(DSValue.class);

		criteria.add(ignoreCase ? Restrictions.eq("code", code).ignoreCase() : Restrictions.eq("code", code));
		criteria.add(Restrictions.eq("codeSystem", codeSystem));
		criteria.add(Restrictions.eq("valueSetName", valueSetName));

		List<?> list = criteria.list();
		if (list == null || list.isEmpty())
		{
			return null;
		}
		return (DSValue) list.get(0);
	}

	@Override
	public List<MdlVerslag> getAlleMdlVerslagenVanClient(Client client)
	{
		Criteria criteria = getSession().createCriteria(MdlVerslag.class);
		criteria.createAlias("screeningRonde", "screeningronde");

		criteria.add(Restrictions.eq("screeningronde.dossier", client.getColonDossier()));
		criteria.add(Restrictions.eq("status", VerslagStatus.AFGEROND));

		criteria.addOrder(Order.desc("datumOnderzoek"));

		return criteria.list();
	}

	@Override
	public List<OntvangenCdaBericht> searchBerichten(BerichtZoekFilter filter, long first, long count, String property, boolean ascending)
	{
		BaseCriteria<OntvangenCdaBericht> criteria = createCriteria(filter);
		return criteria.list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count), property, ascending));
	}

	private BaseCriteria<OntvangenCdaBericht> createCriteria(BerichtZoekFilter filter)
	{
		BaseCriteria<OntvangenCdaBericht> crit = new BaseCriteria<>(OntvangenCdaBericht.class);

		boolean mdl = Boolean.TRUE.equals(filter.getMldBerichten());
		boolean pa = Boolean.TRUE.equals(filter.getPaLabBerichten());
		boolean cyto = Boolean.TRUE.equals(filter.getCytologieBerichten());
		boolean followUp = Boolean.TRUE.equals(filter.getFollowUpBerichten());
		boolean colon = mdl || pa;
		boolean cervix = cyto;
		boolean mamma = followUp;
		boolean geenBerichtType = !(mdl || pa || cyto || followUp);

		if (geenBerichtType)
		{
			crit.add(Restrictions.isNull("berichtType"));
		}
		else
		{
			DetachedCriteria ontvangenCdaBerichtenColon = DetachedCriteria.forClass(ColonVerslag.class);
			ontvangenCdaBerichtenColon.setProjection(Property.forName("ontvangenCdaBericht"));
			DetachedCriteria ontvangenCdaBerichtenCervix = DetachedCriteria.forClass(CervixVerslag.class);
			ontvangenCdaBerichtenCervix.setProjection(Property.forName("ontvangenCdaBericht"));
			DetachedCriteria ontvangenCdaBerichtenMamma = DetachedCriteria.forClass(MammaVerslag.class);
			ontvangenCdaBerichtenCervix.setProjection(Property.forName("ontvangenCdaBericht"));

			Disjunction disjunction = Restrictions.disjunction();
			crit.add(disjunction);
			if (colon)
			{
				disjunction.add(Property.forName("id").in(ontvangenCdaBerichtenColon));
			}
			if (cervix)
			{
				disjunction.add(Property.forName("id").in(ontvangenCdaBerichtenCervix));
			}
			if (mamma)
			{
				disjunction.add(Property.forName("id").in(ontvangenCdaBerichtenMamma));
			}

			Disjunction or = Restrictions.disjunction();
			if (mdl)
			{
				or.add(Restrictions.eq("berichtType", BerichtType.MDL_VERSLAG));
			}
			if (pa)
			{
				or.add(Restrictions.eq("berichtType", BerichtType.PA_LAB_VERSLAG));
			}
			if (cervix)
			{
				or.add(Restrictions.eq("berichtType", BerichtType.CERVIX_CYTOLOGIE_VERSLAG));
			}
			if (mamma)
			{
				or.add(Restrictions.eq("berichtType", BerichtType.MAMMA_PA_FOLLOW_UP_VERSLAG));
			}
			crit.add(or);
		}
		if (StringUtils.isNotBlank(filter.getText()))
		{
			crit.add(Restrictions.or(Restrictions.like("berichtId", filter.getText(), MatchMode.ANYWHERE), Restrictions.like("setId", filter.getText(), MatchMode.ANYWHERE),
				Restrictions.like("projectVersion", filter.getText(), MatchMode.ANYWHERE)));
		}

		return crit;
	}

	@Override
	public long countBerichten(BerichtZoekFilter filter)
	{
		return createCriteria(filter).countLong(getSession(), "id");
	}

	@Override
	public List<Object> getIdsEnBerichtTypen(BerichtZoekFilter filter)
	{
		BaseCriteria<OntvangenCdaBericht> criteria = createCriteria(filter);

		return criteria.listForProjection(getSession(), new ListCriteria(-1, -1),
			Projections.projectionList().add(Projections.property("id")).add(Projections.property("berichtType")));
	}

	@Override
	public MdlVerslag getActueelsteMdlVerslag(ColonScreeningRonde screeningRonde)
	{
		Criteria criteria = getSession().createCriteria(MdlVerslag.class);
		criteria.add(Restrictions.eq("screeningRonde", screeningRonde));
		criteria.add(Restrictions.eq("status", VerslagStatus.AFGEROND));
		criteria.setMaxResults(1);
		criteria.addOrder(Order.desc("datumOnderzoek"));
		return (MdlVerslag) criteria.uniqueResult();
	}

	@Override
	public long getMdlVerslagenWithOnderzoekDatum(Verslag verslag, Date aanvangVerrichting)
	{
		Criteria criteria = getSession().createCriteria(MdlVerslag.class);
		criteria.add(Restrictions.eq("screeningRonde", verslag.getScreeningRonde()));
		if (verslag.getId() != null)
		{
			criteria.add(Restrictions.ne("id", verslag.getId()));
		}
		criteria.add(Restrictions.eq("status", VerslagStatus.AFGEROND));
		criteria.add(Restrictions.eq("datumOnderzoek", aanvangVerrichting));
		criteria.setProjection(Projections.rowCount());
		return (Long) criteria.uniqueResult();
	}

	@Override
	public <V extends Verslag> List<V> zoekVerslagen(V zoekObject, int first, int count, String property, boolean ascending)
	{
		Criteria criteria = createCriteria(zoekObject);
		if (StringUtils.isNotBlank(property))
		{
			if (ascending)
			{
				criteria.addOrder(Order.asc(property));
			}
			else
			{
				criteria.addOrder(Order.desc(property));
			}
		}
		criteria.setFirstResult(first);
		criteria.setMaxResults(count);
		return criteria.list();
	}

	private <V extends Verslag> Criteria createCriteria(V zoekObject)
	{
		Criteria criteria = getSession().createCriteria(zoekObject.getClass());
		criteria.createAlias("screeningRonde", "screeningRonde");
		criteria.createAlias("screeningRonde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.add(Restrictions.eq("client.id", zoekObject.getScreeningRonde().getDossier().getClient().getId()));
		if (zoekObject.getType() != null)
		{
			criteria.add(Restrictions.eq("type", zoekObject.getType()));
		}
		return criteria;
	}

	@Override
	public <V extends Verslag> long countVerslagen(V zoekObject)
	{
		Criteria criteria = createCriteria(zoekObject);
		criteria.setProjection(Projections.rowCount());
		return (Long) criteria.uniqueResult();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW)
	public void setBerichtenOpnieuwVerwerken(List<Long> ids)
	{
		Query<Void> query = getSession().createQuery(
			"update " + OntvangenCdaBericht.class.getName() + " set status = '" + BerichtStatus.VERWERKING.name() + "' where id in (:ids)");
		query.setParameterList("ids", ids);
		query.executeUpdate();
	}
}
