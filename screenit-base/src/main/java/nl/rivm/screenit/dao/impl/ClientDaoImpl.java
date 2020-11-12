package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.SQLQuery;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.type.StringType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Strings;
import com.google.common.primitives.Ints;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class ClientDaoImpl extends AbstractAutowiredDao implements ClientDao
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateClient(Client client)
	{
		this.getSession().saveOrUpdate(client.getPersoon().getGbaAdres());
		this.getSession().saveOrUpdate(client.getPersoon());
		this.getSession().saveOrUpdate(client);
	}

	@Override
	public Client getClientByBsn(String bsn)
	{
		Criteria crit = this.getSession().createCriteria(Client.class);
		crit.createAlias("persoon", "persoon");
		crit.add(Restrictions.eq("persoon.bsn", bsn));
		return (Client) crit.uniqueResult();
	}

	@Override
	public Client getClientZonderBezwaar(String bsn)
	{
		Criteria crit = this.getSession().createCriteria(Client.class);
		crit.createAlias("persoon", "persoon");
		crit.add(Restrictions.eq("persoon.bsn", bsn));
		crit.add(Restrictions.ne("gbaStatus", GbaStatus.BEZWAAR));
		return (Client) crit.uniqueResult();
	}

	@Override
	public Client getLaatstAfgevoerdeClient(String bsn)
	{
		Criteria crit = this.getSession().createCriteria(Client.class);
		Criteria persoonCriteria = crit.createCriteria("persoon", "persoon");
		persoonCriteria.add(Restrictions.sqlRestriction("right({alias}.bsn, 9) = ?", bsn, StringType.INSTANCE));
		crit.add(Restrictions.eq("gbaStatus", GbaStatus.AFGEVOERD));
		crit.addOrder(Order.desc("persoon.bsn"));

		List<Client> clienten = crit.list();

		return clienten.stream().findFirst().orElse(null);
	}

	@Override
	public Client getClientByBsnFromNg01Bericht(String bsn, String anummer)
	{
		Criteria crit = this.getSession().createCriteria(Client.class);
		crit.add(Restrictions.sizeNe("gbaMutaties", 0));

		DetachedCriteria subquery = DetachedCriteria.forClass(Client.class);
		subquery.createAlias("persoon", "persoon");
		subquery.add(Restrictions.like("persoon.bsn", bsn, MatchMode.END));
		subquery.add(Restrictions.eq("gbaStatus", GbaStatus.AFGEVOERD));
		if (StringUtils.isNotBlank(anummer))
		{
			subquery.add(Restrictions.eq("persoon.anummer", anummer));
		}
		subquery.addOrder(Order.desc("persoon.bsn"));
		subquery.setProjection(Projections.id());
		List<Client> personen = subquery.getExecutableCriteria(getSession()).list();

		if (personen.isEmpty())
		{
			return null;
		}
		crit.add(Restrictions.eq("id", personen.get(0)));

		return (Client) crit.uniqueResult();
	}

	@Override
	public Client getClientByANummer(String anummer)
	{
		Criteria crit = this.getSession().createCriteria(Client.class);
		crit.createAlias("persoon", "persoon");
		crit.add(Restrictions.eq("persoon.anummer", anummer));
		return (Client) crit.uniqueResult();
	}

	@Override
	public List<Client> zoekClienten(Client zoekObject)
	{
		Criteria crit = this.getSession().createCriteria(Client.class);
		crit.createAlias("persoon", "persoon");
		crit.createAlias("persoon.gbaAdres", "gbaAdres");

		crit.add(Restrictions.ne("gbaStatus", GbaStatus.AFGEVOERD));
		crit.add(Restrictions.ne("gbaStatus", GbaStatus.BEZWAAR));

		GbaPersoon persoon = zoekObject.getPersoon();
		BagAdres gbaAdres = persoon.getGbaAdres();
		String bsn = persoon.getBsn();
		String postcode = gbaAdres.getPostcode();

		boolean criteriaToegevoegd = false;
		if (StringUtils.isNotBlank(bsn))
		{
			crit.add(Restrictions.eq("persoon.bsn", bsn));
			criteriaToegevoegd = true;
		}

		if (StringUtils.isNotBlank(postcode))
		{
			crit.add(Restrictions.eq("gbaAdres.postcode", postcode));
			crit.add(Restrictions.eq("gbaAdres.huisnummer", gbaAdres.getHuisnummer()));
			criteriaToegevoegd = true;
		}
		if (!criteriaToegevoegd)
		{
			throw new IllegalAccessError(
				"Er zijn geen bsn en/of postcode opgegeven. Zonder de info kan niet gezocht worden naar clienten.");
		}

		crit.addOrder(Order.asc("persoon.achternaam"));

		List<Client> clienten = crit.list();

		clienten.removeIf(client -> !DateUtil.geboortedatumEquals(client.getPersoon(), persoon));

		return clienten;
	}

	@Override
	public List<Client> getClientenMetTitel(String titelCode)
	{
		Criteria crit = this.getSession().createCriteria(Client.class);
		crit.createAlias("persoon", "persoon");

		crit.add(Restrictions.eq("persoon.titelCode", titelCode));

		return crit.list();
	}

	@Override
	public List<Client> getClientenOpAdres(BagAdres adres, Integer minimaleLeeftijd, Integer maximaleLeeftijd, int uitnodigingsInterval)
	{
		Criteria crit = this.getSession().createCriteria(Client.class);
		crit.createAlias("persoon", "persoon");
		crit.createAlias("persoon.gbaAdres", "adres");

		ScreenitRestrictions.addLeeftijdsgrensRestrictions(crit, minimaleLeeftijd, maximaleLeeftijd, uitnodigingsInterval, currentDateSupplier.getLocalDate());

		crit.add(Restrictions.isNull("persoon.overlijdensdatum"));

		if (adres.getHuisnummer() != null)
		{
			crit.add(Restrictions.eq("adres.huisnummer", adres.getHuisnummer()));
		}
		else
		{
			crit.add(Restrictions.isNull("adres.huisnummer"));
		}

		if (!Strings.isNullOrEmpty(adres.getHuisletter()))
		{
			crit.add(Restrictions.eq("adres.huisletter", adres.getHuisletter()));
		}
		else
		{
			crit.add(Restrictions.isNull("adres.huisletter"));
		}

		if (adres.getHuisnummerToevoeging() != null)
		{
			crit.add(Restrictions.eq("adres.huisnummerToevoeging", adres.getHuisnummerToevoeging()));
		}
		else
		{
			crit.add(Restrictions.isNull("adres.huisnummerToevoeging"));
		}

		if (adres.getHuisnummerAanduiding() != null)
		{
			crit.add(Restrictions.eq("adres.huisnummerAanduiding", adres.getHuisnummerAanduiding()));
		}
		else
		{
			crit.add(Restrictions.isNull("adres.huisnummerAanduiding"));
		}

		if (adres.getPostcode() != null)
		{
			crit.add(Restrictions.eq("adres.postcode", adres.getPostcode()));
		}
		else
		{
			crit.add(Restrictions.isNull("adres.postcode"));
		}

		if (!Strings.isNullOrEmpty(adres.getLocatieBeschrijving()))
		{
			crit.add(Restrictions.eq("adres.locatieBeschrijving", adres.getLocatieBeschrijving()));
		}
		else
		{
			crit.add(Restrictions.isNull("adres.locatieBeschrijving"));
		}

		return crit.list();
	}

	private BaseCriteria<ClientContact> getClientContactCriteria(Client client)
	{
		BaseCriteria<ClientContact> criteria = new BaseCriteria<>(ClientContact.class);
		criteria.add(Restrictions.eq("client", client));
		return criteria;
	}

	@Override
	public List<ClientContact> getClientContacten(Client client, long first, long count, String sortProperty, boolean ascending)
	{
		BaseCriteria<ClientContact> criteria = getClientContactCriteria(client);
		if (sortProperty.startsWith("medewerker"))
		{
			criteria.alias("instellingGebruiker");
			criteria.alias("instellingGebruiker.medewerker", "medewerker");
		}
		return criteria.list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count), sortProperty, ascending));
	}

	@Override
	public List<ClientContact> getClientContacten(Client client)
	{
		BaseCriteria<ClientContact> criteria = getClientContactCriteria(client);
		return criteria.list(getSession());
	}

	@Override
	public boolean heeftClientIntakeConclusieMetBezwaar(String bsn)
	{
		Criteria crit = this.getSession().createCriteria(Client.class);
		crit.createAlias("persoon", "persoon");
		crit.add(Restrictions.eq("persoon.bsn", bsn));
		crit.createAlias("colonDossier", "colonDossier");
		crit.createAlias("colonDossier.laatsteScreeningRonde", "laatsteScreeningRonde");
		crit.createAlias("laatsteScreeningRonde.laatsteAfspraak", "laatsteAfspraak");
		crit.add(Restrictions.eq("laatsteAfspraak.bezwaar", Boolean.TRUE));

		return crit.uniqueResult() != null;
	}

	@Override
	public boolean heeftDossierMetRondeOfAfmelding(Client client)
	{
		SQLQuery query = getSession().createSQLQuery("SELECT 1"
			+ " FROM gedeeld.pat_patient pa"
			+ "  LEFT JOIN colon.colon_dossier co ON pa.colon_dossier = co.id"
			+ "  LEFT JOIN cervix.dossier ce ON pa.cervix_dossier = ce.id"
			+ "  LEFT JOIN mamma.dossier ma ON pa.mamma_dossier = ma.id"
			+ " WHERE pa.id = :clientId"
			+ "  AND (co.laatste_screening_ronde IS NOT NULL OR co.laatste_afmelding IS NOT NULL"
			+ "   OR ce.laatste_screening_ronde IS NOT NULL OR ce.laatste_afmelding IS NOT NULL"
			+ "   OR ma.laatste_screening_ronde IS NOT NULL OR ma.laatste_afmelding IS NOT NULL);");
		query.setParameter("clientId", client.getId());
		return query.uniqueResult() != null;
	}

	@Override
	public int countUsedColonHandtekeningBrief(UploadDocument handtekeningDocument, String handtekeningProperty)
	{
		Criteria crit = getSession().createCriteria(ColonAfmelding.class);
		crit.add(Restrictions.eq(handtekeningProperty, handtekeningDocument));
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).intValue();
	}

	@Override
	public List<ColonUitnodiging> getAllColonUitnodigingenVanClientInPeriode(Client client, Date begin, Date eind)
	{

		Criteria crit = getSession().createCriteria(ColonUitnodiging.class);
		crit.add(Restrictions.and(
			Restrictions.gt("creatieDatum", begin), 
			Restrictions.lt("creatieDatum", eind)
		));
		return crit.list();
	}
}
