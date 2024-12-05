package nl.rivm.screenit.batch.jobs.colon.selectie.maxleeftijdpushuitnodigingstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.time.LocalDate;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.colon.selectie.AbstractUitnodigingPushReader;
import nl.rivm.screenit.batch.jobs.colon.selectie.SelectieConstants;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Gemeente_;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Slf4j
public abstract class AbstractUitnodigingPushMaxLeeftijdReader extends AbstractUitnodigingPushReader<Client>
{

	@Autowired
	protected SimplePreferenceService preferenceService;

	protected AbstractUitnodigingPushMaxLeeftijdReader(ColonUitnodigingCategorie categorie)
	{
		super(categorie);
	}

	@Override
	protected Specification<Client> createSpecification()
	{
		var geboortedatum = currentDateSupplier.getLocalDate().minusYears(preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name(), 75) + 1);

		geboortedatum = geboortedatum.minusDays(getExecutionContext().getInt(SelectieConstants.PUSH_MAX_LEEFTIJD_COUNT) - 1);
		LOG.info("Controle op geboortedatum {}", geboortedatum);

		return baseSpecifications().and(PersoonSpecification.filterGeboortedatum(DateUtil.toUtilDate(geboortedatum)).with(Client_.persoon));
	}

	@Override
	protected List<Selection<?>> createProjections(Root<Client> r, CriteriaBuilder cb)
	{
		return List.of(
			r.get(SingleTableHibernateObject_.id),
			cb.nullLiteral(Long.class),
			gemeenteJoin(r).get(AbstractHibernateObject_.id),
			join(gemeenteJoin(r), Gemeente_.screeningOrganisatie, JoinType.LEFT).get(SingleTableHibernateObject_.id));
	}

	@Override
	protected LocalDate getPeildatum()
	{
		return super.getPeildatum().minusDays(getExecutionContext().getInt(SelectieConstants.PUSH_MAX_LEEFTIJD_COUNT));
	}

	private static Join<BagAdres, Gemeente> gemeenteJoin(Root<Client> r)
	{
		var persoonJoin = join(r, Client_.persoon);
		var adresJoin = join(persoonJoin, GbaPersoon_.gbaAdres);
		return join(adresJoin, BagAdres_.gbaGemeente);
	}
}
