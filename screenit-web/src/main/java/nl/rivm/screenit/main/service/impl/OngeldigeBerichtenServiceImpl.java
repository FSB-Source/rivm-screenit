package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;

import nl.rivm.screenit.main.service.BerichtenZoekFilter;
import nl.rivm.screenit.main.service.OngeldigeBerichtenService;
import nl.rivm.screenit.main.service.VerslagService;
import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht_;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.repository.algemeen.MeldingOngeldigCdaBerichtRepository;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.algemeen.MeldingOngeldigCdaBerichtSpecification.filterOpBsn;
import static nl.rivm.screenit.specification.algemeen.MeldingOngeldigCdaBerichtSpecification.filterOpMeldingContaining;
import static nl.rivm.screenit.specification.algemeen.MeldingOngeldigCdaBerichtSpecification.filterOpScreeningOrganisatie;
import static nl.rivm.screenit.specification.algemeen.MeldingOngeldigCdaBerichtSpecification.heeftBerichtVanType;
import static nl.rivm.screenit.specification.algemeen.MeldingOngeldigCdaBerichtSpecification.isActief;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class OngeldigeBerichtenServiceImpl implements OngeldigeBerichtenService
{

	private static final Logger LOG = LoggerFactory.getLogger(OngeldigeBerichtenServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private VerslagService verslagService;

	@Autowired
	private MeldingOngeldigCdaBerichtRepository meldingOngeldigCdaBerichtRepository;

	@Override
	public List<MeldingOngeldigCdaBericht> zoekOngeldigeBerichten(BerichtZoekFilter filter, long first, long count, Sort sort)
	{
		return meldingOngeldigCdaBerichtRepository.findWith(maakOngeldigeBerichtenSpecification(filter), q -> q.sortBy(sort).all(first, count));
	}

	@Override
	public long countOngeldigeBerichten(BerichtZoekFilter filter)
	{
		return meldingOngeldigCdaBerichtRepository.count(maakOngeldigeBerichtenSpecification(filter));
	}

	private Specification<MeldingOngeldigCdaBericht> maakOngeldigeBerichtenSpecification(BerichtZoekFilter filter)
	{
		return isActief()
			.and(matchedOpBerichtTypeUitFilter(filter))
			.and(filterOpBsn(filter.getBsn()))
			.and(filterOpMeldingContaining(filter.getText()))
			.and(filterOpScreeningOrganisatie(filter.getScreeningOrganisatie()));
	}

	private Specification<MeldingOngeldigCdaBericht> matchedOpBerichtTypeUitFilter(BerichtZoekFilter filter)
	{
		var verslagTypes = new ArrayList<BerichtType>();
		if (Boolean.TRUE.equals(filter.getMdlBerichten()))
		{
			verslagTypes.add(BerichtType.MDL_VERSLAG);
		}
		if (Boolean.TRUE.equals(filter.getPaLabBerichten()))
		{
			verslagTypes.add(BerichtType.PA_LAB_VERSLAG);
		}
		if (Boolean.TRUE.equals(filter.getCytologieBerichten()))
		{
			verslagTypes.add(BerichtType.CERVIX_CYTOLOGIE_VERSLAG);
		}
		if (Boolean.TRUE.equals(filter.getFollowUpBerichten()))
		{
			verslagTypes.add(BerichtType.MAMMA_PA_FOLLOW_UP_VERSLAG);
		}
		return verslagTypes.isEmpty() ? (r, q, cb) -> cb.disjunction() : heeftBerichtVanType(verslagTypes);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void berichtOpnieuwAanbieden(MeldingOngeldigCdaBericht melding)
	{
		LOG.info("CDA bericht " + melding.getOntvangenCdaBericht().getId() + " in MeldingOngeldigCdaBericht " + melding.getId() + " opnieuw aangeboden aan batch");
		verwijderenOngeldigBericht(melding);
		verslagService.berichtOpnieuwVerwerken(melding.getOntvangenCdaBericht());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderenOngeldigBericht(MeldingOngeldigCdaBericht meldingOngeldigCdaBericht)
	{
		meldingOngeldigCdaBericht.setActief(false);
		hibernateService.saveOrUpdate(meldingOngeldigCdaBericht);
		LOG.info("MeldingOngeldigCdaBericht " + meldingOngeldigCdaBericht.getId() + " gedeactiveerd");
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void herverwerkAlleBerichten(BerichtenZoekFilter berichtFilter)
	{
		var ongeldigeBerichten = zoekOngeldigeBerichten(berichtFilter, -1, -1, Sort.by(MeldingOngeldigCdaBericht_.DATUM).descending());

		var ongeldigeBerichtenIdsPerBvo = new EnumMap<Bevolkingsonderzoek, List<Long>>(Bevolkingsonderzoek.class);
		for (var ongeldigCdaBericht : ongeldigeBerichten)
		{
			if (Boolean.TRUE.equals(ongeldigCdaBericht.getActief()) && Boolean.TRUE.equals(ongeldigCdaBericht.getHerstelbaar()))
			{
				verwijderenOngeldigBericht(ongeldigCdaBericht);
				var bevolkingsonderzoek = ongeldigCdaBericht.getOntvangenCdaBericht().getBerichtType().getBevolkingsonderzoek();
				var ongeldigeBerichtenIds = ongeldigeBerichtenIdsPerBvo.get(bevolkingsonderzoek);
				if (ongeldigeBerichtenIds == null)
				{
					ongeldigeBerichtenIds = new ArrayList<>();
					ongeldigeBerichtenIdsPerBvo.put(bevolkingsonderzoek, ongeldigeBerichtenIds);
				}
				ongeldigeBerichtenIds.add(ongeldigCdaBericht.getOntvangenCdaBericht().getId());
			}
		}

		ongeldigeBerichtenIdsPerBvo.forEach((bvo, ids) -> verslagService.berichtenOpnieuwVerwerken(ids, bvo));
	}
}
