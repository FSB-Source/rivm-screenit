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

import java.util.List;
import java.util.stream.Collectors;

import javax.persistence.criteria.JoinType;

import nl.rivm.screenit.factory.algemeen.BriefFactory;
import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.MergedBrievenFilter;
import nl.rivm.screenit.model.MergedBrieven_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument_;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.repository.algemeen.BezwaarBriefRepository;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.MergedBrievenSpecification;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.heeftBriefTypeIn;
import static nl.rivm.screenit.specification.algemeen.ClientBriefSpecification.heeftAfmelding;
import static nl.rivm.screenit.specification.algemeen.MergedBrievenSpecification.filterControle;
import static nl.rivm.screenit.specification.algemeen.MergedBrievenSpecification.filterGeprint;
import static nl.rivm.screenit.specification.algemeen.MergedBrievenSpecification.isVerwijderd;
import static nl.rivm.screenit.specification.algemeen.MergedBrievenSpecification.isVrijgegeven;
import static nl.rivm.screenit.util.StringUtil.propertyChain;

@Service
public class BriefServiceImpl implements BriefService
{

	@Autowired
	private BriefFactory briefFactory;

	@Autowired
	private BezwaarBriefRepository bezwaarBriefRepository;

	@Override
	public <M extends MergedBrieven<?>> List<M> getMergedBrieven(ScreeningOrganisatie screeningOrganisatie, MergedBrievenFilter<M> filter, long first, long count,
		Sort sort)
	{
		var uploadDocNaamPropertyChain = propertyChain(MergedBrieven_.MERGED_BRIEVEN, UploadDocument_.NAAM);

		var repository = briefFactory.getMergedBriefTypeRepository(filter.getMergedBrievenClass());

		var spec = getMergedBrievenSpecification(screeningOrganisatie, filter);

		if (!uploadDocNaamPropertyChain.equals(sort.stream().iterator().next().getProperty()))
		{
			sort = sort.and(Sort.by(Sort.Order.asc(uploadDocNaamPropertyChain)));
		}

		var finalSort = sort;
		return repository.findWith(spec, filter.getMergedBrievenClass(), q -> q.sortBy(finalSort, (order, r, cb) ->
		{
			join(r, MergedBrieven_.mergedBrieven, JoinType.INNER);
			join(r, MergedBrieven_.afgedruktDoor, JoinType.LEFT);
			join(r, MergedBrieven_.screeningOrganisatie, JoinType.LEFT);
			return null;
		})).fetch(entityGraph ->
		{
			entityGraph.addSubgraph(MergedBrieven_.MERGED_BRIEVEN);
			entityGraph.addSubgraph(MergedBrieven_.AFGEDRUKT_DOOR);
			entityGraph.addSubgraph(MergedBrieven_.SCREENING_ORGANISATIE);
		}).all(first, count);
	}

	@Override
	public <M extends MergedBrieven<?>> Long countMergedBrieven(ScreeningOrganisatie screeningOrganisatie, MergedBrievenFilter<M> filter)
	{
		var repository = briefFactory.getMergedBriefTypeRepository(filter.getMergedBrievenClass());
		var spec = getMergedBrievenSpecification(screeningOrganisatie, filter);
		return repository.count(spec);
	}

	@Override
	public <B extends ClientBrief<?, ?, ?>> List<B> getBrievenVanAfmelding(Afmelding afmelding, boolean heraanmelding)
	{
		var onderzoek = afmelding.getBevolkingsonderzoek();
		var clientBriefClass = switch (onderzoek)
		{
			case CERVIX -> CervixBrief.class;
			case COLON -> ColonBrief.class;
			case MAMMA -> MammaBrief.class;
		};

		var briefClass = (Class<B>) clientBriefClass;

		List<BriefType> types = heraanmelding ? List.of(
			BriefType.CERVIX_HERAANMELDING_AANVRAAG, BriefType.CERVIX_HERAANMELDING_HANDTEKENING, BriefType.CERVIX_HERAANMELDING_BEVESTIGING,
			BriefType.COLON_HERAANMELDING_AANVRAAG, BriefType.COLON_HERAANMELDING_HANDTEKENING, BriefType.COLON_HERAANMELDING_BEVESTIGING,
			BriefType.MAMMA_HERAANMELDING_BEVESTIGING
		) : List.of(
			BriefType.CERVIX_AFMELDING_AANVRAAG, BriefType.CERVIX_AFMELDING_HANDTEKENING, BriefType.CERVIX_BEVESTIGING_DEFINITIEVE_AFMELDING,
			BriefType.COLON_AFMELDING_AANVRAAG, BriefType.COLON_AFMELDING_HANDTEKENING, BriefType.COLON_AFMELDING_BEVESTIGING,
			BriefType.MAMMA_AFMELDING_AANVRAAG, BriefType.MAMMA_AFMELDING_HANDTEKENING, BriefType.MAMMA_BEVESTIGING_DEFINITIEVE_AFMELDING
		);

		var specification = heeftAfmelding(afmelding, briefClass).and(heeftBriefTypeIn(types));

		var briefTypeRepository = briefFactory.getBriefTypeRepository(briefClass);
		return briefTypeRepository.findAll(specification);
	}

	@Override
	public List<BezwaarBrief> getBrievenVanBezwaar(BezwaarMoment moment)
	{
		return bezwaarBriefRepository.findByBezwaarMoment(moment);
	}

	@Override
	public List<BezwaarBrief> getOorspronkelijkeBevestigingsbrieven(BezwaarMoment bezwaarMoment)
	{
		return getBrievenVanBezwaar(bezwaarMoment).stream()
			.filter(brief -> BriefType.CLIENT_BEZWAAR_BEVESTIGING_BRIEVEN.contains(brief.getBriefType()) && brief.getHerdruk() == null).collect(Collectors.toList());
	}

	protected <B extends MergedBrieven<?>> ExtendedSpecification<B> getMergedBrievenSpecification(
		ScreeningOrganisatie screeningOrganisatie, MergedBrievenFilter<B> filter)
	{
		return MergedBrievenSpecification.<B> filterScreeningOrganisatie(screeningOrganisatie)
			.and(filterGeprint(filter.getActief()))
			.and(filterControle(filter.getControle()))
			.and(isVerwijderd(false))
			.and(isVrijgegeven(true));
	}

}
