package nl.rivm.screenit.service.mamma.impl;

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

import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.mamma.MammaBaseFollowUpDao;
import nl.rivm.screenit.dto.mamma.MammaFollowUpInstellingDto;
import nl.rivm.screenit.dto.mamma.MammaFollowUpInstellingRadiologieDto;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.enums.MammaFollowUpDoorverwezenFilterOptie;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag_;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.repository.mamma.MammaBaseFollowUpRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.UITSLAG_GUNSTIG;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.UITSLAG_ONGUNSTIG;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaBaseFollowUpSpecification.filterOnderzoekCreatieDatumAangemaaktTussen;
import static nl.rivm.screenit.specification.mamma.MammaBaseFollowUpSpecification.filterOpBeoordelingStatus;
import static nl.rivm.screenit.specification.mamma.MammaBaseFollowUpSpecification.filterOpScreeningOrganisatie;
import static nl.rivm.screenit.specification.mamma.MammaBaseFollowUpSpecification.heeftAangemaaktOpOfVoor;
import static nl.rivm.screenit.specification.mamma.MammaBaseFollowUpSpecification.heeftGeenIngevoerdDoor;
import static nl.rivm.screenit.specification.mamma.MammaBaseFollowUpSpecification.heeftLaatsteBeoordelingMetUitslag;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseFollowUpServiceImpl implements MammaBaseFollowUpService
{

	@Autowired
	private MammaBaseFollowUpDao followUpDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseFollowUpRepository followUpRepository;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Override
	public List<MammaFollowUpInstellingRadiologieDto> zoekOpenstaandeRadiologieVerslagenPerOrganisatie(ScreeningOrganisatie regio,
		MammaFollowUpDoorverwezenFilterOptie doorverwezenFilterOptie, Integer jaar)
	{
		var aangemaaktOp = DateUtil.toUtilDate(
			dateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.MAMMA_FOLLOW_UP_RADIOLOGIE_WERKLIJST_NA_DOWNLOADEN.name())));
		return followUpRepository.findWith(beoordelingStatusFilter(doorverwezenFilterOptie)
				.and(heeftLaatsteBeoordelingMetUitslag())
				.and(heeftGeenIngevoerdDoor())
				.and(heeftAangemaaktOpOfVoor(aangemaaktOp))
				.and(filterOpScreeningOrganisatie(regio))
				.and(onderZoekDatum(jaar)),
			MammaFollowUpInstellingRadiologieDto.class,
			q ->
				q.projections((cb, r) ->
					{
						var instellingJoin = join(r, MammaFollowUpRadiologieVerslag_.aangemaaktIn);
						return List.of(
							instellingJoin.get(SingleTableHibernateObject_.id),
							instellingJoin.get(Instelling_.naam),
							instellingJoin.get(Instelling_.mammaRadiologieGebeld),
							cb.count(r.get(AbstractHibernateObject_.id)),
							instellingJoin.get(Instelling_.telefoon),
							instellingJoin.get(Instelling_.telefoon2)
						);
					}
				).groupBy((cb, r) ->
				{
					var instellingJoin = join(r, MammaFollowUpRadiologieVerslag_.aangemaaktIn);
					return List.of(
						instellingJoin.get(SingleTableHibernateObject_.id),
						instellingJoin.get(Instelling_.naam),
						instellingJoin.get(Instelling_.mammaRadiologieGebeld),
						instellingJoin.get(Instelling_.telefoon),
						instellingJoin.get(Instelling_.telefoon2)
					);
				}).all());
	}

	private Specification<MammaFollowUpRadiologieVerslag> beoordelingStatusFilter(MammaFollowUpDoorverwezenFilterOptie doorverwezenFilterOptie)
	{
		switch (doorverwezenFilterOptie)
		{
		case DOORVERWEZEN:
			return filterOpBeoordelingStatus(UITSLAG_ONGUNSTIG);
		case NIET_DOORVERWEZEN:
			return filterOpBeoordelingStatus(UITSLAG_GUNSTIG);
		case ALLES:
			return filterOpBeoordelingStatus(null);
		}
		throw new IllegalStateException("Unexpected value: " + doorverwezenFilterOptie);
	}

	private Specification<MammaFollowUpRadiologieVerslag> onderZoekDatum(Integer jaar)
	{
		if (jaar == null)
		{
			return filterOnderzoekCreatieDatumAangemaaktTussen(null);
		}
		var vanaf = DateUtil.toUtilDate(LocalDate.of(jaar, 1, 1));
		var tot = DateUtil.toUtilDate(LocalDate.of(jaar + 1, 1, 1));
		var range = Range.closedOpen(vanaf, tot);

		return filterOnderzoekCreatieDatumAangemaaktTussen(range);
	}

	@Override
	public List<MammaFollowUpInstellingDto> zoekInstellingenMetOpenstaandePaVerslagen(ScreeningOrganisatie regio)
	{
		return followUpDao.zoekInstellingenMetOpenstaandePaVerslagen(regio);
	}

	@Override
	public List<MammaFollowUpRadiologieVerslag> zoekDossiersMetOpenstaandePaVerslagen(Instelling instelling, int first, int count, SortState<String> sortState)
	{
		return followUpDao.zoekDossiersMetOpenstaandePaVerslagen(instelling, first, count, sortState);
	}

	@Override
	public long countDossiersMetOpenstaandePaVerslagen(Instelling instelling)
	{
		return followUpDao.countDossiersMetOpenstaandePaVerslagen(instelling);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void refreshUpdateFollowUpConclusie(MammaDossier dossier)
	{
		Boolean heeftOpenstaandeFollowUpConclusie = followUpDao.heeftOpenstaandeFollowUpConclusie(dossier);
		if (!dossier.getUpdateFollowUpConclusie().equals(heeftOpenstaandeFollowUpConclusie))
		{
			dossier.setUpdateFollowUpConclusie(heeftOpenstaandeFollowUpConclusie);
			hibernateService.saveOrUpdate(dossier);
		}
	}

	@Override
	public LocalDate getEersteAutorisatieDatumPaVerslag(MammaScreeningRonde screeningRonde)
	{
		return getFollowUpVerslagenZonderLandelijkeMonitor(screeningRonde)
			.stream()
			.filter(v -> v.getVerslagContent() != null && v.getVerslagContent().getPathologieMedischeObservatie() != null
				&& v.getVerslagContent().getPathologieMedischeObservatie().getDatumAutorisatieUitslag() != null)
			.map(v -> v.getVerslagContent().getPathologieMedischeObservatie().getDatumAutorisatieUitslag())
			.map(DateUtil::toLocalDate)
			.min(Comparator.naturalOrder())
			.orElse(null);
	}

	@Override
	public List<MammaFollowUpVerslag> getFollowUpVerslagenZonderLandelijkeMonitor(MammaScreeningRonde screeningRonde)
	{
		return screeningRonde.getFollowUpVerslagen().stream().filter(pa -> pa.getType() != VerslagType.MAMMA_PA_FOLLOW_UP_MONITOR).collect(Collectors.toList());
	}
}
